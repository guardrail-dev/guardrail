package dev.guardrail.core

import atto._, Atto._
import cats.syntax.all._

import dev.guardrail.Target
import dev.guardrail.generators.LanguageParameter
import dev.guardrail.languages.LA

object PathExtractor {

  def lookupName[L <: LA, T](bindingName: String, pathArgs: List[LanguageParameter[L]])(
      f: LanguageParameter[L] => atto.Parser[T]
  ): atto.Parser[T] =
    pathArgs
      .find(_.argName.value == bindingName)
      .fold[atto.Parser[T]](
        err(s"Unable to find argument ${bindingName}")
      )(param => f(param))

  val variable: atto.Parser[String] = char('{') ~> many(notChar('}'))
    .map(_.mkString("")) <~ char('}')

  def generateUrlPathParams[L <: LA](
      path: Tracker[String],
      pathArgs: List[LanguageParameter[L]],
      showLiteralPathComponent: String => L#Term,
      showInterpolatedPathComponent: L#TermName => L#Term,
      initialPathTerm: L#Term,
      combinePathTerms: (L#Term, L#Term) => L#Term
  ): Target[L#Term] = {
    val term: atto.Parser[L#Term] = variable.flatMap { binding =>
      lookupName(binding, pathArgs) { param =>
        ok(showInterpolatedPathComponent(param.paramName))
      }
    }
    val other: atto.Parser[String]                         = many1(notChar('{')).map(_.toList.mkString)
    val pattern: atto.Parser[List[Either[String, L#Term]]] = many(either(term, other).map(_.swap: Either[String, L#Term]))

    for {
      parts <- path.map(pattern.parseOnly(_).either).raiseErrorIfLeft
      result = parts.unwrapTracker
        .map {
          case Left(part)  => showLiteralPathComponent(part)
          case Right(term) => term
        }
        .foldLeft[L#Term](initialPathTerm)((a, b) => combinePathTerms(a, b))
    } yield result
  }
}

class PathExtractor[L <: LA, T, TN <: T, ModelGeneratorType](
    pathSegmentConverter: (LanguageParameter[L], Option[T], ModelGeneratorType) => Either[String, T],
    buildParamConstraint: ((String, String)) => T,
    joinParams: (T, T) => T,
    stringPath: String => T,
    liftBinding: L#TermName => TN,
    litRegex: (String, L#TermName, String) => T
) {
  // (Option[TN], T) is (Option[Binding], Segment)
  type P  = Parser[(Option[TN], T)]
  type LP = Parser[List[(Option[TN], T)]]

  val plainString: Parser[String]   = many(noneOf("{}/?")).map(_.mkString)
  val plainNEString: Parser[String] = many1(noneOf("{}/?")).map(_.toList.mkString)
  val stringSegment: P              = plainNEString.map(s => (None, stringPath(s)))
  def regexSegment(implicit pathArgs: List[LanguageParameter[L]], modelGeneratorType: ModelGeneratorType): P =
    (plainString ~ PathExtractor.variable ~ plainString).flatMap { case ((before, binding), after) =>
      PathExtractor.lookupName[L, (Option[TN], T)](binding, pathArgs) { case param @ LanguageParameter(_, _, paramName, argName, _) =>
        val value = if (before.nonEmpty || after.nonEmpty) {
          pathSegmentConverter(param, Some(litRegex(before.mkString, paramName, after.mkString)), modelGeneratorType)
            .fold(err, ok)
        } else {
          pathSegmentConverter(param, None, modelGeneratorType).fold(err, ok)
        }
        value.map((Some(liftBinding(paramName)), _))
      }
    }

  private val pathSep: Parser[Char] = many1(char('/')).map(_.head)

  def segments(implicit pathArgs: List[LanguageParameter[L]], modelGeneratorType: ModelGeneratorType): LP =
    sepBy1(choice(regexSegment(pathArgs, modelGeneratorType), stringSegment), pathSep)
      .map(_.toList)

  val qsValueOnly: Parser[(String, String)] = ok("") ~ (char('=') ~> opt(many(noneOf("&")))
    .map(_.fold("")(_.mkString)))
  val staticQSArg: Parser[(String, String)] = many1(noneOf("=&"))
    .map(_.toList.mkString) ~ opt(char('=') ~> many(noneOf("&")))
    .map(_.fold("")(_.mkString))
  val staticQSTerm: Parser[T] =
    choice(staticQSArg, qsValueOnly).map(buildParamConstraint)
  val queryPart: Parser[T]               = sepBy1(staticQSTerm, char('&')).map(_.reduceLeft(joinParams))
  val leadingSlash: Parser[Option[Char]] = opt(pathSep)
  val trailingSlash: Parser[Boolean]     = opt(pathSep).map(_.nonEmpty)
  val staticQS: Parser[Option[T]]        = (char('?') ~> queryPart.map(Option.apply _)) | char('?').map(_ => Option.empty[T]) | ok(Option.empty[T])
  val emptyPath: Parser[(List[(Option[TN], T)], (Boolean, Option[T]))]   = ok((List.empty[(Option[TN], T)], (false, None)))
  val emptyPathQS: Parser[(List[(Option[TN], T)], (Boolean, Option[T]))] = ok(List.empty[(Option[TN], T)]) ~ (ok(false) ~ staticQS)
  def pattern(implicit
      pathArgs: List[LanguageParameter[L]],
      modelGeneratorType: ModelGeneratorType
  ): Parser[(List[(Option[TN], T)], (Boolean, Option[T]))] =
    opt(leadingSlash) ~> ((segments ~ (trailingSlash ~ staticQS)) | emptyPathQS | emptyPath) <~ endOfInput
  def runParse(
      path: Tracker[String],
      pathArgs: List[LanguageParameter[L]],
      modelGeneratorType: ModelGeneratorType
  ): Target[(List[(Option[TN], T)], (Boolean, Option[T]))] =
    pattern(pathArgs, modelGeneratorType)
      .parse(path.unwrapTracker)
      .done match {
      case ParseResult.Done(input, result)         => Target.pure(result)
      case ParseResult.Fail(input, stack, message) => Target.raiseUserError(s"Failed to parse URL: ${message} (unparsed: ${input}) (${path.showHistory})")
      case ParseResult.Partial(k)                  => Target.raiseUserError(s"Unexpected parser state attempting to parse ${path} (${path.showHistory})")
    }
}
