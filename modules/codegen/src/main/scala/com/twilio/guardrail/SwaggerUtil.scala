package com.twilio.guardrail

import io.swagger.v3.oas.models._
import io.swagger.v3.oas.models.PathItem._
import io.swagger.v3.oas.models.media._
import io.swagger.v3.oas.models.parameters._
import io.swagger.v3.oas.models.responses._
import cats.{ FlatMap, Foldable }
import cats.free.Free
import cats.implicits._
import com.twilio.guardrail.terms.{ ScalaTerms, SwaggerTerms }
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.extract.{ Default, ScalaType }
import com.twilio.guardrail.generators.{ Responses, ScalaParameter }
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.shims._
import java.util.{ Map => JMap }
import scala.language.reflectiveCalls
import scala.meta._
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.protocol.terms.protocol.PropMeta
import scala.collection.JavaConverters._

object SwaggerUtil {
  sealed trait ResolvedType[L <: LA]
  case class Resolved[L <: LA](tpe: L#Type, classDep: Option[L#TermName], defaultValue: Option[L#Term]) extends ResolvedType[L]
  sealed trait LazyResolvedType[L <: LA]                                                                extends ResolvedType[L]
  case class Deferred[L <: LA](value: String)                                                           extends LazyResolvedType[L]
  case class DeferredArray[L <: LA](value: String)                                                      extends LazyResolvedType[L]
  case class DeferredMap[L <: LA](value: String)                                                        extends LazyResolvedType[L]
  object ResolvedType {
    def resolveReferences[L <: LA, F[_]](
        values: List[(String, ResolvedType[L])]
    )(implicit Sc: ScalaTerms[L, F]): Free[F, List[(String, Resolved[L])]] = {
      import Sc._
      val (lazyTypes, resolvedTypes) = Foldable[List].partitionEither(values) {
        case (clsName, x: Resolved[L])         => Right((clsName, x))
        case (clsName, x: LazyResolvedType[L]) => Left((clsName, x))
      }

      def lookupTypeName(clsName: String, tpeName: String, resolvedTypes: List[(String, Resolved[L])])(
          f: L#Type => Free[F, L#Type]
      ): Free[F, Option[(String, Resolved[L])]] =
        resolvedTypes
          .find(_._1 == tpeName)
          .map(_._2.tpe)
          .traverse(x => f(x).map(y => (clsName, Resolved[L](y, None, None))))

      FlatMap[Free[F, ?]]
        .tailRecM[(List[(String, LazyResolvedType[L])], List[(String, Resolved[L])]), List[(String, Resolved[L])]](
          (lazyTypes, resolvedTypes)
        ) {
          case (lazyTypes, resolvedTypes) =>
            if (lazyTypes.isEmpty) {
              Free.pure(Right(resolvedTypes))
            } else {
              def partitionEitherM[G[_], A, B, C](fa: List[A])(f: A => G[Either[B, C]])(implicit A: cats.Alternative[List],
                                                                                        M: cats.Monad[G]): G[(List[B], List[C])] = {
                import cats.instances.tuple._

                implicit val mb: cats.Monoid[List[B]] = A.algebra[B]
                implicit val mc: cats.Monoid[List[C]] = A.algebra[C]

                Foldable[List].foldMapM[G, A, (List[B], List[C])](fa)(
                  a =>
                    f(a).map {
                      case Right(c) => (A.empty[B], A.pure(c))
                      case Left(b)  => (A.pure(b), A.empty[C])
                  }
                )
              }

              (partitionEitherM(lazyTypes) {
                case x @ (clsName, Deferred(tpeName)) =>
                  lookupTypeName(clsName, tpeName, resolvedTypes)(Free.pure _).map(Either.fromOption(_, x))
                case x @ (clsName, DeferredArray(tpeName)) =>
                  lookupTypeName(clsName, tpeName, resolvedTypes)(liftVectorType).map(Either.fromOption(_, x))
                case x @ (clsName, DeferredMap(tpeName)) =>
                  lookupTypeName(clsName, tpeName, resolvedTypes)(liftMapType).map(Either.fromOption(_, x))
              }).map({
                case (newLazyTypes, newResolvedTypes) =>
                  Left((newLazyTypes, resolvedTypes ++ newResolvedTypes))
              })
            }
        }
    }

    def resolve[L <: LA, F[_]](
        value: ResolvedType[L],
        protocolElems: List[StrictProtocolElems[L]]
    )(implicit Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F]): Free[F, Resolved[L]] = {
      import Sc._
      import Sw._
      value match {
        case x @ Resolved(tpe, _, default) => Free.pure(x)
        case Deferred(name) =>
          resolveType(name, protocolElems)
            .flatMap {
              case RandomType(name, tpe) =>
                Free.pure(Resolved[L](tpe, None, None))
              case ClassDefinition(name, tpe, cls, _, _) =>
                widenTypeName(tpe).map(Resolved[L](_, None, None))
              case EnumDefinition(name, tpe, elems, cls, _) =>
                widenTypeName(tpe).map(Resolved[L](_, None, None))
              case ADT(_, tpe, _, _) =>
                widenTypeName(tpe).map(Resolved[L](_, None, None))
            }
        case DeferredArray(name) =>
          resolveType(name, protocolElems)
            .flatMap {
              case RandomType(name, tpe) =>
                liftVectorType(tpe).map(Resolved[L](_, None, None))
              case ClassDefinition(name, tpe, cls, _, _) =>
                widenTypeName(tpe).flatMap(liftVectorType).map(Resolved[L](_, None, None))
              case EnumDefinition(name, tpe, elems, cls, _) =>
                widenTypeName(tpe).flatMap(liftVectorType).map(Resolved[L](_, None, None))
              case ADT(_, tpe, _, _) =>
                widenTypeName(tpe).flatMap(liftVectorType).map(Resolved[L](_, None, None))
            }
        case DeferredMap(name) =>
          resolveType(name, protocolElems)
            .flatMap {
              case RandomType(name, tpe) =>
                liftMapType(tpe).map(Resolved[L](_, None, None))
              case ClassDefinition(_, tpe, _, _, _) =>
                widenTypeName(tpe).flatMap(liftMapType).map(Resolved[L](_, None, None))
              case EnumDefinition(_, tpe, _, _, _) =>
                widenTypeName(tpe).flatMap(liftMapType).map(Resolved[L](_, None, None))
              case ADT(_, tpe, _, _) =>
                widenTypeName(tpe).flatMap(liftMapType).map(Resolved[L](_, None, None))
            }
      }
    }
  }

  sealed class ModelMetaTypePartiallyApplied[L <: LA, F[_]](val dummy: Boolean = true) {
    def apply[T <: Schema[_]](model: T)(implicit Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F], F: FrameworkTerms[L, F]): Free[F, ResolvedType[L]] = {
      import Sc._
      import Sw._
      model match {
        case ref: Schema[_] if ref.getSimpleRef.isDefined =>
          for {
            ref <- getSimpleRef(ref)
          } yield Deferred[L](ref)
        case arr: ArraySchema =>
          for {
            items <- getItems(arr)
            meta  <- propMeta[L, F](items)
            res <- meta match {
              case Resolved(inner, dep, default) =>
                (liftVectorType(inner), default.traverse(x => liftVectorTerm(x))).mapN(Resolved[L](_, dep, _))
              case x: Deferred[L]      => embedArray(x)
              case x: DeferredArray[L] => embedArray(x)
              case x: DeferredMap[L]   => embedArray(x)
            }
          } yield res
        case impl: Schema[_] =>
          for {
            tpeName <- getType(impl)
            tpe     <- typeName[L, F](tpeName, Option(impl.getFormat()), ScalaType(impl))
          } yield Resolved[L](tpe, None, None)
      }
    }
  }

  def modelMetaType[L <: LA, F[_]]: ModelMetaTypePartiallyApplied[L, F] = new ModelMetaTypePartiallyApplied[L, F]()

  def extractConcreteTypes[L <: LA, F[_]](
      definitions: List[(String, Schema[_])]
  )(implicit Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F], F: FrameworkTerms[L, F]): Free[F, List[PropMeta[L]]] = {
    import Sc._
    for {
      entries <- definitions.traverse[Free[F, ?], (String, SwaggerUtil.ResolvedType[L])] {
        case (clsName, impl: Schema[_]) if (Option(impl.getProperties()).isDefined || Option(impl.getEnum()).isDefined) =>
          pureTypeName(clsName).flatMap(widenTypeName).map(x => (clsName, SwaggerUtil.Resolved[L](x, None, None): SwaggerUtil.ResolvedType[L]))
        case (clsName, comp: ComposedSchema) =>
          for {
            x <- pureTypeName(clsName).flatMap(widenTypeName)
            parentSimpleRef = comp.getAllOf.asScala.headOption.flatMap(_.getSimpleRef)
            parentTerm <- parentSimpleRef.traverse(n => pureTermName(n))
            resolvedType = SwaggerUtil.Resolved[L](x, parentTerm, None): SwaggerUtil.ResolvedType[L]
          } yield (clsName, resolvedType)
        case (clsName, definition) =>
          SwaggerUtil
            .modelMetaType[L, F](definition)
            .value
            .map(x => (clsName, x))
      }
      result <- SwaggerUtil.ResolvedType.resolveReferences[L, F](entries)
    } yield
      result.map {
        case (clsName, SwaggerUtil.Resolved(tpe, _, _)) =>
          PropMeta[L](clsName, tpe)
      }
  }

  // Standard type conversions, as documented in http://swagger.io/specification/#data-types-12
  def typeName[L <: LA, F[_]](typeName: String, format: Option[String], customType: Option[String])(implicit Sc: ScalaTerms[L, F],
                                                                                                    F: FrameworkTerms[L, F]): Free[F, L#Type] = {
    import Sc._
    import F._

    def log(fmt: Option[String], t: L#Type): L#Type = {
      fmt.foreach { fmt =>
        println(s"Warning: Deprecated behavior: Unsupported type '$fmt', falling back to $t. Please switch definitions to x-scala-type for custom types")
      }

      t
    }
    def liftCustomType(s: String): Free[F, Option[L#Type]] = {
      val tpe = s.trim
      if (tpe.nonEmpty) {
        parseType(tpe)
      } else Free.pure(Option.empty[L#Type])
    }

    customType
      .flatTraverse(liftCustomType _)
      .flatMap(
        _.fold({
          (typeName, format) match {
            case ("string", Some("date"))      => dateType()
            case ("string", Some("date-time")) => dateTimeType()
            case ("string", fmt)               => stringType(fmt).map(log(fmt, _))
            case ("number", Some("float"))     => floatType()
            case ("number", Some("double"))    => doubleType()
            case ("number", fmt)               => numberType(fmt).map(log(fmt, _))
            case ("integer", Some("int32"))    => intType()
            case ("integer", Some("int64"))    => longType()
            case ("integer", fmt)              => integerType(fmt).map(log(fmt, _))
            case ("boolean", fmt)              => booleanType(fmt).map(log(fmt, _))
            case ("array", fmt)                => arrayType(fmt).map(log(fmt, _))
            case ("file", fmt) =>
              fileType(fmt).map(log(fmt, _))
            case ("binary", _) =>
              fileType(None).map(log(None, _))
            case ("object", fmt) => objectType(fmt).map(log(fmt, _))
            case (tpe, fmt) =>
              fallbackType(tpe, fmt)
          }
        })(Free.pure(_))
      )
  }

  def propMeta[L <: LA, F[_]](property: Schema[_])(implicit Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F], F: FrameworkTerms[L, F]): Free[F, ResolvedType[L]] = {
    import F._
    import Sc._
    import Sw._
    property match {
      case p: ArraySchema =>
        for {
          items <- getItemsP(p)
          rec   <- propMeta[L, F](items)
          res <- rec match {
            case Resolved(inner, dep, default) =>
              (liftVectorType(inner), default.traverse(liftVectorTerm)).mapN(Resolved[L](_, dep, _): ResolvedType[L])
            case x: DeferredMap[L]   => embedArray(x)
            case x: DeferredArray[L] => embedArray(x)
            case x: Deferred[L]      => embedArray(x)
          }
        } yield res
      case m: MapSchema =>
        for {
          rec <- propMeta[L, F](m.getAdditionalProperties.asInstanceOf[Schema[_]]) //fixme: the definition says it could be boolean
          res <- rec match {
            case Resolved(inner, dep, _) => liftMapType(inner).map(Resolved[L](_, dep, None))
            case x: DeferredMap[L]       => embedMap(x)
            case x: DeferredArray[L]     => embedMap(x)
            case x: Deferred[L]          => embedMap(x)
          }
        } yield res
      case o: ObjectSchema =>
        objectType(None).map(Resolved[L](_, None, None)) // TODO: o.getProperties

      case ref: Schema[_] if ref.getSimpleRef.isDefined =>
        getSimpleRef(ref).map(Deferred[L])

      case b: BooleanSchema =>
        (typeName[L, F]("boolean", None, ScalaType(b)), Default(b).extract[Boolean].traverse(litBoolean(_))).mapN(Resolved[L](_, None, _))

      case s: StringSchema =>
        (typeName[L, F]("string", Option(s.getFormat()), ScalaType(s)), Default(s).extract[String].traverse(litString(_)))
          .mapN(Resolved[L](_, None, _))

      case d: DateSchema =>
        typeName[L, F]("string", Some("date"), ScalaType(d)).map(Resolved[L](_, None, None))

      case d: DateTimeSchema =>
        typeName[L, F]("string", Some("date-time"), ScalaType(d)).map(Resolved[L](_, None, None))

      case i: IntegerSchema =>
        typeName[L, F]("integer", Option(i.getFormat), ScalaType(i)).map(Resolved[L](_, None, None))

      case d: NumberSchema =>
        typeName[L, F]("number", Option(d.getFormat), ScalaType(d)).map(Resolved[L](_, None, None))

      case x =>
        fallbackPropertyTypeHandler(x).map(Resolved[L](_, None, None))
    }
  }

  /*
    Required \ Default  || Defined  || Undefined / NULL ||
    =====================================================
    TRUE                || a: T = v || a: T             ||
    FALSE / NULL        || a: T = v || a: Opt[T] = None ||
   */

  private[this] val successCodesWithEntities =
    List(200, 201, 202, 203, 206, 226)
  private[this] val successCodesWithoutEntities = List(204, 205)

  private[this] def getBestSuccessResponse(responses: JMap[String, ApiResponse]): Option[ApiResponse] =
    successCodesWithEntities
      .map(_.toString)
      .find(responses.containsKey)
      .flatMap(code => Option(responses.get(code)))
  private[this] def hasEmptySuccessType(responses: JMap[String, ApiResponse]): Boolean =
    successCodesWithoutEntities.map(_.toString).exists(responses.containsKey)

  def getResponseType[L <: LA, F[_]](httpMethod: HttpMethod, operation: Operation, ignoredType: L#Type)(
      implicit Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F],
      F: FrameworkTerms[L, F]
  ): Free[F, ResolvedType[L]] = {
    import Sc._
    if (httpMethod == HttpMethod.GET || httpMethod == HttpMethod.PUT || httpMethod == HttpMethod.POST) {
      Option(operation.getResponses)
        .flatMap { responses =>
          getBestSuccessResponse(responses)
            .flatMap[Schema[_]](resp => Option(resp.getContent.values().asScala.head.getSchema)) //fixme: use of head
            .map(propMeta[L, F](_))
            .orElse(
              if (hasEmptySuccessType(responses))
                Some(Free.pure[F, ResolvedType[L]](Resolved[L](ignoredType, None, None)))
              else None
            )
        }
        .getOrElse(Free.pure(Resolved[L](ignoredType, None, None): ResolvedType[L]))
    } else {
      Free.pure(Resolved[L](ignoredType, None, None): ResolvedType[L])
    }
  }

  def getResponseType[L <: LA](httpMethod: HttpMethod, responses: Responses[L], ignoredType: L#Type): Resolved[L] =
    if (httpMethod == HttpMethod.GET || httpMethod == HttpMethod.PUT || httpMethod == HttpMethod.POST) {
      successCodesWithEntities
        .flatMap(code => responses.value.find(_.statusCode == code))
        .flatMap(_.value.map(_._1))
        .headOption
        .fold[Resolved[L]](Resolved[L](ignoredType, None, None))(tpe => Resolved[L](tpe, None, None))
    } else {
      Resolved[L](ignoredType, None, None)
    }

  object paths {
    import atto._, Atto._

    private[this] def lookupName[T](bindingName: String,
                                    pathArgs: List[ScalaParameter[ScalaLanguage]])(f: ScalaParameter[ScalaLanguage] => Parser[T]): Parser[T] =
      pathArgs
        .find(_.argName.value == bindingName)
        .fold[Parser[T]](
          err(s"Unable to find argument ${bindingName}")
        )(param => f(param))

    private[this] val variable: Parser[String] = char('{') ~> many(notChar('}'))
      .map(_.mkString("")) <~ char('}')

    def generateUrlPathParams(path: String, pathArgs: List[ScalaParameter[ScalaLanguage]]): Target[Term] = {
      val term: Parser[Term.Apply] = variable.flatMap { binding =>
        lookupName(binding, pathArgs) { param =>
          ok(q"Formatter.addPath(${param.paramName})")
        }
      }
      val other: Parser[String]                             = many1(notChar('{')).map(_.toList.mkString)
      val pattern: Parser[List[Either[String, Term.Apply]]] = many(either(term, other).map(_.swap: Either[String, Term.Apply]))

      for {
        parts <- pattern
          .parseOnly(path)
          .either
          .fold(Target.raiseError(_), Target.pure(_))
        result = parts
          .map({
            case Left(part)  => Lit.String(part)
            case Right(term) => term
          })
          .foldLeft[Term](q"host + basePath")({ case (a, b) => q"${a} + ${b}" })
      } yield result
    }

    class Extractors[T, TN <: T](
        pathSegmentConverter: (ScalaParameter[ScalaLanguage], Option[T]) => Either[String, T],
        buildParamConstraint: ((String, String)) => T,
        joinParams: (T, T) => T,
        stringPath: String => T,
        liftBinding: Term.Name => TN,
        litRegex: (String, Term.Name, String) => T
    ) {
      // (Option[TN], T) is (Option[Binding], Segment)
      type P  = Parser[(Option[TN], T)]
      type LP = Parser[List[(Option[TN], T)]]

      val plainString      = many(noneOf("{}/?")).map(_.mkString)
      val plainNEString    = many1(noneOf("{}/?")).map(_.toList.mkString)
      val stringSegment: P = plainNEString.map(s => (None, stringPath(s)))
      def regexSegment(implicit pathArgs: List[ScalaParameter[ScalaLanguage]]): P =
        (plainString ~ variable ~ plainString).flatMap {
          case ((before, binding), after) =>
            lookupName(binding, pathArgs) {
              case param @ ScalaParameter(_, _, paramName, argName, _) =>
                val value = if (before.nonEmpty || after.nonEmpty) {
                  pathSegmentConverter(param, Some(litRegex(before.mkString, paramName, after.mkString)))
                    .fold(err, ok)
                } else {
                  pathSegmentConverter(param, None).fold(err, ok)
                }
                value.map((Some(liftBinding(paramName)), _))
            }
        }

      def segments(implicit pathArgs: List[ScalaParameter[ScalaLanguage]]): LP =
        sepBy1(choice(regexSegment(pathArgs), stringSegment), char('/'))
          .map(_.toList)

      val qsValueOnly: Parser[(String, String)] = ok("") ~ (char('=') ~> opt(many(noneOf("&")))
        .map(_.fold("")(_.mkString)))
      val staticQSArg: Parser[(String, String)] = many1(noneOf("=&"))
        .map(_.toList.mkString) ~ opt(char('=') ~> many(noneOf("&")))
        .map(_.fold("")(_.mkString))
      val staticQSTerm: Parser[T] =
        choice(staticQSArg, qsValueOnly).map(buildParamConstraint)
      val queryPart: Parser[T]                                               = sepBy1(staticQSTerm, char('&')).map(_.reduceLeft(joinParams))
      val leadingSlash: Parser[Option[Char]]                                 = opt(char('/'))
      val trailingSlash: Parser[Boolean]                                     = opt(char('/')).map(_.nonEmpty)
      val staticQS: Parser[Option[T]]                                        = (char('?') ~> queryPart.map(Option.apply _)) | char('?').map(_ => Option.empty[T]) | ok(Option.empty[T])
      val emptyPath: Parser[(List[(Option[TN], T)], (Boolean, Option[T]))]   = ok((List.empty[(Option[TN], T)], (false, None)))
      val emptyPathQS: Parser[(List[(Option[TN], T)], (Boolean, Option[T]))] = ok(List.empty[(Option[TN], T)]) ~ (ok(false) ~ staticQS)
      def pattern(implicit pathArgs: List[ScalaParameter[ScalaLanguage]]): Parser[(List[(Option[TN], T)], (Boolean, Option[T]))] =
        opt(leadingSlash) ~> ((segments ~ (trailingSlash ~ staticQS)) | emptyPathQS | emptyPath) <~ endOfInput
      def runParse(path: String, pathArgs: List[ScalaParameter[ScalaLanguage]]): Target[(List[(Option[TN], T)], (Boolean, Option[T]))] =
        pattern(pathArgs)
          .parse(path)
          .done match {
          case ParseResult.Done(input, result)         => Target.pure(result)
          case ParseResult.Fail(input, stack, message) => Target.raiseError(s"Failed to parse URL: ${message} (unparsed: ${input})")
          case ParseResult.Partial(k)                  => Target.raiseError(s"Unexpected parser state attempting to parse ${path}")
        }
    }

    object akkaExtractor
        extends Extractors[Term, Term.Name](
          pathSegmentConverter = {
            case (ScalaParameter(_, param, _, argName, argType), base) =>
              base.fold {
                argType match {
                  case t"String" => Right(q"Segment")
                  case t"Double" => Right(q"DoubleNumber")
                  case t"BigDecimal" =>
                    Right(q"Segment.map(BigDecimal.apply _)")
                  case t"Int"    => Right(q"IntNumber")
                  case t"Long"   => Right(q"LongNumber")
                  case t"BigInt" => Right(q"Segment.map(BigInt.apply _)")
                  case tpe @ Type.Name(_) =>
                    Right(q"Segment.flatMap(str => io.circe.Json.fromString(str).as[${tpe}].toOption)")
                }
              } { segment =>
                argType match {
                  case t"String" => Right(segment)
                  case t"BigDecimal" =>
                    Right(q"${segment}.map(BigDecimal.apply _)")
                  case t"BigInt" => Right(q"${segment}.map(BigInt.apply _)")
                  case tpe @ Type.Name(_) =>
                    Right(q"${segment}.flatMap(str => io.circe.Json.fromString(str).as[${tpe}].toOption)")
                }
              }
          },
          buildParamConstraint = {
            case (k, v) =>
              q" parameter(${Lit.String(k)}).require(_ == ${Lit.String(v)}) "
          },
          joinParams = { (l, r) =>
            q"${l} & ${r}"
          },
          stringPath = Lit.String(_),
          liftBinding = identity,
          litRegex = (before, _, after) =>
            q"""new scala.util.matching.Regex("^" + ${Lit
              .String(before)} + "(.*)" + ${Lit.String(after)} + ${Lit.String("$")})"""
        )

    object http4sExtractor
        extends Extractors[Pat, Term.Name](
          pathSegmentConverter = {
            case (ScalaParameter(_, param, paramName, argName, argType), base) =>
              base.fold[Either[String, Pat]] {
                argType match {
                  case t"String"     => Right(Pat.Var(paramName))
                  case t"Double"     => Right(p"DoubleVar($paramName)")
                  case t"BigDecimal" => Right(p"BigDecimalVar(${Pat.Var(paramName)})")
                  case t"Int"        => Right(p"IntVar(${Pat.Var(paramName)})")
                  case t"Long"       => Right(p"LongVar(${Pat.Var(paramName)})")
                  case t"BigInt"     => Right(p"BigIntVar(${Pat.Var(paramName)})")
                  case tpe @ Type.Name(_) =>
                    Right(p"${Term.Name(s"${tpe}Var")}(${Pat.Var(paramName)})")
                }
              } { _ =>
                //todo add support for regex segment
                Left("Unsupported feature")
              }
          },
          buildParamConstraint = {
            case (k, v) =>
              p"${Term.Name(s"${k.capitalize}Matcher")}(${Lit.String(v)})"
          },
          joinParams = { (l, r) =>
            p"${l} +& ${r}"
          },
          stringPath = Lit.String(_),
          liftBinding = identity,
          litRegex = (before, _, after) =>
            //todo add support for regex segment
            throw new UnsupportedOperationException
        )

    def generateUrlAkkaPathExtractors(path: String, pathArgs: List[ScalaParameter[ScalaLanguage]]): Target[Term] = {
      import akkaExtractor._
      for {
        partsQS <- runParse(path, pathArgs)
        (parts, (trailingSlash, queryParams)) = partsQS
        (directive, bindings) = parts
          .foldLeft[(Term, List[Term.Name])]((q"pathEnd", List.empty))({
            case ((q"pathEnd   ", bindings), (termName, b)) =>
              (q"path(${b}       )", bindings ++ termName)
            case ((q"path(${a })", bindings), (termName, c)) =>
              (q"path(${a} / ${c})", bindings ++ termName)
          })
        trailingSlashed = if (trailingSlash) {
          directive match {
            case q"path(${a })" => q"pathPrefix(${a}) & pathEndOrSingleSlash"
            case q"pathEnd"     => q"pathEndOrSingleSlash"
          }
        } else directive
        result = queryParams.fold(trailingSlashed) { qs =>
          q"${trailingSlashed} & ${qs}"
        }
      } yield result
    }

    def generateUrlHttp4sPathExtractors(path: String, pathArgs: List[ScalaParameter[ScalaLanguage]]): Target[(Pat, Option[Pat])] = {
      import http4sExtractor._
      for {
        partsQS <- runParse(path, pathArgs)
        (parts, (trailingSlash, queryParams)) = partsQS
        (directive, bindings) = parts
          .foldLeft[(Pat, List[Term.Name])]((p"${Term.Name("Root")}", List.empty))({
            case ((acc, bindings), (termName, c)) =>
              (p"$acc / ${c}", bindings ++ termName)
          })
        trailingSlashed = if (trailingSlash) {
          p"$directive / ${Lit.String("")}"
        } else directive
      } yield (trailingSlashed, queryParams)
    }
  }
}
