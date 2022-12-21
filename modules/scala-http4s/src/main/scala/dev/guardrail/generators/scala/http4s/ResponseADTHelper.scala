package dev.guardrail.generators.scala.http4s

import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.terms.{ AnyContentType, ApplicationJson, ContentType, Response, Responses }
import dev.guardrail.terms.protocol._

import scala.meta._

object ResponseADTHelper {
  def generateResponseDefinitions(
      responseClsName: String,
      responses: Responses[ScalaLanguage],
      protocolElems: List[StrictProtocolElems[ScalaLanguage]]
  ): List[Defn] = {
    val isGeneric                         = isDefinitionGeneric(responses)
    val extraTypes: List[Type]            = if (isGeneric) List(t"F") else Nil
    val extraTypeParams: List[Type.Param] = if (isGeneric) List(tparam"F[_]") else Nil

    val responseSuperType     = Type.Name(responseClsName)
    val responseSuperTerm     = Term.Name(responseClsName)
    val responseSuperTemplate = Init(if (isGeneric) Type.Apply(responseSuperType, Type.ArgClause(extraTypes)) else responseSuperType, Name(""), Seq.empty)

    val (terms, foldPair) = responses.value.map { case Response(statusCodeName, valueType, headers) =>
      val responseTerm = Term.Name(s"${statusCodeName.value}")
      val responseName = Type.Name(s"${statusCodeName.value}")

      val foldHandleName = Term.Name(s"handle${statusCodeName.value}")

      val allParams = valueType.map(tpe => (tpe, q"value")).toList ++ headers.value.map(h => (h.tpe, h.term))
      allParams match {
        case Nil if !isGeneric =>
          val foldParameter = param"${foldHandleName}: => A"
          val foldCase      = p"case ${responseSuperTerm}.${responseTerm} => ${foldHandleName}"
          (q"case object $responseTerm extends $responseSuperTemplate", (foldParameter, foldCase))
        case _ =>
          val responseCaseType = if (isGeneric) t"$responseSuperTerm.$responseName[F]" else t"$responseSuperTerm.$responseName"
          val foldParameter    = param"${foldHandleName}: (..${allParams.map(_._1)}) => A"
          val foldCase         = p"case x: $responseCaseType => ${foldHandleName}(..${allParams.map(t => q"x.${t._2}")})"
          (
            q"case class  $responseName[..$extraTypeParams](..${allParams.map(t => param"${t._2}: ${t._1}")}) extends $responseSuperTemplate",
            (foldParameter, foldCase)
          )
      }
    }.unzip

    val (foldParams, foldCases) = foldPair.unzip

    val companion = q"""
            object ${Term.Name(responseClsName)} {
              ..$terms
            }
          """

    val cls = q"""
        sealed abstract class ${responseSuperType}[..$extraTypeParams] {
          def fold[A](..${foldParams}): A = ${Term.Match(Term.This(Name("")), foldCases, Nil)}
        }
      """
    List[Defn](cls, companion)
  }

  def generateDecoder(tpe: Type, consumes: Seq[ContentType]): Term =
    if (isJsonEncoderDecoder(consumes))
      q"jsonOf[F, $tpe]"
    else
      tpe match {
        case t"Option[$_]" => q"""
                  decodeBy(MediaType.text.plain) { msg =>
                    msg.contentLength.filter(_ > 0).fold[DecodeResult[F, $tpe]](DecodeResult.successT(None)){ _ =>
                      DecodeResult.success(decodeText(msg)).flatMap { str =>
                        _root_.io.circe.Json.fromString(str).as[$tpe]
                          .fold(failure =>
                            DecodeResult.failureT(InvalidMessageBodyFailure(s"Could not decode response: $$str", Some(failure))),
                            DecodeResult.successT(_)
                          )
                      }
                    }
                  }
                """
        case _ => q"EntityDecoder[F, $tpe]"
      }

  def generateEncoder(tpe: Type, produces: Seq[ContentType]): Term =
    if (isJsonEncoderDecoder(produces))
      q"jsonEncoderOf[F, $tpe]"
    else
      tpe match {
        case t"Option[$_]" => q"""
                  encodeBy(`Content-Type`(MediaType.text.plain, Charset.`UTF-8`)) { a: $tpe =>
                    a.fold[Entity[F]](Entity.empty)(e => EntityEncoder[F, String].toEntity(e.toString))
                  }
                """
        case _ => q"EntityEncoder[F, $tpe]"
      }

  def generateEntityResponseGenerator(term: Term.Ref): Term =
    q"""
       new org.http4s.dsl.impl.EntityResponseGenerator[F,F] {
          def status = $term
          val liftG = cats.arrow.FunctionK.id
       }
     """

  def isDefinitionGeneric(responses: Responses[ScalaLanguage]): Boolean =
    responses.value.exists { response =>
      response.value.exists { case (_, tpe, _) =>
        tpe match {
          case t"fs2.Stream[F,Byte]" => true
          case _                     => false
        }
      }
    }

  private def isJsonEncoderDecoder(consumesOrProduces: Seq[ContentType]): Boolean =
    consumesOrProduces.exists(ContentType.isSubtypeOf[ApplicationJson]) ||
      consumesOrProduces.exists(ct => ct.value.startsWith("application/") && ct.value.endsWith("+json")) ||
      consumesOrProduces.isEmpty ||
      consumesOrProduces.exists(
        ContentType.isSubtypeOf[AnyContentType]
      ) // guardrial converts missing contentTypes to */* what should be converted to JSON according OpenAPI
}
