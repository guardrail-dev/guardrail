package com.twilio.guardrail.generators

import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.protocol.terms.{ Response, Responses }
import com.twilio.guardrail.terms.RouteMeta
import com.twilio.guardrail.StrictProtocolElems
import scala.meta._

object Http4sHelper {
  def generateResponseDefinitions(operationId: String,
                                  responses: Responses[ScalaLanguage],
                                  protocolElems: List[StrictProtocolElems[ScalaLanguage]]): List[Defn] = {
    val responseSuperType     = Type.Name(s"${operationId.capitalize}Response")
    val responseSuperTerm     = Term.Name(s"${operationId.capitalize}Response")
    val responseSuperTemplate = template"${Init(responseSuperType, Name(""), List.empty)}"

    val (terms, foldPair) = responses.value
      .map({
        case Response(statusCodeName, valueType) =>
          val responseTerm   = Term.Name(s"${statusCodeName.value}")
          val responseName   = Type.Name(s"${statusCodeName.value}")
          val foldHandleName = Term.Name(s"handle${statusCodeName.value}")

          valueType.fold[(Defn, (Term.Param, Case))]({
            val foldParameter = param"${foldHandleName}: => A"
            val foldCase      = p"case ${responseSuperTerm}.${responseTerm} => ${foldHandleName}"
            (q"case object $responseTerm extends $responseSuperTemplate", (foldParameter, foldCase))
          }) { valueType =>
            val foldParameter = param"${foldHandleName}: ${valueType} => A"
            val foldCase      = p"case x: ${responseSuperTerm}.${responseName} => ${foldHandleName}(x.value)"
            (q"case class  $responseName(value: $valueType) extends $responseSuperTemplate", (foldParameter, foldCase))
          }
      })
      .unzip

    val (foldParams, foldCases) = foldPair.unzip

    val companion = q"""
            object ${Term.Name(s"${operationId.capitalize}Response")} {
              ..$terms
            }
          """

    List[Defn](
      q"""
        sealed abstract class ${responseSuperType} {
          def fold[A](..${foldParams}): A = ${Term.Match(Term.This(Name("")), foldCases)}
        }
      """
    ) ++ List[Defn](
      companion
    )
  }

  def generateDecoder(tpe: Type, consumes: Seq[RouteMeta.ContentType]): Term =
    if (consumes.contains(RouteMeta.ApplicationJson))
      q"jsonOf[F, $tpe]"
    else
      tpe match {
        case t"String"     => q"EntityDecoder[F, String]"
        case t"Option[$_]" => q"""
                  decodeBy(MediaType.text.plain) { msg =>
                    msg.contentLength.filter(_ > 0).fold[DecodeResult[F, $tpe]](DecodeResult.success(None)){ _ =>
                      DecodeResult.success(decodeString(msg)).flatMap { str =>
                        Json.fromString(str).as[$tpe]
                          .fold(failure =>
                            DecodeResult.failure(InvalidMessageBodyFailure(s"Could not decode response: $$str", Some(failure))),
                            DecodeResult.success(_)
                          )
                      }
                    }
                  }
                """
        case _             => q"""
                  EntityDecoder[F, String].flatMapR[$tpe] { str =>
                    Json.fromString(str).as[$tpe]
                      .fold(failure =>
                        DecodeResult.failure(InvalidMessageBodyFailure(s"Could not decode response: $$str", Some(failure))),
                        DecodeResult.success(_)
                      )
                  }
                """
      }

  def generateEncoder(tpe: Type, produces: Seq[RouteMeta.ContentType]): Term =
    if (produces.contains(RouteMeta.ApplicationJson))
      q"jsonEncoderOf[F, $tpe]"
    else
      tpe match {
        case t"String"     => q"EntityEncoder[F, String]"
        case t"Option[$_]" => q"""
                  encodeBy(`Content-Type`(MediaType.text.plain, Charset.`UTF-8`)) { a: $tpe =>
                    a.fold[Entity[F]](Entity.empty)(e => EntityEncoder[F, String].toEntity(e.toString))
                  }
                """
        case _             => q"EntityEncoder[F, String].contramap[$tpe](_.toString)"
      }
}
