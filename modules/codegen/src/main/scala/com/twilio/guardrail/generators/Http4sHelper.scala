package com.twilio.guardrail.generators

import cats.MonadError
import cats.data.{ EitherK, EitherT }
import cats.free.Free
import cats.implicits._
import cats.data.EitherK
import com.twilio.guardrail.generators.Http4sServerGenerator.ServerTermInterp.splitOperationParts
import com.twilio.guardrail.languages.{ LA, ScalaLanguage }
import com.twilio.guardrail.terms.framework.{ FrameworkTerm, FrameworkTerms }
import com.twilio.guardrail.terms.{ ScalaTerm, ScalaTerms, SwaggerTerm, SwaggerTerms }
import com.twilio.guardrail.{ StrictProtocolElems, SwaggerUtil, Target }
import io.swagger.models.{ Operation, Response => SwaggerResponse }
import scala.collection.JavaConverters._
import scala.meta._

class Response[L <: LA](val statusCodeName: L#TermName, val statusCode: Int, val value: Option[(L#Type, Option[L#Term])])
object Response {
  def unapply[L <: LA](value: Response[L]): Option[(L#TermName, Option[L#Type])] = Some((value.statusCodeName, value.value.map(_._1)))
}
class Responses[L <: LA](val value: List[Response[L]])
object Http4sHelper {
  def getResponsesF[L <: LA, F[_]](operationId: String, operation: Operation, protocolElems: List[StrictProtocolElems[L]])(
      implicit Fw: FrameworkTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, Responses[L]] = {
    import Fw._
    import Sc._
    for {
      responses <- Sw.getResponses(operationId, operation)

      instances <- responses
        .foldLeft[List[Free[F, Response[L]]]](List.empty)({
          case (acc, (key, resp)) =>
            acc :+ (for {
              httpCode <- lookupStatusCode(key)
              (statusCode, statusCodeName) = httpCode
              valueType <- Option(resp.getSchema).traverse { prop =>
                for {
                  meta     <- SwaggerUtil.propMetaF[L, F](prop)
                  resolved <- SwaggerUtil.ResolvedType.resolveF[L, F](meta, protocolElems)
                  SwaggerUtil.Resolved(baseType, _, baseDefaultValue) = resolved
                } yield (baseType, baseDefaultValue)
              }
            } yield new Response[L](statusCodeName, statusCode, valueType))
        })
        .sequence
    } yield new Responses[L](instances)
  }

  def getResponses(operationId: String,
                   operation: Operation,
                   protocolElems: List[StrictProtocolElems[ScalaLanguage]],
                   gs: GeneratorSettings[ScalaLanguage]): Target[Responses[ScalaLanguage]] = {
    type Program[T] = EitherK[ScalaTerm[ScalaLanguage, ?], EitherK[SwaggerTerm[ScalaLanguage, ?], FrameworkTerm[ScalaLanguage, ?], ?], T]
    val interp = ScalaGenerator.ScalaInterp.or(SwaggerGenerator.SwaggerInterp.or(Http4sGenerator.FrameworkInterp))
    getResponsesF[ScalaLanguage, Program](operationId, operation, protocolElems)
      .foldMap(interp)
  }

  def generateResponseDefinitions(operationId: String, operation: Operation, protocolElems: List[StrictProtocolElems[ScalaLanguage]]): Target[List[Defn]] =
    for {
      gs        <- Target.getGeneratorSettings
      responses <- Http4sHelper.getResponses(operationId, operation, protocolElems, gs)
      responseSuperType     = Type.Name(s"${operationId.capitalize}Response")
      responseSuperTemplate = template"${Init(responseSuperType, Name(""), List.empty)}"

      terms = responses.value.map {
        case Response(statusCodeName, valueType) =>
          val responseTerm = Term.Name(s"${statusCodeName.value}")
          val responseName = Type.Name(s"${statusCodeName.value}")
          valueType.fold[Defn](
            (q"case object $responseTerm extends $responseSuperTemplate")
          ) { valueType =>
            (q"case class  $responseName(value: $valueType) extends $responseSuperTemplate")
          }
      }

      companion = q"""
            object ${Term.Name(s"${operationId.capitalize}Response")} {
              ..$terms
            }
          """

    } yield
      List[Defn](
        q"sealed abstract class ${responseSuperType}"
      ) ++ List[Defn](
        companion
      )

  def generateDecoder(tpe: Type, consumes: Seq[String]): Term =
    if (consumes.contains("application/json"))
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

  def generateEncoder(tpe: Type, produces: Seq[String]): Term =
    if (produces.contains("application/json"))
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
