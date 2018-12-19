package com.twilio.guardrail.generators

import cats.MonadError
import cats.implicits._
import cats.data.EitherK
import com.twilio.guardrail.generators.Http4sServerGenerator.ServerTermInterp.splitOperationParts
import com.twilio.guardrail.{ StrictProtocolElems, SwaggerUtil, Target }
import io.swagger.models.{ Operation, Response => SwaggerResponse }
import com.twilio.guardrail.languages.{ LA, ScalaLanguage }
import com.twilio.guardrail.terms.{ ScalaTerm, SwaggerTerm }
import scala.collection.JavaConverters._
import scala.meta._

class Response[L <: LA](val statusCodeName: L#TermName, val statusCode: Int, val value: Option[(L#Type, Option[L#Term])])
object Response {
  def unapply[L <: LA](value: Response[L]): Option[(L#TermName, Option[L#Type])] = Some((value.statusCodeName, value.value.map(_._1)))
}
class Responses[L <: LA](val value: List[Response[L]])
object Http4sHelper {
  object HttpHelper {
    def apply(code: String): Option[(Int, String)] =
      code match {
        case "100" => Option((100, "Continue"))
        case "101" => Option((101, "SwitchingProtocols"))
        case "102" => Option((102, "Processing"))

        case "200" => Option((200, "Ok"))
        case "201" => Option((201, "Created"))
        case "202" => Option((202, "Accepted"))
        case "203" => Option((203, "NonAuthoritativeInformation"))
        case "204" => Option((204, "NoContent"))
        case "205" => Option((205, "ResetContent"))
        case "206" => Option((206, "PartialContent"))
        case "207" => Option((207, "MultiStatus"))
        case "208" => Option((208, "AlreadyReported"))
        case "226" => Option((226, "IMUsed"))

        case "300" => Option((300, "MultipleChoices"))
        case "301" => Option((301, "MovedPermanently"))
        case "302" => Option((302, "Found"))
        case "303" => Option((303, "SeeOther"))
        case "304" => Option((304, "NotModified"))
        case "305" => Option((305, "UseProxy"))
        case "307" => Option((307, "TemporaryRedirect"))
        case "308" => Option((308, "PermanentRedirect"))

        case "400" => Option((400, "BadRequest"))
        case "401" => Option((401, "Unauthorized"))
        case "402" => Option((402, "PaymentRequired"))
        case "403" => Option((403, "Forbidden"))
        case "404" => Option((404, "NotFound"))
        case "405" => Option((405, "MethodNotAllowed"))
        case "406" => Option((406, "NotAcceptable"))
        case "407" => Option((407, "ProxyAuthenticationRequired"))
        case "408" => Option((408, "RequestTimeout"))
        case "409" => Option((409, "Conflict"))
        case "410" => Option((410, "Gone"))
        case "411" => Option((411, "LengthRequired"))
        case "412" => Option((412, "PreconditionFailed"))
        case "413" => Option((413, "PayloadTooLarge"))
        case "414" => Option((414, "UriTooLong"))
        case "415" => Option((415, "UnsupportedMediaType"))
        case "416" => Option((416, "RangeNotSatisfiable"))
        case "417" => Option((417, "ExpectationFailed"))
        case "422" => Option((422, "UnprocessableEntity"))
        case "423" => Option((423, "Locked"))
        case "424" => Option((424, "FailedDependency"))
        case "426" => Option((426, "UpgradeRequired"))
        case "428" => Option((428, "PreconditionRequired"))
        case "429" => Option((429, "TooManyRequests"))
        case "431" => Option((431, "RequestHeaderFieldsTooLarge"))
        case "451" => Option((451, "UnavailableForLegalReasons"))

        case "500" => Option((500, "InternalServerError"))
        case "501" => Option((501, "NotImplemented"))
        case "502" => Option((502, "BadGateway"))
        case "503" => Option((503, "ServiceUnavailable"))
        case "504" => Option((504, "GatewayTimeout"))
        case "505" => Option((505, "HttpVersionNotSupported"))
        case "506" => Option((506, "VariantAlsoNegotiates"))
        case "507" => Option((507, "InsufficientStorage"))
        case "508" => Option((508, "LoopDetected"))
        case "510" => Option((510, "NotExtended"))
        case "511" => Option((511, "NetworkAuthenticationRequired"))
        case _     => None
      }
  }

  def getResponses(operationId: String,
                   responses: java.util.Map[String, SwaggerResponse],
                   protocolElems: List[StrictProtocolElems[ScalaLanguage]],
                   gs: GeneratorSettings[ScalaLanguage]): Target[Responses[ScalaLanguage]] =
    for {
      responses <- Target.fromOption(Option(responses).map(_.asScala), s"No responses defined for ${operationId}")

      instances <- responses
        .foldLeft[List[Target[Response[ScalaLanguage]]]](List.empty)({
          case (acc, (key, resp)) =>
            acc :+ (for {
              httpCode <- Target.fromOption(HttpHelper(key), s"Unknown HTTP type: ${key}")
              (statusCode, friendlyName) = httpCode
              statusCodeName             = Term.Name(friendlyName)
              valueType <- Option(resp.getSchema).traverse { prop =>
                for {
                  meta     <- SwaggerUtil.propMetaF[ScalaLanguage, EitherK[ScalaTerm[ScalaLanguage, ?], SwaggerTerm[ScalaLanguage, ?], ?]](prop).foldMap(ScalaGenerator.ScalaInterp.or(SwaggerGenerator.SwaggerInterp))
                  resolved <- SwaggerUtil.ResolvedType
                    .resolve[Target](meta, protocolElems)
                  SwaggerUtil.Resolved(baseType, _, baseDefaultValue) = resolved
                } yield (baseType, baseDefaultValue)
              }
            } yield new Response[ScalaLanguage](statusCodeName, statusCode, valueType))
        })
        .sequence
    } yield new Responses[ScalaLanguage](instances)

  def generateResponseDefinitions(operationId: String, operation: Operation, protocolElems: List[StrictProtocolElems[ScalaLanguage]]): Target[List[Defn]] =
    for {
      gs        <- Target.getGeneratorSettings
      responses <- Http4sHelper.getResponses(operationId, operation.getResponses, protocolElems, gs)
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
