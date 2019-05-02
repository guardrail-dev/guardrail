package com.twilio.guardrail
package terms

import cats.free.Free
import com.twilio.guardrail.generators.{ ScalaParameter, ScalaParameters }
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.terms.SecurityRequirements.SecurityScopes
import com.twilio.guardrail.terms.framework.FrameworkTerms
import io.swagger.v3.oas.models.{ Operation, PathItem }
import io.swagger.v3.oas.models.PathItem.HttpMethod
import io.swagger.v3.oas.models.media.{ ArraySchema, MediaType, Schema }
import io.swagger.v3.oas.models.parameters.{ Parameter, RequestBody }
import io.swagger.v3.oas.models.responses.ApiResponse
import io.swagger.v3.oas.models.security.{ OAuthFlows, SecurityRequirement, SecurityScheme => SwSecurityScheme }
import java.net.URI
import java.util
import scala.collection.JavaConverters._

object RouteMeta {
  sealed abstract class ContentType(value: String)
  case object ApplicationJson    extends ContentType("application/json")
  case object MultipartFormData  extends ContentType("multipart/form-data")
  case object UrlencodedFormData extends ContentType("application/x-www-form-urlencoded")
  case object TextPlain          extends ContentType("text/plain")
  object ContentType {
    def unapply(value: String): Option[ContentType] = value match {
      case "application/json"                  => Some(ApplicationJson)
      case "multipart/form-data"               => Some(MultipartFormData)
      case "application/x-www-form-urlencoded" => Some(UrlencodedFormData)
      case "text/plain"                        => Some(TextPlain)
      case _                                   => None
    }
  }
}

object SecurityRequirements {
  type SecurityScopes = List[String]

  sealed trait Location
  case object Global extends Location
  case object Local  extends Location

  def apply(requirements: util.List[SecurityRequirement], optionalSchemes: List[String], location: Location): SecurityRequirements =
    SecurityRequirements(requirements.asScala.map(_.asScala.mapValues(_.asScala.toList).toMap).toList, optionalSchemes, location)
}
case class SecurityRequirements(requirements: List[Map[String, SecurityScopes]], optionalSchemes: List[String], location: SecurityRequirements.Location)

case class RouteMeta(path: String, method: HttpMethod, operation: Operation, securityRequirements: Option[SecurityRequirements]) {

  private def extractPrimitiveFromRequestBody(requestBody: RequestBody): Option[Parameter] =
    for {
      content <- Option(requestBody.getContent())
      mt      <- content.values().asScala.headOption
      tpe     <- Option(mt.getSchema.getType())
    } yield {
      val p = new Parameter

      val schema = mt.getSchema

      if (schema.getFormat == "binary") {
        schema.setType("file")
        schema.setFormat(null)
      }

      p.setIn("body")
      p.setName("body")
      p.setSchema(schema)
      p.setRequired(requestBody.getRequired)

      p.setExtensions(Option(schema.getExtensions).getOrElse(new java.util.HashMap[String, Object]()))
      p
    }

  // https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.0.md#fixed-fields-8
  // RequestBody can represent either a RequestBody object or $ref.
  // (these are both represented in the same RequestBody class)
  private def extractRefParamFromRequestBody(requestBody: RequestBody): Option[Parameter] = {
    val content = for {
      content <- Option(requestBody.getContent)
      mt      <- content.values().asScala.headOption
      ref     <- Option(mt.getSchema.get$ref())
    } yield {
      val p = new Parameter

      val schema = mt.getSchema

      if (schema.getFormat == "binary") {
        schema.setType("file")
        schema.setFormat(null)
      }

      p.setIn("body")
      p.setName("body")
      p.setSchema(schema)
      p.set$ref(ref)

      p.setRequired(requestBody.getRequired)

      p.setExtensions(Option(schema.getExtensions).getOrElse(new java.util.HashMap[String, Object]()))
      p
    }

    val ref = Option(requestBody.get$ref()).map { x =>
      val p = new Parameter

      p.setIn("body")
      p.setName("body")
      p.set$ref(x)

      p.setRequired(requestBody.getRequired)

      p.setExtensions(Option(requestBody.getExtensions).getOrElse(new java.util.HashMap[String, Object]()))

      p
    }

    content.orElse(ref)
  }

  /** Temporary hack method to adapt to open-api v3 spec */
  private def extractParamsFromRequestBody(requestBody: RequestBody): List[Parameter] =
    Option(requestBody.getContent())
      .fold(List.empty[MediaType])(x => Option(x.values()).toList.flatMap(_.asScala))
      .flatMap { mt =>
        val requiredFields = Option(mt.getSchema).flatMap(x => Option(x.getRequired)).fold(Set.empty[String])(_.asScala.toSet)

        Option(mt.getSchema.getProperties).map(_.asScala.toList).getOrElse(List.empty).map {
          case (name, schema) =>
            val p = new Parameter

            if (Option(schema.getFormat).contains("binary")) {
              schema.setType("file")
              schema.setFormat(null)
            }

            p.setName(name)
            p.setIn("formData")
            p.setSchema(schema)

            val isRequired: Boolean = if (requiredFields.nonEmpty) {
              requiredFields.contains(name)
            } else {
              Option[java.lang.Boolean](requestBody.getRequired).fold(false)(identity)
            }

            p.setRequired(isRequired)
            p.setExtensions(Option(schema.getExtensions).getOrElse(new java.util.HashMap[String, Object]()))
            p
        }
      }

  private val parameters: List[Parameter] = {
    val p = Option(operation.getParameters)
      .map(_.asScala.toList)
      .getOrElse(List.empty)

    val params =
      (Option(operation.getRequestBody).flatMap(extractRefParamFromRequestBody) ++
        p ++
        Option(operation.getRequestBody).toList.flatMap(extractParamsFromRequestBody) ++
        Option(operation.getRequestBody).flatMap(extractPrimitiveFromRequestBody)).toList
    params
  }

  def getParameters[L <: LA, F[_]](
      protocolElems: List[StrictProtocolElems[L]]
  )(implicit Fw: FrameworkTerms[L, F], Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F]): Free[F, ScalaParameters[L]] =
    ScalaParameter
      .fromParameters(protocolElems)
      .apply(parameters)
      .map({ a =>
        new ScalaParameters[L](a)
      })
}

sealed trait SecurityScheme[L <: LA] {
  def tpe: Option[L#Type]
}
case class ApiKeySecurityScheme[L <: LA](name: String, in: SwSecurityScheme.In, tpe: Option[L#Type]) extends SecurityScheme[L]
case class HttpSecurityScheme[L <: LA](authScheme: String, tpe: Option[L#Type])                      extends SecurityScheme[L]
case class OpenIdConnectSecurityScheme[L <: LA](url: URI, tpe: Option[L#Type])                       extends SecurityScheme[L]
case class OAuth2SecurityScheme[L <: LA](flows: OAuthFlows, tpe: Option[L#Type])                     extends SecurityScheme[L]

sealed trait SwaggerTerm[L <: LA, T]
case class ExtractOperations[L <: LA](paths: List[(String, PathItem)], globalSecurityRequirements: Option[SecurityRequirements])
    extends SwaggerTerm[L, List[RouteMeta]]
case class ExtractApiKeySecurityScheme[L <: LA](schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type])
    extends SwaggerTerm[L, ApiKeySecurityScheme[L]]
case class ExtractHttpSecurityScheme[L <: LA](schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type])
    extends SwaggerTerm[L, HttpSecurityScheme[L]]
case class ExtractOpenIdConnectSecurityScheme[L <: LA](schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type])
    extends SwaggerTerm[L, OpenIdConnectSecurityScheme[L]]
case class ExtractOAuth2SecurityScheme[L <: LA](schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type])
    extends SwaggerTerm[L, OAuth2SecurityScheme[L]]
case class GetClassName[L <: LA](operation: Operation, vendorPrefixes: List[String])       extends SwaggerTerm[L, List[String]]
case class GetParameterName[L <: LA](parameter: Parameter)                                 extends SwaggerTerm[L, String]
case class GetBodyParameterSchema[L <: LA](parameter: Parameter)                           extends SwaggerTerm[L, Schema[_]]
case class GetHeaderParameterType[L <: LA](parameter: Parameter)                           extends SwaggerTerm[L, String]
case class GetPathParameterType[L <: LA](parameter: Parameter)                             extends SwaggerTerm[L, String]
case class GetQueryParameterType[L <: LA](parameter: Parameter)                            extends SwaggerTerm[L, String]
case class GetCookieParameterType[L <: LA](parameter: Parameter)                           extends SwaggerTerm[L, String]
case class GetFormParameterType[L <: LA](parameter: Parameter)                             extends SwaggerTerm[L, String]
case class GetSerializableParameterType[L <: LA](parameter: Parameter)                     extends SwaggerTerm[L, String]
case class GetRefParameterRef[L <: LA](parameter: Parameter)                               extends SwaggerTerm[L, String]
case class FallbackParameterHandler[L <: LA](parameter: Parameter)                         extends SwaggerTerm[L, SwaggerUtil.ResolvedType[L]]
case class GetOperationId[L <: LA](operation: Operation)                                   extends SwaggerTerm[L, String]
case class GetResponses[L <: LA](operationId: String, operation: Operation)                extends SwaggerTerm[L, Map[String, ApiResponse]]
case class GetSimpleRef[L <: LA](ref: Schema[_])                                           extends SwaggerTerm[L, String]
case class GetItems[L <: LA](arr: ArraySchema)                                             extends SwaggerTerm[L, Schema[_]]
case class GetType[L <: LA](model: Schema[_])                                              extends SwaggerTerm[L, String]
case class FallbackPropertyTypeHandler[L <: LA](prop: Schema[_])                           extends SwaggerTerm[L, L#Type]
case class ResolveType[L <: LA](name: String, protocolElems: List[StrictProtocolElems[L]]) extends SwaggerTerm[L, StrictProtocolElems[L]]
case class FallbackResolveElems[L <: LA](lazyElems: List[LazyProtocolElems[L]])            extends SwaggerTerm[L, List[StrictProtocolElems[L]]]
case class LogPush[L <: LA](name: String)                                                  extends SwaggerTerm[L, Unit]
case class LogPop[L <: LA]()                                                               extends SwaggerTerm[L, Unit]
case class LogDebug[L <: LA](message: String)                                              extends SwaggerTerm[L, Unit]
case class LogInfo[L <: LA](message: String)                                               extends SwaggerTerm[L, Unit]
case class LogWarning[L <: LA](message: String)                                            extends SwaggerTerm[L, Unit]
case class LogError[L <: LA](message: String)                                              extends SwaggerTerm[L, Unit]
