package com.twilio.guardrail
package terms

import cats.data.{ NonEmptyList, NonEmptyMap }
import cats.free.Free
import cats.data.State
import cats.implicits._
import cats.Order
import com.twilio.guardrail.core.{ Mappish, Tracker }
import com.twilio.guardrail.core.implicits._
import com.twilio.guardrail.generators.{ ScalaParameter, ScalaParameters }
import com.twilio.guardrail.generators.syntax._
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.terms.SecurityRequirements.SecurityScopes
import com.twilio.guardrail.terms.framework.FrameworkTerms
import io.swagger.v3.oas.models.{ Components, Operation, PathItem }
import io.swagger.v3.oas.models.PathItem.HttpMethod
import io.swagger.v3.oas.models.media.{ ArraySchema, Encoding, MediaType, Schema, StringSchema }
import io.swagger.v3.oas.models.parameters.{ Parameter, RequestBody }
import io.swagger.v3.oas.models.responses.ApiResponse
import io.swagger.v3.oas.models.security.{ OAuthFlows, SecurityRequirement, SecurityScheme => SwSecurityScheme }
import java.net.URI
import java.util.Locale
import scala.collection.JavaConverters._
import scala.collection.immutable.TreeMap

object RouteMeta {
  sealed abstract class ContentType(val value: String) {
    override val toString: String = value
  }
  case object ApplicationJson    extends ContentType("application/json")
  case object MultipartFormData  extends ContentType("multipart/form-data")
  case object UrlencodedFormData extends ContentType("application/x-www-form-urlencoded")
  case object TextPlain          extends ContentType("text/plain")
  case object OctetStream        extends ContentType("application/octet-stream")
  object ContentType {
    def unapply(value: String): Option[ContentType] = value.toLowerCase(Locale.US) match {
      case "application/json"                  => Some(ApplicationJson)
      case "multipart/form-data"               => Some(MultipartFormData)
      case "application/x-www-form-urlencoded" => Some(UrlencodedFormData)
      case "text/plain"                        => Some(TextPlain)
      case "application/octet-stream"          => Some(OctetStream)
      case _                                   => None
    }
    implicit val ContentTypeOrder = Order[String].contramap[ContentType](_.value)
  }
}

object SecurityRequirements {
  type SecurityScopes = List[String]

  sealed trait Location
  case object Global extends Location
  case object Local  extends Location

  def apply(requirements: NonEmptyList[SecurityRequirement], optionalSchemes: List[String], location: Location): Option[SecurityRequirements] = {
    implicit val strOrder = Order.fromComparable[String]
    for {
      convertedReqs <- NonEmptyList.fromList(
        requirements.toList
          .flatMap(
            req =>
              NonEmptyMap.fromMap(
                TreeMap(req.asScala.mapValues(_.asScala.toList).toSeq: _*)
              )
          )
      )
    } yield SecurityRequirements(convertedReqs, optionalSchemes, location)
  }
}
case class SecurityRequirements(
    requirements: NonEmptyList[NonEmptyMap[String, SecurityScopes]],
    optionalSchemes: List[String],
    location: SecurityRequirements.Location
)

case class RouteMeta(path: Tracker[String], method: HttpMethod, operation: Tracker[Operation], securityRequirements: Option[SecurityRequirements]) {
  override def toString(): String = s"RouteMeta(${path.unwrapTracker}, $method, ${operation.get.showNotNull} (${operation.showHistory}), $securityRequirements)"
  object MediaType {
    def unapply(value: MediaType): Option[(Option[Schema[_]], Option[Map[String, Encoding]], Option[Map[String, Object]])] = {
      val schema: Option[Schema[_]] = Option(value.getSchema)
      val encoding                  = Option(value.getEncoding()).map(_.asScala.toMap)
      val extensions                = Option(value.getExtensions()).map(_.asScala.toMap)
      Option((schema, encoding, extensions))
    }
  }

  private def extractPrimitiveFromRequestBody(
      fields: Mappish[List, String, Tracker[MediaType]],
      required: Tracker[Option[Boolean]]
  ): Option[Tracker[Parameter]] = {
    // FIXME: Just taking the head here isn't super great
    def unifyEntries: List[(String, Tracker[MediaType])] => Option[Tracker[Schema[_]]] =
      _.flatMap({
        case (contentType, mediaType) =>
          mediaType
            .refine[Option[Tracker[Schema[_]]]]({ case mt @ MediaType(None, _, _) if contentType == "text/plain" => mt })(
              x => Option(Tracker.cloneHistory(x, new StringSchema()))
            )
            .orRefine({ case mt @ MediaType(schema, _, _) => schema })(_.indexedDistribute)
            .orRefineFallback(_ => None)
      }).headOption
    for {
      schema <- unifyEntries(fields.value)
      tpe    <- schema.downField("type", _.getType()).indexedDistribute // TODO: Why is this here?
    } yield {

      val p = new Parameter

      if (schema.get.getFormat == "binary") {
        schema.get.setType("file")
        schema.get.setFormat(null)
      }

      p.setIn("body")
      p.setName("body")
      p.setSchema(schema.get)
      required.get.foreach(x => p.setRequired(x))

      schema
        .downField[Option[java.util.Map[String, Object]]]("extensions", _.getExtensions())
        .get
        .foreach(x => p.setExtensions(x))
      Tracker.cloneHistory(schema, p)
    }
  }

  // https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.0.md#fixed-fields-8
  // RequestBody can represent either a RequestBody object or $ref.
  // (these are both represented in the same RequestBody class)
  private def extractRefParamFromRequestBody(
      ref: Tracker[Option[String]],
      fields: Mappish[List, String, com.twilio.guardrail.core.Tracker[io.swagger.v3.oas.models.media.MediaType]],
      extensions: Tracker[Option[java.util.Map[String, Object]]],
      required: Tracker[Option[Boolean]]
  ): Option[Tracker[Parameter]] = {
    val content = for {
      (_, mt) <- fields.value.headOption
      schema  <- mt.downField("schema", _.getSchema()).indexedCosequence
      ref     <- schema.downField("$ref", _.get$ref()).indexedCosequence
    } yield {
      val p = new Parameter

      if (schema.get.getFormat == "binary") {
        schema.get.setType("file")
        schema.get.setFormat(null)
      }

      p.setIn("body")
      p.setName("body")
      p.setSchema(schema.get)
      p.set$ref(ref.get)

      required.get.foreach(x => p.setRequired(x))

      extensions.get.foreach(x => p.setExtensions(x))
      Tracker.cloneHistory(ref, p)
    }

    val refParam = ref.cotraverse { x =>
      val p = new Parameter

      p.setIn("body")
      p.setName("body")
      p.set$ref(x.get)

      required.get.foreach(x => p.setRequired(x))

      extensions.get.foreach(x => p.setExtensions(x))

      Tracker.cloneHistory(ref, p)
    }

    content.orElse(refParam)
  }

  /** Temporary hack method to adapt to open-api v3 spec */
  private def extractParamsFromRequestBody(
      fields: Mappish[List, String, com.twilio.guardrail.core.Tracker[io.swagger.v3.oas.models.media.MediaType]],
      required: Tracker[Option[Boolean]]
  ): List[Tracker[Parameter]] = {
    type HashCode            = Int
    type Count               = Int
    type ParameterCountState = (Count, Map[HashCode, Count])
    val contentTypes: List[RouteMeta.ContentType] = fields.value.collect({ case (RouteMeta.ContentType(ct), _) => ct })
    val ((maxCount, instances), ps) = fields.value
      .flatMap({
        case (_, mt) =>
          for {
            mtSchema <- mt.downField("schema", _.getSchema()).indexedCosequence.toList
            requiredFields = mtSchema.downField("required", _.getRequired).get.toSet
            (name, schema) <- mtSchema.downField("properties", _.getProperties()).indexedCosequence.value
          } yield {
            val p = new Parameter

            if (schema.downField("format", _.getFormat).get.contains("binary")) {
              schema.get.setType("file")
              schema.get.setFormat(null)
            }

            p.setName(name)
            p.setIn("formData")
            p.setSchema(schema.get)

            val isRequired: Boolean = if (requiredFields.nonEmpty) {
              requiredFields.contains(name)
            } else {
              required.get.getOrElse(false)
            }

            p.setRequired(isRequired)
            p.setExtensions(schema.unwrapTracker.getExtensions)

            if (schema.downField("type", _.getType()).indexedCosequence.exists(_.get == "file") && contentTypes.contains(RouteMeta.UrlencodedFormData)) {
              p.setRequired(false)
            }

            Tracker.cloneHistory(mt, p)
          }
      })
      .traverse[State[ParameterCountState, ?], Tracker[Parameter]] { p =>
        State[ParameterCountState, Tracker[Parameter]]({
          case (maxCount, instances) =>
            val updated = instances.updated(p.get.hashCode, instances.getOrElse(p.get.hashCode, 0) + 1)
            ((Math.max(maxCount, updated.values.max), updated), p)
        })
      }
      .runEmpty
      .value

    ps.distinctBy(_.get).map { p =>
      instances.get(p.hashCode).foreach { count =>
        // FIXME: Regardless of what the specification says, if a parameter does not appear across all media types, mark it as optional
        if (count != maxCount) {
          p.get.setRequired(false)
        }
      }
      p
    }
  }

  private val parameters: List[Tracker[Parameter]] = {
    operation.downField("parameters", _.getParameters()).indexedDistribute ++
      operation
        .downField("requestBody", _.getRequestBody())
        .map(_.toList)
        .flatExtract({ requestBody =>
          val content  = requestBody.downField("content", _.getContent()).indexedCosequence
          val required = requestBody.downField("required", _.getRequired())

          val refParam = extractRefParamFromRequestBody(
            requestBody.downField("$ref", _.get$ref()),
            content,
            requestBody.downField[Option[java.util.Map[String, Object]]]("extensions", _.getExtensions()),
            required
          )
          val params    = extractParamsFromRequestBody(content, required)
          val primitive = extractPrimitiveFromRequestBody(content, required)
          refParam.toList ++ params ++ primitive.toList
        })
  }

  def getParameters[L <: LA, F[_]](
      protocolElems: List[StrictProtocolElems[L]]
  )(implicit Fw: FrameworkTerms[L, F], Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F]): Free[F, ScalaParameters[L]] =
    for {
      a <- ScalaParameter.fromParameters(protocolElems).apply(parameters)
    } yield new ScalaParameters[L](a)
}

sealed trait SecurityScheme[L <: LA] {
  def tpe: Option[L#Type]
}
case class ApiKeySecurityScheme[L <: LA](name: String, in: SwSecurityScheme.In, tpe: Option[L#Type]) extends SecurityScheme[L]
case class HttpSecurityScheme[L <: LA](authScheme: String, tpe: Option[L#Type])                      extends SecurityScheme[L]
case class OpenIdConnectSecurityScheme[L <: LA](url: URI, tpe: Option[L#Type])                       extends SecurityScheme[L]
case class OAuth2SecurityScheme[L <: LA](flows: OAuthFlows, tpe: Option[L#Type])                     extends SecurityScheme[L]

sealed trait SwaggerTerm[L <: LA, T]
case class ExtractCommonRequestBodies[L <: LA](components: Option[Components]) extends SwaggerTerm[L, Map[String, RequestBody]]
case class ExtractOperations[L <: LA](
    paths: Tracker[Mappish[List, String, PathItem]],
    commonRequestBodies: Map[String, RequestBody],
    globalSecurityRequirements: Option[SecurityRequirements]
) extends SwaggerTerm[L, List[RouteMeta]]
case class ExtractApiKeySecurityScheme[L <: LA](schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type])
    extends SwaggerTerm[L, ApiKeySecurityScheme[L]]
case class ExtractHttpSecurityScheme[L <: LA](schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type])
    extends SwaggerTerm[L, HttpSecurityScheme[L]]
case class ExtractOpenIdConnectSecurityScheme[L <: LA](schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type])
    extends SwaggerTerm[L, OpenIdConnectSecurityScheme[L]]
case class ExtractOAuth2SecurityScheme[L <: LA](schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type])
    extends SwaggerTerm[L, OAuth2SecurityScheme[L]]
case class GetClassName[L <: LA](operation: Tracker[Operation], vendorPrefixes: List[String]) extends SwaggerTerm[L, List[String]]
case class GetParameterName[L <: LA](parameter: Parameter)                                    extends SwaggerTerm[L, String]
case class GetBodyParameterSchema[L <: LA](parameter: Tracker[Parameter])                     extends SwaggerTerm[L, Tracker[Schema[_]]]
case class GetHeaderParameterType[L <: LA](parameter: Tracker[Parameter])                     extends SwaggerTerm[L, Tracker[String]]
case class GetPathParameterType[L <: LA](parameter: Tracker[Parameter])                       extends SwaggerTerm[L, Tracker[String]]
case class GetQueryParameterType[L <: LA](parameter: Tracker[Parameter])                      extends SwaggerTerm[L, Tracker[String]]
case class GetCookieParameterType[L <: LA](parameter: Tracker[Parameter])                     extends SwaggerTerm[L, Tracker[String]]
case class GetFormParameterType[L <: LA](parameter: Tracker[Parameter])                       extends SwaggerTerm[L, Tracker[String]]
case class GetSerializableParameterType[L <: LA](parameter: Tracker[Parameter])               extends SwaggerTerm[L, Tracker[String]]
case class GetRefParameterRef[L <: LA](parameter: Tracker[Parameter])                         extends SwaggerTerm[L, Tracker[String]]
case class FallbackParameterHandler[L <: LA](parameter: Tracker[Parameter])                   extends SwaggerTerm[L, SwaggerUtil.ResolvedType[L]]
case class GetOperationId[L <: LA](operation: Tracker[Operation])                             extends SwaggerTerm[L, String]
case class GetResponses[L <: LA](operationId: String, operation: Tracker[Operation])          extends SwaggerTerm[L, NonEmptyList[(String, Tracker[ApiResponse])]]
case class GetSimpleRef[L <: LA](ref: Tracker[Option[Schema[_]]])                             extends SwaggerTerm[L, String]
case class GetItems[L <: LA](arr: Tracker[ArraySchema])                                       extends SwaggerTerm[L, Tracker[Schema[_]]]
case class GetType[L <: LA](model: Tracker[Schema[_]])                                        extends SwaggerTerm[L, Tracker[String]]
case class FallbackPropertyTypeHandler[L <: LA](prop: Schema[_])                              extends SwaggerTerm[L, L#Type]
case class ResolveType[L <: LA](name: String, protocolElems: List[StrictProtocolElems[L]])    extends SwaggerTerm[L, StrictProtocolElems[L]]
case class FallbackResolveElems[L <: LA](lazyElems: List[LazyProtocolElems[L]])               extends SwaggerTerm[L, List[StrictProtocolElems[L]]]
case class LogPush[L <: LA](name: String)                                                     extends SwaggerTerm[L, Unit]
case class LogPop[L <: LA]()                                                                  extends SwaggerTerm[L, Unit]
case class LogDebug[L <: LA](message: String)                                                 extends SwaggerTerm[L, Unit]
case class LogInfo[L <: LA](message: String)                                                  extends SwaggerTerm[L, Unit]
case class LogWarning[L <: LA](message: String)                                               extends SwaggerTerm[L, Unit]
case class LogError[L <: LA](message: String)                                                 extends SwaggerTerm[L, Unit]
