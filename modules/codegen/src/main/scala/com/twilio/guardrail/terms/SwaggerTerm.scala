package com.twilio.guardrail
package terms

import java.{ lang, util }

import cats.MonadError
import cats.free.Free
import cats.implicits._
import com.twilio.guardrail.generators.{ ScalaParameter, ScalaParameters }
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.shims._

import scala.collection.JavaConverters._
import com.twilio.guardrail.terms.framework.FrameworkTerms
import io.swagger.v3.oas.models.{ Operation, PathItem }
import io.swagger.v3.oas.models.PathItem.HttpMethod
import io.swagger.v3.oas.models.media.{ ArraySchema, MediaType, ObjectSchema, Schema }
import io.swagger.v3.oas.models.parameters.{ Parameter, RequestBody }
import io.swagger.v3.oas.models.responses.ApiResponse

case class RouteMeta(path: String, method: HttpMethod, operation: Operation) {

  private def extractPrimitiveFromRequestBody(requestBody: RequestBody): Option[Parameter] =
    for {
      content <- Option(requestBody.getContent())
      mt <- content.values().asScala.headOption
      tpe <- Option(mt.getSchema.getType())
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

      p.setExtensions(Option(schema.getExtensions).getOrElse(new util.HashMap[String, Object]()))
      p
    }

  private def extractRefParamFromRequestBody(requestBody: RequestBody): Option[Parameter] =
    for {
      content <- Option(requestBody.getContent)
      mt <- content.values().asScala.headOption
      ref <- Option(mt.getSchema.get$ref())
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

      p.setExtensions(Option(schema.getExtensions).getOrElse(new util.HashMap[String, Object]()))
      p
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
              Option[Boolean](requestBody.getRequired).getOrElse(false)
            }

            p.setRequired(isRequired)
            p.setExtensions(Option(schema.getExtensions).getOrElse(new util.HashMap[String, Object]()))
            p
        }
      }

  private val parameters: List[Parameter] = { //option of list is a bad signature
    val p = Option(operation.getParameters)
      .map(_.asScala.toList)
      .getOrElse(List.empty)

    val params =
      (Option(operation.getRequestBody).flatMap(extractRefParamFromRequestBody) ++
        p ++
        Option(operation.getRequestBody).toList.flatMap(extractParamsFromRequestBody) ++
        Option(operation.getRequestBody).flatMap(extractPrimitiveFromRequestBody)
      ).toList
    params
  }

  def getParameters[L <: LA, F[_]](
      protocolElems: List[StrictProtocolElems[L]]
  )(implicit Fw: FrameworkTerms[L, F], Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F]): Free[F, ScalaParameters[L]] = {
    ScalaParameter.fromParameters(protocolElems).apply(parameters)
      .map({ a =>
        new ScalaParameters[L](a)
      })
  }
}

sealed trait SwaggerTerm[L <: LA, T]
case class ExtractOperations[L <: LA](paths: List[(String, PathItem)])                     extends SwaggerTerm[L, List[RouteMeta]]
case class GetClassName[L <: LA](operation: Operation)                                     extends SwaggerTerm[L, List[String]]
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
