package com.twilio.guardrail

import _root_.io.swagger.v3.oas.models.Operation
import _root_.io.swagger.v3.oas.models.media.MediaType
import _root_.io.swagger.v3.oas.models.media.Schema
import _root_.io.swagger.v3.oas.models.parameters.Parameter
import _root_.io.swagger.v3.oas.models.{ OpenAPI, PathItem }
import java.net.URI
import scala.collection.JavaConverters._

package object shims {
  implicit class OpenApiExt(swagger: OpenAPI) {
    val serverUrls: List[String] = swagger.getServers.asScala.toList.map(_.getUrl)

    def schemes(): List[String] =
      serverUrls.filter(_ != "/").map(s => new URI(s)).map(uri => Option(uri.getScheme)).collect { case Some(x) => x }

    def host(): Option[String] =
      for {
        list <- Option(serverUrls.filter(_ != "/")).filter(_.nonEmpty)
        head <- list.headOption
      } yield {
        new URI(head).getAuthority
      }

    def basePath(): Option[String] = {
      val pathOpt = for {
        list <- Option(serverUrls.filter(_ != "/")).filter(_.nonEmpty)
        head <- list.headOption
        path <- Option(new URI(head).getPath)
      } yield path

      pathOpt.filter(_ != "/")
    }

    def getPathsOpt(): List[(String, PathItem)] =
      Option(swagger.getPaths).map(_.asScala.toList).getOrElse(List.empty)
  }

  implicit class OperationExt(operation: Operation) {
    def produces: Seq[String] =
      Try(operation.getResponses.values().asScala.toList.flatMap(apiResponse => apiResponse.getContent.keySet().asScala.toList)).toOption
        .getOrElse(Seq.empty)

    def consumes: Seq[String] =
      Try(operation.getRequestBody.getContent.keySet()).toOption.fold(List.empty[String])(_.asScala.toList)

  }

  implicit class MediaTypeExt(mt: MediaType) {
    def requiredFields(): Set[String] = Option(mt.getSchema.getRequired).map(_.asScala.toSet).getOrElse(Set.empty)
  }

  implicit class ParameterExt(parameter: Parameter) {
    def format(): String = parameter.getSchema.getFormat()

    def isInCookies: Boolean  = parameter.getIn == "cookie"
    def isInQuery: Boolean    = parameter.getIn == "query"
    def isInPath: Boolean     = parameter.getIn == "path"
    def isInHeader: Boolean   = parameter.getIn == "header"
    def isInBody: Boolean     = parameter.getIn == "body"
    def isInFormData: Boolean = parameter.getIn == "formData"
    def isRef: Boolean        = Option(parameter.get$ref()).isDefined

    def getSimpleRef: String =
      if (isRef) {
        parameter.get$ref().split('/').last
      } else {
        throw new IllegalStateException("not a ref")
      }
  }

  implicit class SchemaExt(schema: Schema[_]) {
    def extractEnum(): Option[List[String]] =
      Option(schema.getEnum())
        .map(_.asScala.map(_.asInstanceOf[String]).to[List])

    def getSimpleRef: Option[String] =
      Option(schema.get$ref()).flatMap(_.split('/').lastOption)
  }
}
