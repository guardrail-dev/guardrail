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
    val serverUrls: List[String] = Option(swagger.getServers).toList.flatMap(_.asScala.toList).map(_.getUrl)

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
    def consumes: Seq[String] =
      for {
        body        <- Option(operation.getRequestBody()).toList
        content     <- Option(body.getContent()).toList
        contentType <- content.asScala.keys
        if contentType != "*/*"
      } yield contentType

    def produces: Seq[String] =
      for {
        responses   <- Option(operation.getResponses()).toList
        response    <- responses.asScala.values
        content     <- Option(response.getContent()).toList
        contentType <- content.asScala.keys
        if contentType != "*/*"
      } yield contentType
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
  }
}
