package dev.guardrail

import _root_.io.swagger.v3.oas.models.Operation
import _root_.io.swagger.v3.oas.models.parameters.Parameter
import scala.collection.JavaConverters._

package object shims {
  implicit class OperationExt(operation: Operation) {
    def consumes: List[String] =
      for {
        body        <- Option(operation.getRequestBody()).toList
        content     <- Option(body.getContent()).toList
        contentType <- content.asScala.keys
        if contentType != "*/*"
      } yield contentType

    def produces: List[String] =
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
