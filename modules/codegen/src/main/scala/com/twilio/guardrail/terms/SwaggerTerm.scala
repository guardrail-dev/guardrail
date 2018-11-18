package com.twilio.guardrail
package terms

import _root_.io.swagger.models._
import cats.MonadError
import cats.implicits._
import com.twilio.guardrail.generators.{ GeneratorSettings, ScalaParameter, ScalaParameters }
import com.twilio.guardrail.languages.{ LA, ScalaLanguage }
import scala.collection.JavaConverters._

case class RouteMeta(path: String, method: HttpMethod, operation: Operation) {
  private val parameters = {
    Option(operation.getParameters)
      .map(_.asScala.toList)
  }

  def getParameters(protocolElems: List[StrictProtocolElems[ScalaLanguage]], gs: GeneratorSettings[ScalaLanguage]): Target[ScalaParameters] =
    parameters
      .map(ScalaParameter.fromParameters(protocolElems, gs))
      .getOrElse(Target.pure(List.empty[ScalaParameter]))
      .map(new ScalaParameters(_))
}

sealed trait SwaggerTerm[L <: LA, T]
case class ExtractOperations[L <: LA](paths: List[(String, Path)]) extends SwaggerTerm[L, List[RouteMeta]]
case class GetClassName[L <: LA](operation: Operation)             extends SwaggerTerm[L, List[String]]
