package com.twilio.guardrail.protocol.terms.server

import _root_.io.swagger.models.{ Operation, Path }
import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.generators.GeneratorSettings
import com.twilio.guardrail.{ RenderedRoutes, ServerRoute, StrictProtocolElems, TracingField }

import scala.meta._

class ServerTerms[F[_]](implicit I: InjectK[ServerTerm, F]) {
  def extractOperations(paths: List[(String, Path)]): Free[F, List[ServerRoute]] =
    Free.inject(ExtractOperations(paths))
  def getClassName(operation: Operation): Free[F, List[String]] =
    Free.inject(GetClassName(operation))
  def buildTracingFields(operation: Operation, resourceName: List[String], tracing: Boolean): Free[F, Option[TracingField]] =
    Free.inject(BuildTracingFields(operation, resourceName, tracing))
  def generateRoutes(className: List[String], resourceName: String, basePath: Option[String], tracing: Boolean, protocolElems: List[StrictProtocolElems])(
      routes: List[ServerRoute]
  ): Free[F, RenderedRoutes] =
    Free.inject(GenerateRoutes(className, resourceName, basePath, routes, tracing, protocolElems))
  def getExtraRouteParams(tracing: Boolean): Free[F, List[Term.Param]] =
    Free.inject(GetExtraRouteParams(tracing))
  def generateResponseDefinitions(operation: Operation, protocolElems: List[StrictProtocolElems]): Free[F, List[Defn]] =
    Free.inject(GenerateResponseDefinitions(operation, protocolElems))
  def renderClass(resourceName: String,
                  handlerName: String,
                  combinedRouteTerms: Term,
                  extraRouteParams: List[Term.Param],
                  responseDefinitions: List[Defn],
                  supportDefinitions: List[Defn]): Free[F, List[Stat]] =
    Free.inject(RenderClass(resourceName, handlerName, combinedRouteTerms, extraRouteParams, responseDefinitions, supportDefinitions))
  def renderHandler(handlerName: String, methodSigs: List[Decl.Def], handlerDefinitions: List[Stat]) =
    Free.inject(RenderHandler(handlerName, methodSigs, handlerDefinitions))
  def getExtraImports(tracing: Boolean): Free[F, List[Import]] =
    Free.inject(GetExtraImports(tracing))
}

object ServerTerms {
  implicit def serverTerms[F[_]](implicit I: InjectK[ServerTerm, F]): ServerTerms[F] =
    new ServerTerms[F]
}
