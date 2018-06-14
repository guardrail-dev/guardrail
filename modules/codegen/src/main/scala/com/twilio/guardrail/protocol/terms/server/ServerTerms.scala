package com.twilio.guardrail.protocol.terms.server

import _root_.io.swagger.models.{Operation, Path}
import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.generators.ScalaParameter
import com.twilio.guardrail.{RenderedRoute, ServerRoute, StrictProtocolElems}

import scala.meta._

class ServerTerms[F[_]](implicit I: InjectK[ServerTerm, F]) {
  def extractOperations(paths: List[(String, Path)]): Free[F, List[ServerRoute]] =
    Free.inject(ExtractOperations(paths))
  def getClassName(operation: Operation): Free[F, List[String]] =
    Free.inject(GetClassName(operation))
  def buildTracingFields(
      operation: Operation,
      className: List[String],
      tracing: Boolean): Free[F, Option[(ScalaParameter, Term)]] =
    Free.inject(BuildTracingFields(operation, className, tracing))
  def generateRoute(
      resourceName: String,
      basePath: Option[String],
      tracingFields: Option[(ScalaParameter, Term)],
      responseDefinitions: List[Defn],
      protocolElems: List[StrictProtocolElems])(route: ServerRoute): Free[F, RenderedRoute] =
    Free.inject(GenerateRoute(resourceName, basePath, route, tracingFields, responseDefinitions, protocolElems))
  def getExtraRouteParams(tracing: Boolean): Free[F, List[Term.Param]] =
    Free.inject(GetExtraRouteParams(tracing))
  def generateResponseDefinitions(operation: Operation, protocolElems: List[StrictProtocolElems]): Free[F, List[Defn]] =
    Free.inject(GenerateResponseDefinitions(operation, protocolElems))
  def renderClass(
      resourceName: String,
      handlerName: String,
      combinedRouteTerms: Term,
      extraRouteParams: List[Term.Param],
      responseDefinitions: List[Defn]): Free[F, Stat] =
    Free.inject(RenderClass(resourceName, handlerName, combinedRouteTerms, extraRouteParams, responseDefinitions))
  def renderHandler(handlerName: String, methodSigs: List[Decl.Def]) =
    Free.inject(RenderHandler(handlerName, methodSigs))
  def combineRouteTerms(terms: List[Term]): Free[F, Term] =
    Free.inject(CombineRouteTerms(terms))
  def getExtraImports(tracing: Boolean): Free[F, List[Import]] =
    Free.inject(GetExtraImports(tracing))
}

object ServerTerms {
  implicit def serverTerms[F[_]](implicit I: InjectK[ServerTerm, F]): ServerTerms[F] =
    new ServerTerms[F]
}
