package com.twilio.swagger.codegen
package terms.server

import _root_.io.swagger.models.{ModelImpl, Operation, Path}
import cats.data.NonEmptyList
import cats.free.{Free, Inject}
import com.twilio.swagger.codegen.generators.ScalaParameter
import scala.meta._

class ServerTerms[F[_]](implicit I: Inject[ServerTerm, F]) {
  def extractOperations(paths: List[(String, Path)]): Free[F, List[ServerRoute]] =
    Free.inject(ExtractOperations(paths))
  def getClassName(operation: Operation): Free[F, NonEmptyList[String]] =
    Free.inject(GetClassName(operation))
  def buildTracingFields(operation: Operation, className: NonEmptyList[String], tracing: Boolean): Free[F, Option[(ScalaParameter, Term)]] =
    Free.inject(BuildTracingFields(operation, className, tracing))
  def generateRoute(resourceName: String, basePath: Option[String], tracingFields: Option[(ScalaParameter, Term)], responseDefinitions: List[Defn], protocolElems: List[StrictProtocolElems])(route: ServerRoute): Free[F, RenderedRoute] =
    Free.inject(GenerateRoute(resourceName, basePath, route, tracingFields, responseDefinitions, protocolElems))
  def getExtraRouteParams(tracing: Boolean): Free[F, List[Term.Param]] =
    Free.inject(GetExtraRouteParams(tracing))
  def generateResponseDefinitions(operation: Operation, protocolElems: List[StrictProtocolElems]): Free[F, List[Defn]] =
    Free.inject(GenerateResponseDefinitions(operation, protocolElems))
  def renderClass(resourceName: String, handlerName: String, combinedRouteTerms: Term, extraRouteParams: List[Term.Param], responseDefinitions: List[Defn]): Free[F, Stat] =
    Free.inject(RenderClass(resourceName, handlerName, combinedRouteTerms, extraRouteParams, responseDefinitions))
  def renderHandler(handlerName: String, methodSigs: List[Decl.Def]) =
    Free.inject(RenderHandler(handlerName, methodSigs))
  def combineRouteTerms(terms: List[Term]): Free[F, Term] =
    Free.inject(CombineRouteTerms(terms))
  def getFrameworkImports(tracing: Boolean): Free[F, List[Import]] =
    Free.inject(GetFrameworkImports(tracing))
  def getExtraImports(tracing: Boolean): Free[F, List[Import]] =
    Free.inject(GetExtraImports(tracing))
}

object ServerTerms {
  implicit def serverTerms[F[_]](implicit I: Inject[ServerTerm, F]): ServerTerms[F] = new ServerTerms[F]
}
