package com.twilio.swagger.codegen
package terms.server

import _root_.io.swagger.models.{ModelImpl, Operation, Path}
import cats.data.NonEmptyList
import com.twilio.swagger.codegen.generators.ScalaParameter
import scala.meta._

sealed trait ServerTerm[T]
case class ExtractOperations(paths: List[(String, Path)]) extends ServerTerm[List[ServerRoute]]

case class GetClassName(operation: Operation) extends ServerTerm[NonEmptyList[String]]
case class BuildTracingFields(operation: Operation, className: NonEmptyList[String], tracing: Boolean) extends ServerTerm[Option[(ScalaParameter, Term)]]
case class GenerateRoute(resourceName: String, basePath: Option[String], route: ServerRoute, tracingFields: Option[(ScalaParameter, Term)], responseDefinitions: List[Defn], protocolElems: List[StrictProtocolElems]) extends ServerTerm[RenderedRoute]
case class GetExtraRouteParams(tracing: Boolean) extends ServerTerm[List[Term.Param]]
case class GenerateResponseDefinitions(operation: Operation, protocolElems: List[StrictProtocolElems]) extends ServerTerm[List[Defn]]
case class RenderClass(className: String, handlerName: String, combinedRouteTerms: Term, extraRouteParams: List[Term.Param], responseDefinitions: List[Defn]) extends ServerTerm[Stat]
case class RenderHandler(handlerName: String, methodSigs: List[Decl.Def]) extends ServerTerm[Stat]
case class CombineRouteTerms(terms: List[Term]) extends ServerTerm[Term]
case class GetFrameworkImports(tracing: Boolean) extends ServerTerm[List[Import]]
case class GetExtraImports(tracing: Boolean) extends ServerTerm[List[Import]]
