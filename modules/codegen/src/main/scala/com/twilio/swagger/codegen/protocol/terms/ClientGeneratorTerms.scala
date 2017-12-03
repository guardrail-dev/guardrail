package com.twilio.swagger.codegen
package terms.client

import _root_.io.swagger.models.{ModelImpl, Operation, Path}
import cats.free.{Free, Inject}
import scala.meta._

class ClientTerms[F[_]](implicit I: Inject[ClientTerm, F]) {
  def extractOperations(paths: List[(String, Path)]): Free[F, List[ClientRoute]] =
    Free.inject[ClientTerm, F](ExtractOperations(paths))
  def getClassName(operation: Operation): Free[F, List[String]] =
    Free.inject[ClientTerm, F](GetClassName(operation))
  def generateClientOperation(className: List[String], tracing: Boolean, protocolElems: List[StrictProtocolElems])(route: ClientRoute): Free[F, Defn] =
    Free.inject[ClientTerm, F](GenerateClientOperation(className, route, tracing, protocolElems))
  def getImports(tracing: Boolean): Free[F, List[Import]] =
    Free.inject[ClientTerm, F](GetImports(tracing))
  def getExtraImports(tracing: Boolean): Free[F, List[Import]] =
    Free.inject[ClientTerm, F](GetExtraImports(tracing))
  def clientClsArgs(tracingName: Option[String], schemes: List[String], host: Option[String], tracing: Boolean): Free[F, List[List[Term.Param]]] =
    Free.inject[ClientTerm, F](ClientClsArgs(tracingName, schemes, host, tracing))
  def buildCompanion(clientName: String, tracingName: Option[String], schemes: List[String], host: Option[String], ctorArgs: List[List[Term.Param]], tracing: Boolean): Free[F, Defn.Object] =
    Free.inject[ClientTerm, F](BuildCompanion(clientName, tracingName, schemes, host, ctorArgs, tracing))
  def buildClient(clientName: String, tracingName: Option[String], schemes: List[String], host: Option[String], basePath: Option[String], ctorArgs: List[List[Term.Param]], clientCalls: List[Defn], tracing: Boolean): Free[F, Defn.Class] =
    Free.inject[ClientTerm, F](BuildClient(clientName, tracingName, schemes, host, basePath, ctorArgs, clientCalls, tracing))
  def getFrameworkImports(tracing: Boolean): Free[F, List[Import]] =
    Free.inject[ClientTerm, F](GetFrameworkImports(tracing))
}

object ClientTerms {
  implicit def enumTerms[F[_]](implicit I: Inject[ClientTerm, F]): ClientTerms[F] = new ClientTerms[F]
}
