package com.twilio.swagger.codegen
package terms.client

import _root_.io.swagger.models.{ModelImpl, Operation, Path}
import scala.collection.immutable.Seq
import scala.meta._

sealed trait ClientTerm[T]
case class ExtractOperations(paths: List[(String, Path)]) extends ClientTerm[List[ClientRoute]]
case class GetClassName(operation: Operation) extends ClientTerm[List[String]]
case class GenerateClientOperation(className: List[String], route: ClientRoute, tracing: Boolean, protocolElems: List[ProtocolElems]) extends ClientTerm[Defn]
case class GetImports(tracing: Boolean) extends ClientTerm[Seq[Import]]
case class GetExtraImports(tracing: Boolean) extends ClientTerm[Seq[Import]]
case class ClientClsArgs(tracingName: Option[String], schemes: Seq[String], host: Option[String], tracing: Boolean) extends ClientTerm[Seq[Seq[Term.Param]]]
case class BuildCompanion(clientName: String, tracingName: Option[String], schemes: Seq[String], host: Option[String], ctorArgs: Seq[Seq[Term.Param]], tracing: Boolean) extends ClientTerm[Defn.Object]
case class BuildClient(clientName: String, tracingName: Option[String], schemes: Seq[String], host: Option[String], basePath: Option[String], ctorArgs: Seq[Seq[Term.Param]], clientCalls: Seq[Defn], tracing: Boolean) extends ClientTerm[Defn.Class]
case class GetFrameworkImports(tracing: Boolean) extends ClientTerm[Seq[Import]]
