package com.twilio.guardrail.protocol.terms.client

import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.StrictProtocolElems
import com.twilio.guardrail.terms.RouteMeta
import com.twilio.guardrail.generators.GeneratorSettings

import scala.meta._

class ClientTerms[F[_]](implicit I: InjectK[ClientTerm, F]) {
  def generateClientOperation(className: List[String], tracing: Boolean, protocolElems: List[StrictProtocolElems])(
      route: RouteMeta
  ): Free[F, Defn] =
    Free.inject[ClientTerm, F](GenerateClientOperation(className, route, tracing, protocolElems))
  def getImports(tracing: Boolean): Free[F, List[Import]] =
    Free.inject[ClientTerm, F](GetImports(tracing))
  def getExtraImports(tracing: Boolean): Free[F, List[Import]] =
    Free.inject[ClientTerm, F](GetExtraImports(tracing))
  def clientClsArgs(tracingName: Option[String], schemes: List[String], host: Option[String], tracing: Boolean): Free[F, List[List[Term.Param]]] =
    Free.inject[ClientTerm, F](ClientClsArgs(tracingName, schemes, host, tracing))
  def buildCompanion(clientName: String,
                     tracingName: Option[String],
                     schemes: List[String],
                     host: Option[String],
                     ctorArgs: List[List[Term.Param]],
                     tracing: Boolean): Free[F, Defn.Object] =
    Free.inject[ClientTerm, F](BuildCompanion(clientName, tracingName, schemes, host, ctorArgs, tracing))
  def buildClient(clientName: String,
                  tracingName: Option[String],
                  schemes: List[String],
                  host: Option[String],
                  basePath: Option[String],
                  ctorArgs: List[List[Term.Param]],
                  clientCalls: List[Defn],
                  tracing: Boolean): Free[F, Defn.Class] =
    Free.inject[ClientTerm, F](BuildClient(clientName, tracingName, schemes, host, basePath, ctorArgs, clientCalls, tracing))
}

object ClientTerms {
  implicit def enumTerms[F[_]](implicit I: InjectK[ClientTerm, F]): ClientTerms[F] =
    new ClientTerms[F]
}
