package com.twilio.guardrail

import _root_.io.swagger.models._
import cats.free.Free
import cats.instances.all._
import cats.syntax.all._
import com.twilio.guardrail.protocol.terms.client.{ ClientTerm, ClientTerms }
import com.twilio.guardrail.swagger.Escape

import scala.collection.JavaConverters._
import scala.meta.{ Lit, Term, Type, _ }
import com.twilio.guardrail.terms.RouteMeta

case class Clients(clients: List[Client])
case class Client(pkg: List[String], clientName: String, lines: List[Stat])

object ClientGenerator {
  type ClientGenerator[A] = ClientTerm[A]

  def fromSwagger[F[_]](context: Context, frameworkImports: List[Import])(
      schemes: List[String],
      host: Option[String],
      basePath: Option[String],
      groupedRoutes: List[(List[String], List[RouteMeta])]
  )(protocolElems: List[StrictProtocolElems])(implicit C: ClientTerms[F]): Free[F, Clients] = {
    import C._
    for {
      clientImports      <- getImports(context.tracing)
      clientExtraImports <- getExtraImports(context.tracing)
      clients <- groupedRoutes.traverse({
        case (pkg, routes) =>
          for {
            clientCalls <- routes.traverse(generateClientOperation(pkg, context.tracing, protocolElems) _)
            clientName  = s"${pkg.lastOption.getOrElse("").capitalize}Client"
            tracingName = Option(pkg.mkString("-")).filterNot(_.isEmpty)
            ctorArgs  <- clientClsArgs(tracingName, schemes, host, context.tracing)
            companion <- buildCompanion(clientName, tracingName, schemes, host, ctorArgs, context.tracing)
            client    <- buildClient(clientName, tracingName, schemes, host, basePath, ctorArgs, clientCalls, context.tracing)
          } yield {
            val stats: List[Stat] = clientImports ++ frameworkImports ++ clientExtraImports :+ companion :+ client
            Client(pkg, clientName, stats.map(Escape.escapeTree))
          }
      })
    } yield Clients(clients)
  }
}
