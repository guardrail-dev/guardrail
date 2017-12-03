package com.twilio.swagger.codegen

import _root_.io.swagger.models._
import cats.free.Free
import cats.instances.all._
import cats.syntax.all._
import scala.collection.JavaConverters._
import scala.meta.{Lit, Term, Type, _}

import com.twilio.swagger.codegen.terms.client._

case class Clients(clients: List[Client], frameworkImports: List[Import])
case class Client(pkg: List[String], clientName: String, lines: List[Stat])
case class ClientRoute(path: String, method: HttpMethod, operation: Operation)

object ClientGenerator {
  type ClientGenerator[A] = ClientTerm[A]

  def fromSwagger[F[_]](context: Context, swagger: Swagger)(protocolElems: List[StrictProtocolElems])(implicit C: ClientTerms[F]): Free[F, Clients] = {
    import C._
    val paths: List[(String, Path)] = Option(swagger.getPaths).map(_.asScala.toList).getOrElse(List.empty)
    val basePath: Option[String] = Option(swagger.getBasePath)
    val schemes: List[String] = Option(swagger.getSchemes).fold(List.empty[String])(_.asScala.to[List].map(_.toValue))

    for {
      routes <- extractOperations(paths)
      classNamedRoutes <- routes.map(route => getClassName(route.operation).map(_ -> route)).sequenceU
      groupedRoutes = classNamedRoutes.groupBy(_._1).mapValues(_.map(_._2)).toList
      clientImports <- getImports(context.tracing)
      frameworkImports <- getFrameworkImports(context.tracing)
      clientExtraImports <- getExtraImports(context.tracing)
      clients <- groupedRoutes.map({ case (pkg, routes) =>
        for {
          clientCalls <- routes.map(generateClientOperation(pkg, context.tracing, protocolElems) _).sequenceU
          clientName = s"${pkg.lastOption.getOrElse("").capitalize}Client"
          tracingName = Option(pkg.mkString("-")).filterNot(_.isEmpty)
          ctorArgs <- clientClsArgs(tracingName, schemes, Option(swagger.getHost), context.tracing)
          host = Option(swagger.getHost)
          companion <- buildCompanion(clientName, tracingName, schemes, host, ctorArgs, context.tracing)
          client <- buildClient(clientName, tracingName, schemes, host, basePath, ctorArgs, clientCalls, context.tracing)
        } yield {
          val stats: List[Stat] = (
            (clientImports ++ frameworkImports ++ clientExtraImports) :+
            companion :+
            client
          )

          Client(pkg, clientName, stats.map(SwaggerUtil.escapeTree))
        }
      }).sequenceU
    } yield Clients(clients, frameworkImports)
  }
}
