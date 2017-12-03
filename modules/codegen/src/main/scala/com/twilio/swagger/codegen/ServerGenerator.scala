package com.twilio.swagger.codegen

import _root_.io.swagger.models._
import cats.Id
import cats.data.NonEmptyList
import cats.free.Free
import cats.instances.all._
import cats.syntax.all._
import com.twilio.swagger.codegen.generators.ScalaParameter
import com.twilio.swagger.codegen.terms.server._
import scala.collection.JavaConverters._
import scala.meta._

case class Servers(servers: List[Server], frameworkImports: List[Import])
case class Server(pkg: NonEmptyList[String], extraImports: List[Import], src: List[Stat])
case class ServerRoute(path: String, method: HttpMethod, operation: Operation)
case class RenderedRoute(route: Term, methodSig: Decl.Def, responseDefinitions: List[Defn])
object ServerGenerator {
  import NelShim._

  type ServerGenerator[A] = ServerTerm[A]

  def formatClassName(str: String): String = s"${str.capitalize}Resource"
  def formatHandlerName(str: String): String = s"${str.capitalize}Handler"

  def fromSwagger[F[_]](context: Context, swagger: Swagger)(protocolElems: List[StrictProtocolElems])(implicit S: ServerTerms[F]): Free[F, Servers] = {
    import S._

    val paths: List[(String, Path)] = Option(swagger.getPaths).map(_.asScala.toList).getOrElse(List.empty)
    val basePath: Option[String] = Option(swagger.getBasePath)

    for {
      routes <- extractOperations(paths)
      classNamedRoutes <- routes.map(route => getClassName(route.operation).map(_ -> route)).sequenceU
      groupedRoutes = classNamedRoutes.groupBy(_._1).mapValues(_.map(_._2)).toList
      frameworkImports <- getFrameworkImports(context.tracing)
      extraImports <- getExtraImports(context.tracing)
      servers <- groupedRoutes.map({ case (className, routes) =>
          val resourceName = formatClassName(className.last)
          val handlerName = formatHandlerName(className.last)
          for {
            renderedRoutes <- routes.map({ case sr@ServerRoute(path, method, operation) =>
              for {
                tracingFields <- buildTracingFields(operation, className, context.tracing)
                responseDefinitions <- generateResponseDefinitions(operation)
                rendered <- generateRoute(resourceName, basePath, tracingFields, responseDefinitions, protocolElems)(sr)
              } yield rendered
            }).sequenceU
            routeTerms = renderedRoutes.map(_.route)
            combinedRouteTerms <- combineRouteTerms(routeTerms)
            methodSigs = renderedRoutes.map(_.methodSig)
            handlerSrc <- renderHandler(formatHandlerName(className.last), methodSigs)
            extraRouteParams <- getExtraRouteParams(context.tracing)
            responseDefinitions = renderedRoutes.flatMap(_.responseDefinitions)
            classSrc <- renderClass(resourceName, handlerName, combinedRouteTerms, extraRouteParams, responseDefinitions)
          } yield {
            Server(className, frameworkImports ++ extraImports, List(SwaggerUtil.escapeTree(handlerSrc), SwaggerUtil.escapeTree(classSrc)))
          }
        }).sequenceU
    } yield Servers(servers, frameworkImports)
  }
}
