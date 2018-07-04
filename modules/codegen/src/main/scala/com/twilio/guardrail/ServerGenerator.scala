package com.twilio.guardrail

import _root_.io.swagger.models._
import cats.Id
import cats.data.NonEmptyList
import cats.free.Free
import cats.instances.all._
import cats.syntax.all._
import com.twilio.guardrail.generators.ScalaParameter
import com.twilio.guardrail.protocol.terms.server.{ ServerTerm, ServerTerms }

import scala.collection.JavaConverters._
import scala.meta._

case class Servers(servers: List[Server])
case class Server(pkg: List[String], extraImports: List[Import], src: List[Stat])
case class ServerRoute(path: String, method: HttpMethod, operation: Operation)
case class RenderedRoute(
    route: Term,
    methodSig: Decl.Def,
    responseDefinitions: List[Defn],
    supportDefinitions: List[Defn],
    handlerDefinitions: List[Stat]
)

object ServerGenerator {
  import NelShim._

  type ServerGenerator[A] = ServerTerm[A]

  def formatClassName(str: String): String   = s"${str.capitalize}Resource"
  def formatHandlerName(str: String): String = s"${str.capitalize}Handler"

  def fromSwagger[F[_]](context: Context, swagger: Swagger, frameworkImports: List[Import])(
      protocolElems: List[StrictProtocolElems]
  )(implicit S: ServerTerms[F]): Free[F, Servers] = {
    import S._

    val paths: List[(String, Path)] =
      Option(swagger.getPaths).map(_.asScala.toList).getOrElse(List.empty)
    val basePath: Option[String] = Option(swagger.getBasePath)

    for {
      routes           <- extractOperations(paths)
      classNamedRoutes <- routes.traverse(route => getClassName(route.operation).map(_ -> route))
      groupedRoutes = classNamedRoutes
        .groupBy(_._1)
        .mapValues(_.map(_._2))
        .toList
      extraImports <- getExtraImports(context.tracing)
      servers <- groupedRoutes.traverse {
        case (className, routes) =>
          val resourceName = formatClassName(className.lastOption.getOrElse(""))
          val handlerName =
            formatHandlerName(className.lastOption.getOrElse(""))
          for {
            renderedRoutes <- routes.traverse {
              case sr @ ServerRoute(path, method, operation) =>
                for {
                  tracingFields       <- buildTracingFields(operation, className, context.tracing)
                  responseDefinitions <- generateResponseDefinitions(operation, protocolElems)
                  rendered            <- generateRoute(resourceName, basePath, tracingFields, responseDefinitions, protocolElems)(sr)
                } yield rendered
            }
            routeTerms = renderedRoutes.map(_.route)
            combinedRouteTerms <- combineRouteTerms(routeTerms)
            methodSigs = renderedRoutes.map(_.methodSig)
            handlerSrc       <- renderHandler(formatHandlerName(className.lastOption.getOrElse("")), methodSigs, renderedRoutes.flatMap(_.handlerDefinitions))
            extraRouteParams <- getExtraRouteParams(context.tracing)
            responseDefinitions = renderedRoutes.flatMap(_.responseDefinitions)
            classSrc <- renderClass(resourceName,
                                    handlerName,
                                    combinedRouteTerms,
                                    extraRouteParams,
                                    responseDefinitions,
                                    renderedRoutes.flatMap(_.supportDefinitions))
          } yield {
            Server(className, frameworkImports ++ extraImports, List(handlerSrc, classSrc))
          }
      }
    } yield Servers(servers)
  }
}
