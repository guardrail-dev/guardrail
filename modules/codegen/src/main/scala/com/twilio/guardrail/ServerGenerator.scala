package com.twilio.guardrail

import _root_.io.swagger.models._
import cats.Id
import cats.data.NonEmptyList
import cats.free.Free
import cats.instances.all._
import cats.syntax.all._
import com.twilio.guardrail.generators.ScalaParameter
import com.twilio.guardrail.languages.{ LA, ScalaLanguage }
import com.twilio.guardrail.protocol.terms.server.{ ServerTerm, ServerTerms }
import scala.collection.JavaConverters._

case class Servers[L <: LA](servers: List[Server[L]])
case class Server[L <: LA](pkg: List[String], extraImports: List[L#Import], src: List[L#Statement])
case class TracingField[L <: LA](param: ScalaParameter, term: L#Term)
case class ServerRoute(path: String, method: HttpMethod, operation: Operation)
case class RenderedRoutes[L <: LA](
    routes: L#Term,
    methodSigs: List[L#MethodDeclaration],
    supportDefinitions: List[L#Definition],
    handlerDefinitions: List[L#Statement]
)

object ServerGenerator {
  import NelShim._

  def formatClassName(str: String): String   = s"${str.capitalize}Resource"
  def formatHandlerName(str: String): String = s"${str.capitalize}Handler"

  def fromSwagger[L <: LA, F[_]](context: Context, swagger: Swagger, frameworkImports: List[L#Import])(
      protocolElems: List[StrictProtocolElems[L]]
  )(implicit S: ServerTerms[L, F]): Free[F, Servers[L]] = {
    import S._

    val paths: List[(String, Path)] =
      Option(swagger.getPaths).map(_.asScala.toList).getOrElse(List.empty)
    val basePath: Option[String] = Option(swagger.getBasePath)

    for {
      routes <- extractOperations(paths)
      classNamedRoutes <- routes
        .traverse(route => getClassName(route.operation).map(_ -> route))
      groupedRoutes = classNamedRoutes
        .groupBy(_._1)
        .mapValues(_.map(_._2))
        .toList
      extraImports <- getExtraImports(context.tracing)
      servers <- groupedRoutes.traverse {
        case (className, unsortedRoutes) =>
          val routes       = unsortedRoutes.sortBy(r => (r.path, r.method))
          val resourceName = formatClassName(className.lastOption.getOrElse(""))
          val handlerName =
            formatHandlerName(className.lastOption.getOrElse(""))
          for {
            responseDefinitions <- routes.flatTraverse {
              case sr @ ServerRoute(path, method, operation) =>
                for {
                  responseDefinitions <- generateResponseDefinitions(operation, protocolElems)
                } yield responseDefinitions
            }
            tracingFields    <- routes.traverse { case ServerRoute(_, _, operation) => buildTracingFields(operation, className, context.tracing) }
            renderedRoutes   <- generateRoutes(resourceName, basePath, tracingFields.zip(routes), protocolElems)
            handlerSrc       <- renderHandler(formatHandlerName(className.lastOption.getOrElse("")), renderedRoutes.methodSigs, renderedRoutes.handlerDefinitions)
            extraRouteParams <- getExtraRouteParams(context.tracing)
            classSrc         <- renderClass(resourceName, handlerName, renderedRoutes.routes, extraRouteParams, responseDefinitions, renderedRoutes.supportDefinitions)
          } yield {
            Server(className, frameworkImports ++ extraImports, handlerSrc +: classSrc)
          }
      }
    } yield Servers[L](servers)
  }
}
