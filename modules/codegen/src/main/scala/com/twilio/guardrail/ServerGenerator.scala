package com.twilio.guardrail

import _root_.io.swagger.models._
import cats.Id
import cats.data.NonEmptyList
import cats.free.Free
import cats.instances.all._
import cats.syntax.all._
import com.twilio.guardrail.generators.{ Http4sHelper, ScalaParameter }
import com.twilio.guardrail.languages.{ LA, ScalaLanguage }
import com.twilio.guardrail.protocol.terms.server.{ ServerTerm, ServerTerms }
import com.twilio.guardrail.terms.{ RouteMeta, ScalaTerms, SwaggerTerms }
import com.twilio.guardrail.terms.framework.FrameworkTerms
import scala.collection.JavaConverters._

case class Servers[L <: LA](servers: List[Server[L]])
case class Server[L <: LA](pkg: List[String], extraImports: List[L#Import], src: List[L#Statement])
case class TracingField[L <: LA](param: ScalaParameter[L], term: L#Term)
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
  )(implicit Fw: FrameworkTerms[L, F], Sc: ScalaTerms[L, F], S: ServerTerms[L, F], Sw: SwaggerTerms[L, F]): Free[F, Servers[L]] = {
    import S._
    import Sw._

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
            responseServerPair <- routes.traverse {
              case route @ RouteMeta(path, method, operation) =>
                for {
                  operationId         <- getOperationId(operation)
                  responses           <- Http4sHelper.getResponsesF(operationId, operation, protocolElems)
                  responseDefinitions <- generateResponseDefinitions(operationId, responses, protocolElems)
                  parameters          <- route.getParametersF[L, F](protocolElems)
                  tracingField        <- buildTracingFields(operation, className, context.tracing)
                } yield (responseDefinitions, (tracingField, route))
            }
            (responseDefinitions, serverOperations) = responseServerPair.unzip
            renderedRoutes   <- generateRoutes(resourceName, basePath, serverOperations, protocolElems)
            handlerSrc       <- renderHandler(formatHandlerName(className.lastOption.getOrElse("")), renderedRoutes.methodSigs, renderedRoutes.handlerDefinitions)
            extraRouteParams <- getExtraRouteParams(context.tracing)
            classSrc <- renderClass(resourceName,
                                    handlerName,
                                    renderedRoutes.routes,
                                    extraRouteParams,
                                    responseDefinitions.flatten,
                                    renderedRoutes.supportDefinitions)
          } yield {
            Server(className, frameworkImports ++ extraImports, handlerSrc +: classSrc)
          }
      }
    } yield Servers[L](servers)
  }
}
