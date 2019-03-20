package com.twilio.guardrail

import _root_.io.swagger.v3.oas.models._
import cats.free.Free
import cats.instances.all._
import cats.syntax.all._
import com.twilio.guardrail.generators.{ Http4sHelper, ScalaParameter }
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.protocol.terms.server.ServerTerms
import com.twilio.guardrail.shims._
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.terms.{ RouteMeta, ScalaTerms, SwaggerTerms }

case class Servers[L <: LA](servers: List[Server[L]], supportDefinitions: List[SupportDefinition[L]])
case class Server[L <: LA](pkg: List[String], extraImports: List[L#Import], handlerDefinition: L#Definition, serverDefinitions: List[L#Definition])
case class TracingField[L <: LA](param: ScalaParameter[L], term: L#Term)
case class RenderedRoutes[L <: LA](
    routes: List[L#Term],
    classAnnotations: List[L#Annotation],
    methodSigs: List[L#MethodDeclaration],
    supportDefinitions: List[L#Definition],
    handlerDefinitions: List[L#Statement]
)

object ServerGenerator {

  def formatClassName(str: String): String   = s"${str.capitalize}Resource"
  def formatHandlerName(str: String): String = s"${str.capitalize}Handler"

  def fromSwagger[L <: LA, F[_]](context: Context, swagger: OpenAPI, frameworkImports: List[L#Import])(
      protocolElems: List[StrictProtocolElems[L]]
  )(implicit Fw: FrameworkTerms[L, F], Sc: ScalaTerms[L, F], S: ServerTerms[L, F], Sw: SwaggerTerms[L, F]): Free[F, Servers[L]] = {
    import S._
    import Sw._

    val paths: List[(String, PathItem)] = swagger.getPathsOpt()
    val basePath: Option[String]        = swagger.basePath()

    for {
      routes <- extractOperations(paths)
      classNamedRoutes <- routes
        .traverse(route => getClassName(route.operation).map(_ -> route))
      groupedRoutes = classNamedRoutes
        .groupBy(_._1)
        .mapValues(_.map(_._2))
        .toList
      extraImports       <- getExtraImports(context.tracing)
      supportDefinitions <- generateSupportDefinitions(context.tracing)
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
                  responses           <- Http4sHelper.getResponses(operationId, operation, protocolElems)
                  responseDefinitions <- generateResponseDefinitions(operationId, responses, protocolElems)
                  parameters          <- route.getParameters[L, F](protocolElems)
                  tracingField        <- buildTracingFields(operation, className, context.tracing)
                } yield (responseDefinitions, (operationId, tracingField, route, parameters, responses))
            }
            (responseDefinitions, serverOperations) = responseServerPair.unzip
            renderedRoutes   <- generateRoutes(context.tracing, resourceName, basePath, serverOperations, protocolElems)
            handlerSrc       <- renderHandler(handlerName, renderedRoutes.methodSigs, renderedRoutes.handlerDefinitions)
            extraRouteParams <- getExtraRouteParams(context.tracing)
            classSrc <- renderClass(
              resourceName,
              handlerName,
              renderedRoutes.classAnnotations,
              renderedRoutes.routes,
              extraRouteParams,
              responseDefinitions.flatten,
              renderedRoutes.supportDefinitions
            )
          } yield {
            Server(className, frameworkImports ++ extraImports, handlerSrc, classSrc)
          }
      }
    } yield Servers[L](servers, supportDefinitions)
  }
}
