package dev.guardrail.generators

import cats.data.NonEmptyList
import cats.syntax.all._

import dev.guardrail._
import dev.guardrail.core.SupportDefinition
import dev.guardrail.languages.LA
import dev.guardrail.terms.Responses
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.protocol.StrictProtocolElems
import dev.guardrail.terms.server.{ GenerateRouteMeta, SecurityExposure, ServerTerms }
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, RouteMeta, SecurityScheme, SwaggerTerms }
import dev.guardrail.core.Tracker
import io.swagger.v3.oas.models.Components

case class Servers[L <: LA](servers: List[Server[L]], supportDefinitions: List[SupportDefinition[L]])
case class Server[L <: LA](pkg: List[String], extraImports: List[L#Import], handlerDefinition: L#Definition, serverDefinitions: List[L#Definition])
case class CustomExtractionField[L <: LA](param: LanguageParameter[L], term: L#Term)
case class TracingField[L <: LA](param: LanguageParameter[L], term: L#Term)
case class RenderedRoutes[L <: LA](
    routes: List[L#Statement],
    classAnnotations: List[L#Annotation],
    methodSigs: List[L#MethodDeclaration],
    supportDefinitions: List[L#Definition],
    handlerDefinitions: List[L#Statement],
    securitySchemesDefinitions: List[L#Definition]
)

object ServerGenerator {
  def fromSwagger[L <: LA, F[_]](context: Context, supportPackage: NonEmptyList[String], basePath: Option[String], frameworkImports: List[L#Import])(
      groupedRoutes: List[(List[String], List[RouteMeta])]
  )(
      protocolElems: List[StrictProtocolElems[L]],
      securitySchemes: Map[String, SecurityScheme[L]],
      components: Tracker[Option[Components]]
  )(implicit Fw: FrameworkTerms[L, F], Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], S: ServerTerms[L, F], Sw: SwaggerTerms[L, F]): F[Servers[L]] = {
    import S._
    import Sw._
    import Sc._

    for {
      extraImports       <- getExtraImports(context.tracing, supportPackage)
      supportDefinitions <- generateSupportDefinitions(context.tracing, securitySchemes)
      servers <- groupedRoutes.traverse { case (className, unsortedRoutes) =>
        val routes = unsortedRoutes
          .groupBy(_.path.unwrapTracker.indexOf('{'))
          .view
          .mapValues(_.sortBy(r => (r.path.unwrapTracker, r.method)))
          .toList
          .sortBy(_._1)
          .flatMap(_._2)
        for {
          resourceName <- formatTypeName(className.lastOption.getOrElse(""), Some("Resource"))
          handlerName  <- formatTypeName(className.lastOption.getOrElse(""), Some("Handler"))

          responseServerPair <- routes.traverse { case route @ RouteMeta(path, method, operation, securityRequirements) =>
            for {
              operationId           <- getOperationId(operation)
              responses             <- Responses.getResponses(operationId, operation, protocolElems)
              responseClsName       <- formatTypeName(operationId, Some("Response"))
              responseDefinitions   <- generateResponseDefinitions(responseClsName, responses, protocolElems)
              methodName            <- formatMethodName(operationId)
              parameters            <- route.getParameters[L, F](components, protocolElems)
              customExtractionField <- buildCustomExtractionFields(operation, className, context.customExtraction)
              tracingField          <- buildTracingFields(operation, className, context.tracing)
            } yield (
              responseDefinitions,
              GenerateRouteMeta(operationId, methodName, responseClsName, customExtractionField, tracingField, route, parameters, responses)
            )
          }
          (responseDefinitions, serverOperations) = responseServerPair.unzip
          securityExposure = serverOperations.flatMap(_.routeMeta.securityRequirements) match {
            case Nil => SecurityExposure.Undefined
            case xs  => if (xs.exists(_.optional)) SecurityExposure.Optional else SecurityExposure.Required
          }
          renderedRoutes <- generateRoutes(
            context.tracing,
            resourceName,
            handlerName,
            basePath,
            serverOperations,
            protocolElems,
            securitySchemes,
            securityExposure,
            context.authImplementation
          )
          handlerSrc <- renderHandler(
            handlerName,
            renderedRoutes.methodSigs,
            renderedRoutes.handlerDefinitions,
            responseDefinitions.flatten,
            context.customExtraction,
            context.authImplementation,
            securityExposure
          )
          extraRouteParams <- getExtraRouteParams(
            resourceName,
            context.customExtraction,
            context.tracing,
            context.authImplementation,
            securityExposure
          )
          classSrc <- renderClass(
            resourceName,
            handlerName,
            renderedRoutes.classAnnotations,
            renderedRoutes.routes,
            extraRouteParams,
            responseDefinitions.flatten,
            renderedRoutes.supportDefinitions,
            renderedRoutes.securitySchemesDefinitions,
            context.customExtraction,
            context.authImplementation
          )
        } yield Server(className, frameworkImports ++ extraImports, handlerSrc, classSrc)
      }
    } yield Servers[L](servers, supportDefinitions)
  }
}
