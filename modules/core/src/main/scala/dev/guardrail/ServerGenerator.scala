package dev.guardrail
import cats.data.NonEmptyList
import cats.syntax.all._
import dev.guardrail.generators.LanguageParameter
import dev.guardrail.languages.LA
import dev.guardrail.protocol.terms.Responses
import dev.guardrail.protocol.terms.server.{ GenerateRouteMeta, ServerTerms }
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, RouteMeta, SecurityScheme, SwaggerTerms }

case class Servers[L <: LA](servers: List[Server[L]], supportDefinitions: List[SupportDefinition[L]])
case class Server[L <: LA](pkg: List[String], extraImports: List[L#Import], handlerDefinition: L#Definition, serverDefinitions: List[L#Definition])
case class CustomExtractionField[L <: LA](param: LanguageParameter[L], term: L#Term)
case class TracingField[L <: LA](param: LanguageParameter[L], term: L#Term)
case class RenderedRoutes[L <: LA](
    routes: List[L#Statement],
    classAnnotations: List[L#Annotation],
    methodSigs: List[L#MethodDeclaration],
    supportDefinitions: List[L#Definition],
    handlerDefinitions: List[L#Statement]
)

object ServerGenerator {
  def fromSwagger[L <: LA, F[_]](context: Context, supportPackage: NonEmptyList[String], basePath: Option[String], frameworkImports: List[L#Import])(
      groupedRoutes: List[(List[String], List[RouteMeta])]
  )(
      protocolElems: List[StrictProtocolElems[L]],
      securitySchemes: Map[String, SecurityScheme[L]]
  )(implicit Fw: FrameworkTerms[L, F], Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], S: ServerTerms[L, F], Sw: SwaggerTerms[L, F]): F[Servers[L]] = {
    import S._
    import Sw._
    import Sc._

    for {
      extraImports       <- getExtraImports(context.tracing, supportPackage)
      supportDefinitions <- generateSupportDefinitions(context.tracing, securitySchemes)
      servers <- groupedRoutes.traverse {
        case (className, unsortedRoutes) =>
          val routes = unsortedRoutes.sortBy(r => (r.path.unwrapTracker, r.method))
          for {
            resourceName <- formatTypeName(className.lastOption.getOrElse(""), Some("Resource"))
            handlerName  <- formatTypeName(className.lastOption.getOrElse(""), Some("Handler"))
            responseServerPair <- routes.traverse {
              case route @ RouteMeta(path, method, operation, securityRequirements) =>
                for {
                  operationId           <- getOperationId(operation)
                  responses             <- Responses.getResponses(operationId, operation, protocolElems)
                  responseClsName       <- formatTypeName(operationId, Some("Response"))
                  responseDefinitions   <- generateResponseDefinitions(responseClsName, responses, protocolElems)
                  methodName            <- formatMethodName(operationId)
                  parameters            <- route.getParameters[L, F](protocolElems)
                  customExtractionField <- buildCustomExtractionFields(operation, className, context.customExtraction)
                  tracingField          <- buildTracingFields(operation, className, context.tracing)
                } yield (
                  responseDefinitions,
                  GenerateRouteMeta(operationId, methodName, responseClsName, customExtractionField, tracingField, route, parameters, responses)
                )
            }
            (responseDefinitions, serverOperations) = responseServerPair.unzip
            renderedRoutes <- generateRoutes(context.tracing, resourceName, handlerName, basePath, serverOperations, protocolElems, securitySchemes)
            handlerSrc <- renderHandler(
              handlerName,
              renderedRoutes.methodSigs,
              renderedRoutes.handlerDefinitions,
              responseDefinitions.flatten,
              context.customExtraction
            )
            extraRouteParams <- getExtraRouteParams(context.customExtraction, context.tracing)
            classSrc <- renderClass(
              resourceName,
              handlerName,
              renderedRoutes.classAnnotations,
              renderedRoutes.routes,
              extraRouteParams,
              responseDefinitions.flatten,
              renderedRoutes.supportDefinitions,
              context.customExtraction
            )
          } yield {
            Server(className, frameworkImports ++ extraImports, handlerSrc, classSrc)
          }
      }
    } yield Servers[L](servers, supportDefinitions)
  }
}
