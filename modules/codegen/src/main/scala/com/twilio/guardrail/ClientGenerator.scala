package com.twilio.guardrail

import cats.data.NonEmptyList
import cats.implicits._
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.protocol.terms.client.ClientTerms
import com.twilio.guardrail.protocol.terms.Responses
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.terms.{ LanguageTerms, RouteMeta, SecurityScheme, SwaggerTerms }
import java.net.URI

case class Clients[L <: LA](clients: List[Client[L]], supportDefinitions: List[SupportDefinition[L]])
case class Client[L <: LA](
    pkg: List[String],
    clientName: String,
    imports: List[L#Import],
    staticDefns: StaticDefns[L],
    client: NonEmptyList[Either[L#Trait, L#ClassDefinition]],
    responseDefinitions: List[L#Definition]
)
case class RenderedClientOperation[L <: LA](
    clientOperation: L#Definition,
    supportDefinitions: List[L#Definition]
)

object ClientGenerator {
  def fromSwagger[L <: LA, F[_]](context: Context, frameworkImports: List[L#Import])(
      serverUrls: Option[NonEmptyList[URI]],
      basePath: Option[String],
      groupedRoutes: List[(List[String], List[RouteMeta])]
  )(
      protocolElems: List[StrictProtocolElems[L]],
      securitySchemes: Map[String, SecurityScheme[L]]
  )(implicit C: ClientTerms[L, F], Fw: FrameworkTerms[L, F], Sc: LanguageTerms[L, F], Sw: SwaggerTerms[L, F]): F[Clients[L]] = {
    import C._
    import Sw._
    import Sc._
    for {
      clientImports      <- getImports(context.tracing)
      clientExtraImports <- getExtraImports(context.tracing)
      supportDefinitions <- generateSupportDefinitions(context.tracing, securitySchemes)
      clients <- groupedRoutes.traverse({
        case (className, unsortedRoutes) =>
          val routes = unsortedRoutes.sortBy(r => (r.path.unwrapTracker, r.method))
          for {
            clientName <- formatTypeName(className.lastOption.getOrElse(""), Some("Client"))
            responseClientPair <- routes.traverse {
              case route @ RouteMeta(path, method, operation, securityRequirements) =>
                for {
                  operationId         <- getOperationId(operation)
                  responses           <- Responses.getResponses[L, F](operationId, operation, protocolElems)
                  responseClsName     <- formatTypeName(operationId, Some("Response"))
                  responseDefinitions <- generateResponseDefinitions(responseClsName, responses, protocolElems)
                  parameters          <- route.getParameters[L, F](protocolElems)
                  methodName          <- formatMethodName(operationId)
                  clientOp            <- generateClientOperation(className, responseClsName, context.tracing, securitySchemes, parameters)(route, methodName, responses)
                } yield (responseDefinitions, clientOp)
            }
            (responseDefinitions, clientOperations) = responseClientPair.unzip
            tracingName                             = Option(className.mkString("-")).filterNot(_.isEmpty)
            ctorArgs    <- clientClsArgs(tracingName, serverUrls, context.tracing)
            staticDefns <- buildStaticDefns(clientName, tracingName, serverUrls, ctorArgs, context.tracing)
            client <- buildClient(
              clientName,
              tracingName,
              serverUrls,
              basePath,
              ctorArgs,
              clientOperations.map(_.clientOperation),
              clientOperations.flatMap(_.supportDefinitions),
              context.tracing
            )
          } yield {
            Client[L](className, clientName, (clientImports ++ frameworkImports ++ clientExtraImports), staticDefns, client, responseDefinitions.flatten)
          }
      })
    } yield Clients[L](clients, supportDefinitions)
  }
}
