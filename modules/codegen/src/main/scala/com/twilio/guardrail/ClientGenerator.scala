package com.twilio.guardrail

import cats.data.NonEmptyList
import cats.free.Free
import cats.implicits._
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.protocol.terms.client.ClientTerms
import com.twilio.guardrail.protocol.terms.Responses
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.terms.{ RouteMeta, ScalaTerms, SecurityScheme, SwaggerTerms }
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
  )(implicit C: ClientTerms[L, Free[F, ?]], Fw: FrameworkTerms[L, F], Sc: ScalaTerms[L, Free[F, ?]], Sw: SwaggerTerms[L, F]): Free[F, Clients[L]] = {
    import C._
    import Sw._
    for {
      clientImports      <- getImports(context.tracing)
      clientExtraImports <- getExtraImports(context.tracing)
      supportDefinitions <- generateSupportDefinitions(context.tracing, securitySchemes)
      clients <- groupedRoutes.traverse({
        case (className, unsortedRoutes) =>
          val routes     = unsortedRoutes.sortBy(r => (r.path.unwrapTracker, r.method))
          val clientName = s"${className.lastOption.getOrElse("").capitalize}Client"
          def splitOperationParts(operationId: String): (List[String], String) = {
            val parts = operationId.split('.')
            (parts.drop(1).toList, parts.last)
          }

          for {
            responseClientPair <- routes.traverse {
              case route @ RouteMeta(path, method, operation, securityRequirements) =>
                for {
                  operationId         <- getOperationId(operation)
                  responses           <- Responses.getResponses[L, F](operationId, operation, protocolElems)
                  responseDefinitions <- generateResponseDefinitions(operationId, responses, protocolElems)
                  parameters          <- route.getParameters[L, F](protocolElems)
                  clientOp            <- generateClientOperation(className, context.tracing, securitySchemes, parameters)(route, operationId, responses)
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
