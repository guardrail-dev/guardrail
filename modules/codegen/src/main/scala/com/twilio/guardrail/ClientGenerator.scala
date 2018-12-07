package com.twilio.guardrail

import _root_.io.swagger.models._
import cats.free.Free
import cats.instances.all._
import cats.syntax.all._
import com.twilio.guardrail.protocol.terms.client.ClientTerms
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.generators.Http4sHelper
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.terms.{ RouteMeta, ScalaTerms, SwaggerTerms }

import scala.collection.JavaConverters._

case class Clients[L <: LA](clients: List[Client[L]])
case class Client[L <: LA](pkg: List[String],
                           clientName: String,
                           imports: List[L#Import],
                           companion: L#ObjectDefinition,
                           client: L#ClassDefinition,
                           responseDefinitions: List[L#Definition])
case class RenderedClientOperation[L <: LA](
    clientOperation: L#Definition,
    supportDefinitions: List[L#Definition]
)

object ClientGenerator {
  def fromSwagger[L <: LA, F[_]](context: Context, frameworkImports: List[L#Import])(
      schemes: List[String],
      host: Option[String],
      basePath: Option[String],
      groupedRoutes: List[(List[String], List[RouteMeta])]
  )(protocolElems: List[StrictProtocolElems[L]])(implicit C: ClientTerms[L, F], Fw: FrameworkTerms[L, F], Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F]): Free[F, Clients[L]] = {
    import C._
    import Sw._
    for {
      clientImports      <- getImports(context.tracing)
      clientExtraImports <- getExtraImports(context.tracing)
      clients <- groupedRoutes.traverse({
        case (className, routes) =>
          val clientName = s"${className.lastOption.getOrElse("").capitalize}Client"
          def splitOperationParts(operationId: String): (List[String], String) = {
            val parts = operationId.split('.')
            (parts.drop(1).toList, parts.last)
          }

          for {
            responseClientPair <- routes.traverse {
              case route @ RouteMeta(path, method, operation) =>
                for {
                  operationId         <- getOperationId(operation)
                  responses           <- Http4sHelper.getResponsesF[L, F](operationId, operation, protocolElems)
                  responseDefinitions <- generateResponseDefinitions(operationId, responses, protocolElems)
                  parameters          <- route.getParametersF[L, F](protocolElems)
                  clientOp            <- generateClientOperation(className, context.tracing, parameters)(route, operationId, responses)
                } yield (responseDefinitions, clientOp)
            }
            (responseDefinitions, clientOperations) = responseClientPair.unzip
            tracingName                             = Option(className.mkString("-")).filterNot(_.isEmpty)
            ctorArgs  <- clientClsArgs(tracingName, schemes, host, context.tracing)
            companion <- buildCompanion(clientName, tracingName, schemes, host, ctorArgs, context.tracing)
            client <- buildClient(
              clientName,
              tracingName,
              schemes,
              host,
              basePath,
              ctorArgs,
              clientOperations.map(_.clientOperation),
              clientOperations.flatMap(_.supportDefinitions),
              context.tracing
            )
          } yield {
            Client[L](className, clientName, (clientImports ++ frameworkImports ++ clientExtraImports), companion, client, responseDefinitions.flatten)
          }
      })
    } yield Clients[L](clients)
  }
}
