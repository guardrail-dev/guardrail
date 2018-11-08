package com.twilio.guardrail

import _root_.io.swagger.models._
import cats.free.Free
import cats.instances.all._
import cats.syntax.all._
import com.twilio.guardrail.protocol.terms.client.ClientTerms
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.languages.LA

import scala.collection.JavaConverters._
import com.twilio.guardrail.terms.RouteMeta

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
  )(protocolElems: List[StrictProtocolElems])(implicit C: ClientTerms[L, F]): Free[F, Clients[L]] = {
    import C._
    for {
      clientImports      <- getImports(context.tracing)
      clientExtraImports <- getExtraImports(context.tracing)
      clients <- groupedRoutes.traverse({
        case (pkg, routes) =>
          for {
            responseDefinitions <- routes.flatTraverse {
              case RouteMeta(path, method, operation) =>
                generateResponseDefinitions(operation, protocolElems)
            }
            clientOperations <- routes.traverse(generateClientOperation(pkg, context.tracing, protocolElems) _)
            clientName  = s"${pkg.lastOption.getOrElse("").capitalize}Client"
            tracingName = Option(pkg.mkString("-")).filterNot(_.isEmpty)
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
            Client[L](pkg, clientName, (clientImports ++ frameworkImports ++ clientExtraImports), companion, client, responseDefinitions)
          }
      })
    } yield Clients[L](clients)
  }
}
