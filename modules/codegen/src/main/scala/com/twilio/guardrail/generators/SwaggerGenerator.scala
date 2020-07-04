package com.twilio.guardrail
package generators

import cats.implicits._
import cats.Monad
import com.twilio.guardrail.core.{ Mappish, Tracker }
import com.twilio.guardrail.core.implicits._
import com.twilio.guardrail.extract.{ ClassPrefix, PackageName, SecurityOptional }
import com.twilio.guardrail.generators.syntax._
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.terms._
import io.swagger.v3.oas.models.Operation
import io.swagger.v3.oas.models.parameters.{ Parameter, RequestBody }
import java.net.URI
import scala.collection.JavaConverters._
import scala.util.Try
import io.swagger.v3.oas.models.Components
import io.swagger.v3.oas.models.security.{ SecurityScheme => SwSecurityScheme }
import io.swagger.v3.oas.models.media.{ ArraySchema, Schema }
import io.swagger.v3.oas.models.PathItem

object SwaggerGenerator {
  private def parameterSchemaType(parameter: Tracker[Parameter]): Target[Tracker[String]] = {
    val parameterName: String = Option(parameter.get.getName).fold("no name")(s => s"named: ${s}")
    for {
      schema <- parameter.downField("schema", _.getSchema).raiseErrorIfEmpty(s"Parameter (${parameterName}) has no schema")
      tpe    <- schema.downField("type", _.getType).raiseErrorIfEmpty(s"Parameter (${parameterName}) has no schema type")
    } yield tpe
  }

  def apply[L <: LA](): SwaggerTerms[L, Target] =
    new SwaggerTerms[L, Target] {
      def MonadF: Monad[Target] = Target.targetInstances
      def splitOperationParts(operationId: String): (List[String], String) = {
        val parts = operationId.split('.')
        (parts.drop(1).toList, parts.last)
      }

      def extractCommonRequestBodies(components: Option[Components]): Target[Map[String, RequestBody]] =
        Target.pure(components.flatMap(c => Option(c.getRequestBodies)).fold(Map.empty[String, RequestBody])(_.asScala.toMap))

      def extractOperations(
          paths: Tracker[Mappish[List, String, PathItem]],
          commonRequestBodies: Map[String, RequestBody],
          globalSecurityRequirements: Option[SecurityRequirements]
      ): Target[List[RouteMeta]] =
        Target.log.function("extractOperations")(for {
          _ <- Target.log.debug(s"Args: ${paths.get.value.map({ case (a, b) => (a, b.showNotNull) })} (${paths.showHistory})")
          routes <- paths.indexedCosequence.value.flatTraverse({
            case (pathStr, path) =>
              for {
                operationMap <- path
                  .downField("operations", _.readOperationsMap)
                  .toNel
                  .raiseErrorIfEmpty("No operations defined")
                operationRoutes <- operationMap.indexedCosequence.value.toList.traverse({
                  case (httpMethod, operation) =>
                    val securityRequirements =
                      operation
                        .downField("security", _.getSecurity)
                        .toNel
                        .orHistory
                        .fold(
                          _ => globalSecurityRequirements,
                          security => SecurityRequirements(security.get, SecurityOptional(operation), SecurityRequirements.Local)
                        )

                    // For some reason the 'resolve' option on the openapi parser doesn't auto-resolve
                    // requestBodies, so let's manually fix that up here.
                    val updatedOperation: Target[Tracker[Operation]] = operation
                      .downField("body", _.getRequestBody)
                      .flatDownField("$ref", _.get$ref)
                      .refine[Target[Tracker[Operation]]]({ case Some(x) => (x, x.split("/").toList) })(
                        tracker =>
                          tracker.get match {
                            case (rbref, "#" :: "components" :: "requestBodies" :: name :: Nil) =>
                              commonRequestBodies
                                .get(name)
                                .fold[Target[Tracker[Operation]]](
                                  Target.raiseUserError(s"Unable to resolve reference to '$rbref' when attempting to process ${tracker.showHistory}")
                                )({ commonRequestBody =>
                                  Target.pure(
                                    operation.map(
                                      op =>
                                        SwaggerUtil
                                          .copyOperation(op)
                                          .requestBody(SwaggerUtil.copyRequestBody(commonRequestBody))
                                    )
                                  )
                                })
                            case (rbref, _) =>
                              Target.raiseUserError(s"Invalid request body $$ref name '$rbref' when attempting to process ${tracker.showHistory}")
                          }
                      )
                      .getOrElse(Target.pure(operation))

                    updatedOperation.map(op => RouteMeta(Tracker.cloneHistory(path, pathStr), httpMethod, op, securityRequirements))
                })
              } yield operationRoutes
          })
        } yield routes)

      def extractApiKeySecurityScheme(schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type]) =
        for {
          name <- Target.fromOption(Option(securityScheme.getName), UserError(s"Security scheme ${schemeName} is an API Key scheme but has no 'name' property"))
          in   <- Target.fromOption(Option(securityScheme.getIn), UserError(s"Security scheme ${schemeName} is an API Key scheme but has no 'in' property"))
        } yield ApiKeySecurityScheme[L](name, in, tpe)

      def extractHttpSecurityScheme(schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type]) =
        for {
          authScheme <- Target.fromOption(Option(securityScheme.getScheme), UserError(s"Security scheme ${schemeName} is a HTTP scheme but has no auth scheme"))
        } yield HttpSecurityScheme[L](authScheme, tpe)

      def extractOpenIdConnectSecurityScheme(
          schemeName: String,
          securityScheme: SwSecurityScheme,
          tpe: Option[L#Type]
      ) =
        for {
          url <- Target.fromOption(
            Option(securityScheme.getOpenIdConnectUrl).flatMap(url => Try(new URI(url)).toOption),
            UserError(s"Security scheme ${schemeName} has a missing or invalid OpenID Connect URL")
          )
        } yield OpenIdConnectSecurityScheme[L](url, tpe)

      def extractOAuth2SecurityScheme(schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type]) =
        for {
          flows <- Target.fromOption(Option(securityScheme.getFlows), UserError(s"Security scheme ${schemeName} has no OAuth2 flows"))
        } yield OAuth2SecurityScheme[L](flows, tpe)

      def getClassName(operation: Tracker[Operation], vendorPrefixes: List[String]) =
        Target.log.function("getClassName")(for {
          _ <- Target.log.debug(s"Args: ${operation.get.showNotNull}")

          className = ClassPrefix(operation, vendorPrefixes) match {
            case cls @ Some(_) => cls.toList
            case None =>
              val pkg = PackageName(operation, vendorPrefixes)
                .map(_.split('.').toVector)
                .orElse({
                  operation
                    .downField("tags", _.getTags)
                    .toNel
                    .indexedCosequence
                    .map { tags =>
                      println(
                        s"Warning: Using `tags` to define package membership is deprecated in favor of the `x-jvm-package` vendor extension (${tags.history})"
                      )
                      tags.get.toList
                    }
                })
              val opPkg = operation.downField("operationId", _.getOperationId()).map(_.toList.flatMap(splitOperationParts(_)._1)).get
              pkg.map(_ ++ opPkg).getOrElse(opPkg).toList
          }
        } yield className)

      def getParameterName(parameter: Parameter) =
        Target.fromOption(Option(parameter.getName()), UserError(s"Parameter missing 'name': ${parameter}"))

      def getBodyParameterSchema(parameter: Tracker[Parameter]) =
        parameter
          .downField("schema", _.getSchema())
          .raiseErrorIfEmpty("Schema not specified")

      def getHeaderParameterType(parameter: Tracker[Parameter]) =
        parameterSchemaType(parameter)

      def getPathParameterType(parameter: Tracker[Parameter]) =
        parameterSchemaType(parameter)

      def getQueryParameterType(parameter: Tracker[Parameter]) =
        parameterSchemaType(parameter)

      def getCookieParameterType(parameter: Tracker[Parameter]) =
        parameterSchemaType(parameter)

      def getFormParameterType(parameter: Tracker[Parameter]) =
        parameterSchemaType(parameter)

      def getSerializableParameterType(parameter: Tracker[Parameter]) =
        parameterSchemaType(parameter)

      def getRefParameterRef(parameter: Tracker[Parameter]) =
        parameter
          .downField("$ref", _.get$ref())
          .map(_.flatMap(_.split("/").lastOption))
          .raiseErrorIfEmpty(s"$$ref not defined for parameter '${parameter.downField("name", _.getName()).get.getOrElse("<name missing as well>")}'")

      def fallbackParameterHandler(parameter: Tracker[Parameter]) =
        Target.raiseUserError(s"Unsure how to handle ${parameter.unwrapTracker} (${parameter.history})")

      def getOperationId(operation: Tracker[Operation]) =
        operation
          .downField("operationId", _.getOperationId())
          .map(_.map(splitOperationParts(_)._2))
          .raiseErrorIfEmpty("Missing operationId")
          .map(_.get)

      def getResponses(operationId: String, operation: Tracker[Operation]) =
        operation.downField("responses", _.getResponses).toNel.raiseErrorIfEmpty(s"No responses defined for ${operationId}").map(_.indexedCosequence.value)

      def getSimpleRef(ref: Tracker[Option[Schema[_]]]) =
        ref
          .flatDownField("$ref", _.get$ref)
          .map(_.flatMap(_.split("/").lastOption))
          .raiseErrorIfEmpty(s"Unspecified $$ref")
          .map(_.unwrapTracker)

      def getItems(arr: Tracker[ArraySchema]) =
        arr
          .downField("items", _.getItems())
          .raiseErrorIfEmpty("Unspecified items")

      def getType(model: Tracker[Schema[_]]) =
        model
          .downField("type", _.getType())
          .raiseErrorIfEmpty("Unknown type")

      def fallbackPropertyTypeHandler(prop: Tracker[Schema[_]]) = {
        val determinedType = prop.downField("type", _.getType()).fold("No type definition")(s => s"type: ${s.unwrapTracker}")
        val className      = prop.unwrapTracker.getClass.getName
        Target.raiseUserError(
          s"""|Unknown type for the following structure (${determinedType}, class: ${className}, ${prop.showHistory}):
              |  ${prop.toString().linesIterator.filterNot(_.contains(": null")).mkString("\n  ")}
              |""".stripMargin
        )
      }

      def resolveType(name: String, protocolElems: List[StrictProtocolElems[L]]) =
        Target.fromOption(protocolElems.find(_.name == name), UserError(s"Unable to resolve ${name}"))

      def fallbackResolveElems(lazyElems: List[LazyProtocolElems[L]]) =
        Target.raiseUserError(s"Unable to resolve: ${lazyElems.map(_.name)}")
      def log: SwaggerLogAdapter[Target] = new SwaggerLogAdapter[Target] {
        def function[A](name: String): Target[A] => Target[A] = Target.log.function(name)
        def push(name: String): Target[Unit]                  = Target.log.push(name)
        def pop: Target[Unit]                                 = Target.log.pop
        def debug(message: String): Target[Unit]              = Target.log.debug(message)
        def info(message: String): Target[Unit]               = Target.log.info(message)
        def warning(message: String): Target[Unit]            = Target.log.warning(message)
        def error(message: String): Target[Unit]              = Target.log.error(message)
      }
    }
}
