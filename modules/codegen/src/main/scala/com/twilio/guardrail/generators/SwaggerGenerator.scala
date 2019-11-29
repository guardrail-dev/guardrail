package com.twilio.guardrail
package generators

import cats.implicits._
import cats.~>
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.core.implicits._
import com.twilio.guardrail.extract.{ PackageName, SecurityOptional }
import com.twilio.guardrail.generators.syntax._
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.terms._
import io.swagger.v3.oas.models.Operation
import io.swagger.v3.oas.models.parameters.{ Parameter, RequestBody }
import java.net.URI
import scala.collection.JavaConverters._
import scala.util.Try

object SwaggerGenerator {
  private def parameterSchemaType(parameter: Tracker[Parameter]): Target[Tracker[String]] =
    for {
      schema <- parameter.downField("schema", _.getSchema).raiseErrorIfEmpty("Parameter has no schema")
      tpe    <- schema.downField("type", _.getType).raiseErrorIfEmpty("Parameter has no schema type")
    } yield tpe

  def apply[L <: LA]() = new (SwaggerTerm[L, ?] ~> Target) {
    def splitOperationParts(operationId: String): (List[String], String) = {
      val parts = operationId.split('.')
      (parts.drop(1).toList, parts.last)
    }

    def apply[T](term: SwaggerTerm[L, T]): Target[T] = term match {
      case ExtractCommonRequestBodies(components) =>
        Target.pure(components.flatMap(c => Option(c.getRequestBodies)).fold(Map.empty[String, RequestBody])(_.asScala.toMap))

      case ExtractOperations(paths, commonRequestBodies, globalSecurityRequirements) =>
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
                                  Target.raiseError(s"Unable to resolve reference to '$rbref' when attempting to process ${tracker.showHistory}")
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
                              Target.raiseError(s"Invalid request body $$ref name '$rbref' when attempting to process ${tracker.showHistory}")
                          }
                      )
                      .getOrElse(Target.pure(operation))

                    updatedOperation.map(op => RouteMeta(Tracker.cloneHistory(path, pathStr), httpMethod, op, securityRequirements))
                })
              } yield operationRoutes
          })
        } yield routes)

      case ExtractApiKeySecurityScheme(schemeName, securityScheme, tpe) =>
        for {
          name <- Target.fromOption(Option(securityScheme.getName), s"Security scheme ${schemeName} is an API Key scheme but has no 'name' property")
          in   <- Target.fromOption(Option(securityScheme.getIn), s"Security scheme ${schemeName} is an API Key scheme but has no 'in' property")
        } yield ApiKeySecurityScheme[L](name, in, tpe)

      case ExtractHttpSecurityScheme(schemeName, securityScheme, tpe) =>
        for {
          authScheme <- Target.fromOption(Option(securityScheme.getScheme), s"Security scheme ${schemeName} is a HTTP scheme but has no auth scheme")
        } yield HttpSecurityScheme[L](authScheme, tpe)

      case ExtractOpenIdConnectSecurityScheme(schemeName, securityScheme, tpe) =>
        for {
          url <- Target.fromOption(
            Option(securityScheme.getOpenIdConnectUrl).flatMap(url => Try(new URI(url)).toOption),
            s"Security scheme ${schemeName} has a missing or invalid OpenID Connect URL"
          )
        } yield OpenIdConnectSecurityScheme[L](url, tpe)

      case ExtractOAuth2SecurityScheme(schemeName, securityScheme, tpe) =>
        for {
          flows <- Target.fromOption(Option(securityScheme.getFlows), s"Security scheme ${schemeName} has no OAuth2 flows")
        } yield OAuth2SecurityScheme[L](flows, tpe)

      case GetClassName(operation, vendorPrefixes) =>
        Target.log.function("getClassName")(for {
          _ <- Target.log.debug(s"Args: ${operation.get.showNotNull}")

          pkg = PackageName(operation, vendorPrefixes)
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
          opPkg     = operation.downField("operationId", _.getOperationId()).map(_.toList.flatMap(splitOperationParts(_)._1)).get
          className = pkg.map(_ ++ opPkg).getOrElse(opPkg).toList
        } yield className)

      case GetParameterName(parameter) =>
        Target.fromOption(Option(parameter.getName()), s"Parameter missing 'name': ${parameter}")

      case GetBodyParameterSchema(parameter) =>
        parameter
          .downField("schema", _.getSchema())
          .raiseErrorIfEmpty("Schema not specified")

      case GetHeaderParameterType(parameter) =>
        parameterSchemaType(parameter)

      case GetPathParameterType(parameter) =>
        parameterSchemaType(parameter)

      case GetQueryParameterType(parameter) =>
        parameterSchemaType(parameter)

      case GetCookieParameterType(parameter) =>
        parameterSchemaType(parameter)

      case GetFormParameterType(parameter) =>
        parameterSchemaType(parameter)

      case GetSerializableParameterType(parameter) =>
        parameterSchemaType(parameter)

      case GetRefParameterRef(parameter) =>
        parameter
          .downField("$ref", _.get$ref())
          .map(_.flatMap(_.split("/").lastOption))
          .raiseErrorIfEmpty(s"$$ref not defined for parameter '${parameter.downField("name", _.getName()).get.getOrElse("<name missing as well>")}'")

      case FallbackParameterHandler(parameter) =>
        Target.raiseError(s"Unsure how to handle ${parameter.unwrapTracker} (${parameter.history})")

      case GetOperationId(operation) =>
        operation
          .downField("operationId", _.getOperationId())
          .map(_.map(splitOperationParts(_)._2))
          .raiseErrorIfEmpty("Missing operationId")
          .map(_.get)

      case GetResponses(operationId, operation) =>
        operation.downField("responses", _.getResponses).toNel.raiseErrorIfEmpty(s"No responses defined for ${operationId}").map(_.indexedCosequence.value)

      case GetSimpleRef(ref) =>
        ref
          .flatDownField("$ref", _.get$ref)
          .map(_.flatMap(_.split("/").lastOption))
          .raiseErrorIfEmpty(s"Unspecified $$ref")
          .map(_.unwrapTracker)

      case GetItems(arr) =>
        arr
          .downField("items", _.getItems())
          .raiseErrorIfEmpty("Unspecified items")

      case GetType(model) =>
        model
          .downField("type", _.getType())
          .raiseErrorIfEmpty("Unknown type")

      case FallbackPropertyTypeHandler(prop) =>
        val determinedType = Option(prop.getType()).fold("No type definition")(s => s"type: $s")
        val className      = prop.getClass.getName
        Target.raiseError(
          s"""|Unknown type for the following structure (${determinedType}, class: ${className}):
              |  ${prop.toString().linesIterator.filterNot(_.contains(": null")).mkString("\n  ")}
              |""".stripMargin
        )

      case ResolveType(name, protocolElems) =>
        Target.fromOption(protocolElems.find(_.name == name), s"Unable to resolve ${name}")

      case FallbackResolveElems(lazyElems) =>
        Target.raiseError(s"Unable to resolve: ${lazyElems.map(_.name)}")

      case LogPush(name) =>
        Target.log.push(name)

      case LogPop() =>
        Target.log.pop

      case LogDebug(message) =>
        Target.log.debug(message)

      case LogInfo(message) =>
        Target.log.info(message)

      case LogWarning(message) =>
        Target.log.warning(message)

      case LogError(message) =>
        Target.log.error(message)
    }
  }
}
