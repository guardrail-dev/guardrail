package com.twilio.guardrail
package generators

import cats.implicits._
import cats.~>
import com.twilio.guardrail.extract.{ PackageName, SecurityOptional }
import com.twilio.guardrail.generators.syntax.RichSchema
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.terms._
import io.swagger.v3.oas.models.Operation
import io.swagger.v3.oas.models.parameters.{ Parameter, RequestBody }
import java.net.URI
import scala.collection.JavaConverters._
import scala.util.Try

object SwaggerGenerator {
  private def parameterSchemaType(parameter: Parameter): Target[String] =
    for {
      schema <- Target.fromOption(Option(parameter.getSchema), s"Parameter '${parameter.getName}' has no schema")
      tpe    <- Target.fromOption(Option(schema.getType), s"Parameter '${parameter.getName}' has no schema type")
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
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"extractOperations(${paths})")
          routes <- paths.traverse({
            case (pathStr, path) =>
              for {
                operationMap <- Target.fromOption(Option(path.readOperationsMap()), "No operations defined")
                operationRoutes <- operationMap.asScala.toList.traverse({
                  case (httpMethod, operation) =>
                    val securityRequirements = Option(operation.getSecurity)
                      .flatMap(SecurityRequirements(_, SecurityOptional(operation), SecurityRequirements.Local))
                      .orElse(globalSecurityRequirements)

                    // For some reason the 'resolve' option on the openapi parser doesn't auto-resolve
                    // requestBodies, so let's manually fix that up here.
                    val updatedOperation = Option(operation.getRequestBody)
                      .flatMap(rb => Option(rb.get$ref))
                      .flatMap(rbref => Option(rbref.split("/")).map(_.toList))
                      .fold(Target.pure(operation))({
                        case refName @ "#" :: "components" :: "requestBodies" :: name :: Nil =>
                          commonRequestBodies
                            .get(name)
                            .fold(
                              Target.raiseError[Operation](s"Unable to resolve reference to '$refName'")
                            )({ commonRequestBody =>
                              Target.pure(
                                SwaggerUtil
                                  .copyOperation(operation)
                                  .requestBody(SwaggerUtil.copyRequestBody(commonRequestBody))
                              )
                            })
                        case x =>
                          Target.raiseError[Operation](s"Invalid request body $$ref name '$x'")
                      })

                    updatedOperation.map(op => RouteMeta(pathStr, httpMethod, op, securityRequirements))
                })
              } yield operationRoutes
          })
        } yield routes.flatten

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
        for {
          _ <- Target.log.debug("SwaggerGenerator", "swagger")(s"getClassName(${operation})")

          pkg = PackageName(operation, vendorPrefixes)
            .map(_.split('.').toVector)
            .orElse({
              Option(operation.getTags).map { tags =>
                println(s"Warning: Using `tags` to define package membership is deprecated in favor of the `x-jvm-package` vendor extension")
                tags.asScala
              }
            })
            .map(_.toList)
          opPkg = Option(operation.getOperationId())
            .map(splitOperationParts)
            .fold(List.empty[String])(_._1)
          className = pkg.map(_ ++ opPkg).getOrElse(opPkg)
        } yield className

      case GetParameterName(parameter) =>
        Target.fromOption(Option(parameter.getName()), s"Parameter missing 'name': ${parameter}")

      case GetBodyParameterSchema(parameter) =>
        Target.fromOption(Option(parameter.getSchema()), s"Schema not specified for parameter '${parameter.getName}'")

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
        Target.fromOption(Option(parameter.get$ref).flatMap(_.split("/").lastOption), s"$$ref not defined for parameter '${parameter.getName}'")

      case FallbackParameterHandler(parameter) =>
        Target.raiseError(s"Unsure how to handle ${parameter}")

      case GetOperationId(operation) =>
        Target.fromOption(Option(operation.getOperationId())
                            .map(splitOperationParts)
                            .map(_._2),
                          "Missing operationId")

      case GetResponses(operationId, operation) =>
        Target.fromOption(Option(operation.getResponses).map(_.asScala.toMap), s"No responses defined for ${operationId}")

      case GetSimpleRef(ref) =>
        Target.fromOption(Option(ref.get$ref).flatMap(_.split("/").lastOption), s"Unspecified $ref")

      case GetItems(arr) =>
        Target.fromOption(Option(arr.getItems()), "items.type unspecified")

      case GetType(model) =>
        val determinedType = Option(model.getType()).fold("No type definition")(s => s"type: $s")
        val className      = model.getClass.getName
        Target.fromOption(
          Option(model.getType()),
          s"""|Unknown type for the following structure (${determinedType}, class: ${className}):
              |  ${model.showNotNullIndented(1)}
              |""".stripMargin
        )

      case FallbackPropertyTypeHandler(prop) =>
        val determinedType = Option(prop.getType()).fold("No type definition")(s => s"type: $s")
        val className      = prop.getClass.getName
        Target.raiseError(
          s"""|Unknown type for the following structure (${determinedType}, class: ${className}):
              |  ${prop.toString().lines.filterNot(_.contains(": null")).mkString("\n  ")}
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
        Target.log.debug(message).apply

      case LogInfo(message) =>
        Target.log.info(message).apply

      case LogWarning(message) =>
        Target.log.warning(message).apply

      case LogError(message) =>
        Target.log.error(message).apply
    }
  }
}
