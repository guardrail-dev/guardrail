package dev.guardrail.generators

import cats.Monad
import cats.syntax.all._
import io.swagger.v3.oas.models.Components
import io.swagger.v3.oas.models.Operation
import io.swagger.v3.oas.models.PathItem
import io.swagger.v3.oas.models.media.{ ArraySchema, Schema }
import io.swagger.v3.oas.models.parameters.{ Parameter, RequestBody }
import io.swagger.v3.oas.models.security.{ SecurityScheme => SwSecurityScheme }
import java.net.URI
import scala.util.Try

import dev.guardrail._
import dev.guardrail.core.extract.{ ClassPrefix, PackageName, SecurityOptional }
import dev.guardrail.core.implicits._
import dev.guardrail.core.{ Mappish, Tracker }
import dev.guardrail.generators.syntax._
import dev.guardrail.languages.LA
import dev.guardrail.terms._
import dev.guardrail.terms.protocol._

object SwaggerGenerator {
  def apply[L <: LA](): SwaggerTerms[L, Target] =
    new SwaggerGenerator[L]
}

class SwaggerGenerator[L <: LA] extends SwaggerTerms[L, Target] {
  override def MonadF: Monad[Target] = Target.targetInstances

  private def parameterSchemaType(parameter: Tracker[Parameter]): Target[Tracker[String]] = {
    val parameterName: String = parameter.downField("name", _.getName).unwrapTracker.fold("no name")(s => s"named: ${s}")
    for {
      schema <- parameter.downField("schema", _.getSchema).raiseErrorIfEmpty(s"Parameter (${parameterName}) has no schema")
      tpe    <- schema.downField("type", _.getType).raiseErrorIfEmpty(s"Parameter (${parameterName}) has no schema type")
    } yield tpe
  }

  private def splitOperationParts(operationId: String): (List[String], String) = {
    val parts = operationId.split('.')
    (parts.drop(1).toList, parts.last)
  }

  override def extractCommonRequestBodies(components: Tracker[Option[Components]]): Target[Map[String, RequestBody]] =
    Target.pure(components.flatDownField("requestBodies", _.getRequestBodies).unwrapTracker.value.toMap)

  override def extractEnum(enumSchema: Tracker[EnumSchema]) = {
    type EnumValues[A] = (String, Option[String]) => List[A] => Either[String, HeldEnum]
    implicit val wrapNumberValues: EnumValues[Number] = {
      case (tpe, fmt) =>
        Option(_)
          .filterNot(_.isEmpty)
          .toRight("Model has no enumerations")
          .map(
            xs =>
              (tpe, fmt) match {
                case ("integer", None)          => IntHeldEnum(xs)
                case ("integer", Some("int32")) => IntHeldEnum(xs)
                case ("integer", Some("int64")) => LongHeldEnum(xs)
                case _                          => StringHeldEnum.fromNumbers(xs) // TODO: Preserve previous behaviour if we don't get a match
              }
          )
    }
    implicit val wrapStringValues: EnumValues[String] = (_, _) => Option(_).filterNot(_.isEmpty).toRight("Model has no enumerations").map(StringHeldEnum.apply)

    def poly[A, B](schema: Schema[A])(translate: A => B)(implicit ev: EnumValues[B]): Tracker[Either[String, HeldEnum]] = {
      val t       = Tracker.cloneHistory(enumSchema, schema)
      val tpeName = t.downField("type", _.getType()).map(_.filterNot(_ == "object").getOrElse("string")).unwrapTracker
      val format  = t.downField("format", _.getFormat()).unwrapTracker

      t.downField("enum", _.getEnum()).map(_.map(translate)).map(ev(tpeName, format))
    }
    val enumEntries: Tracker[Either[String, HeldEnum]] = enumSchema.unwrapTracker match {
      case NumberEnumSchema(value) => poly(value)(identity _)
      case ObjectEnumSchema(value) => poly(value)(_.toString())
      case StringEnumSchema(value) => poly(value)(identity _)
    }
    Target.pure(enumEntries.unwrapTracker)
  }

  override def extractOperations(
      paths: Tracker[Mappish[List, String, PathItem]],
      commonRequestBodies: Map[String, RequestBody],
      globalSecurityRequirements: Option[SecurityRequirements]
  ): Target[List[RouteMeta]] =
    Target.log.function("extractOperations")(for {
      _ <- Target.log.debug(s"Args: ${paths.unwrapTracker.value.map({ case (a, b) => (a, b.showNotNull) })} (${paths.showHistory})")
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
                      security => SecurityRequirements(security.unwrapTracker, SecurityOptional(operation), SecurityRequirements.Local)
                    )

                // For some reason the 'resolve' option on the openapi parser doesn't auto-resolve
                // requestBodies, so let's manually fix that up here.
                val updatedOperation: Target[Tracker[Operation]] = operation
                  .downField("body", _.getRequestBody)
                  .flatDownField("$ref", _.get$ref)
                  .refine[Target[Tracker[Operation]]]({ case Some(x) => (x, x.split("/").toList) })(
                    tracker =>
                      tracker.unwrapTracker match {
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

  override def extractApiKeySecurityScheme(schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type]) =
    for {
      name <- Target.fromOption(Option(securityScheme.getName), UserError(s"Security scheme ${schemeName} is an API Key scheme but has no 'name' property"))
      in   <- Target.fromOption(Option(securityScheme.getIn), UserError(s"Security scheme ${schemeName} is an API Key scheme but has no 'in' property"))
    } yield ApiKeySecurityScheme[L](name, in, tpe)

  override def extractHttpSecurityScheme(schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type]) =
    for {
      authScheme <- Target.fromOption(Option(securityScheme.getScheme), UserError(s"Security scheme ${schemeName} is a HTTP scheme but has no auth scheme"))
    } yield HttpSecurityScheme[L](authScheme, tpe)

  override def extractOpenIdConnectSecurityScheme(
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

  override def extractOAuth2SecurityScheme(schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type]) =
    for {
      flows <- Target.fromOption(Option(securityScheme.getFlows), UserError(s"Security scheme ${schemeName} has no OAuth2 flows"))
    } yield OAuth2SecurityScheme[L](flows, tpe)

  override def getClassName(operation: Tracker[Operation], vendorPrefixes: List[String]) =
    Target.log.function("getClassName")(for {
      _ <- Target.log.debug(s"Args: ${operation.unwrapTracker.showNotNull}")

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
                    s"Warning: Using `tags` to define package membership is deprecated in favor of the `x-jvm-package` vendor extension (${tags.showHistory})"
                  )
                  tags.unwrapTracker.toList
                }
            })
          val opPkg = operation.downField("operationId", _.getOperationId()).map(_.toList.flatMap(splitOperationParts(_)._1)).unwrapTracker
          pkg.fold(opPkg)(_.toList ++ opPkg)
      }
    } yield className)

  override def getParameterName(parameter: Tracker[Parameter]) =
    parameter
      .downField("name", _.getName())
      .raiseErrorIfEmpty("Name not specified")
      .map(_.unwrapTracker)

  override def getBodyParameterSchema(parameter: Tracker[Parameter]) =
    parameter
      .downField("schema", _.getSchema())
      .raiseErrorIfEmpty("Schema not specified")

  override def getHeaderParameterType(parameter: Tracker[Parameter]) =
    parameterSchemaType(parameter)

  override def getPathParameterType(parameter: Tracker[Parameter]) =
    parameterSchemaType(parameter)

  override def getQueryParameterType(parameter: Tracker[Parameter]) =
    parameterSchemaType(parameter)

  override def getCookieParameterType(parameter: Tracker[Parameter]) =
    parameterSchemaType(parameter)

  override def getFormParameterType(parameter: Tracker[Parameter]) =
    parameterSchemaType(parameter)

  override def getSerializableParameterType(parameter: Tracker[Parameter]) =
    parameterSchemaType(parameter)

  override def getRefParameterRef(parameter: Tracker[Parameter]) =
    parameter
      .downField("$ref", _.get$ref())
      .map(_.flatMap(_.split("/").lastOption))
      .raiseErrorIfEmpty(s"$$ref not defined for parameter '${parameter.downField("name", _.getName()).unwrapTracker.getOrElse("<name missing as well>")}'")

  override def fallbackParameterHandler(parameter: Tracker[Parameter]) =
    Target.raiseUserError(s"Unsure how to handle ${parameter.unwrapTracker} (${parameter.showHistory})")

  override def getOperationId(operation: Tracker[Operation]) =
    operation
      .downField("operationId", _.getOperationId())
      .map(_.map(splitOperationParts(_)._2))
      .raiseErrorIfEmpty("Missing operationId")
      .map(_.unwrapTracker)

  override def getResponses(operationId: String, operation: Tracker[Operation]) =
    operation.downField("responses", _.getResponses).toNel.raiseErrorIfEmpty(s"No responses defined for ${operationId}").map(_.indexedCosequence.value)

  override def getSimpleRef(ref: Tracker[Option[Schema[_]]]) =
    ref
      .flatDownField("$ref", _.get$ref)
      .map(_.flatMap(_.split("/").lastOption))
      .raiseErrorIfEmpty(s"Unspecified $$ref")
      .map(_.unwrapTracker)

  override def getItems(arr: Tracker[ArraySchema]) =
    arr
      .downField("items", _.getItems())
      .raiseErrorIfEmpty("Unspecified items")

  override def getType(model: Tracker[Schema[_]]) =
    model
      .downField("type", _.getType())
      .raiseErrorIfEmpty("Unknown type")

  override def fallbackPropertyTypeHandler(prop: Tracker[Schema[_]]) = {
    val determinedType = prop.downField("type", _.getType()).fold("No type definition")(s => s"type: ${s.unwrapTracker}")
    val className      = prop.unwrapTracker.getClass.getName
    Target.raiseUserError(
      s"""|Unknown type for the following structure (${determinedType}, class: ${className}, ${prop.showHistory}):
          |  ${prop.toString().linesIterator.filterNot(_.contains(": null")).mkString("\n  ")}
          |""".stripMargin
    )
  }

  override def resolveType(name: String, protocolElems: List[StrictProtocolElems[L]]) =
    Target.fromOption(protocolElems.find(_.name == name), UserError(s"Unable to resolve ${name}"))

  override def fallbackResolveElems(lazyElems: List[LazyProtocolElems[L]]) =
    Target.raiseUserError(s"Unable to resolve: ${lazyElems.map(_.name)}")
  override def log: SwaggerLogAdapter[Target] = new SwaggerLogAdapter[Target] {
    def function[A](name: String): Target[A] => Target[A] = Target.log.function(name)
    def push(name: String): Target[Unit]                  = Target.log.push(name)
    def pop: Target[Unit]                                 = Target.log.pop
    def debug(message: String): Target[Unit]              = Target.log.debug(message)
    def info(message: String): Target[Unit]               = Target.log.info(message)
    def warning(message: String): Target[Unit]            = Target.log.warning(message)
    def error(message: String): Target[Unit]              = Target.log.error(message)
  }
}
