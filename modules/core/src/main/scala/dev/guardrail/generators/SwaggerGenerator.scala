package dev.guardrail.generators

import cats.Monad
import cats.syntax.all._
import io.swagger.v3.oas.models.Components
import io.swagger.v3.oas.models.Operation
import io.swagger.v3.oas.models.PathItem
import io.swagger.v3.oas.models.headers
import io.swagger.v3.oas.models.media.{ ArraySchema, Schema }
import io.swagger.v3.oas.models.parameters.{ Parameter, RequestBody }
import io.swagger.v3.oas.models.responses.ApiResponse
import io.swagger.v3.oas.models.security.{ SecurityRequirement, SecurityScheme => SwSecurityScheme }
import java.{ util => ju }
import java.net.URI
import scala.util.Try

import dev.guardrail._
import dev.guardrail.core.extract.{ ClassPrefix, PackageName }
import dev.guardrail.core.implicits._
import dev.guardrail.core.{ Mappish, Tracker }
import dev.guardrail.generators.syntax._
import dev.guardrail.languages.LA
import dev.guardrail.terms._
import dev.guardrail.terms.protocol._
import cats.data.NonEmptyList

object SwaggerGenerator {
  def apply[L <: LA](): SwaggerTerms[L, Target] =
    new SwaggerGenerator[L]
}

class SwaggerGenerator[L <: LA] extends SwaggerTerms[L, Target] {
  override def MonadF: Monad[Target] = Target.targetInstances

  private def splitOperationParts(operationId: String): (List[String], String) = {
    val parts = operationId.split('.')
    (parts.drop(1).toList, parts.last)
  }

  private def copyOperation(operation: Operation): Operation =
    new Operation()
      .tags(operation.getTags)
      .summary(operation.getSummary)
      .description(operation.getDescription)
      .externalDocs(operation.getExternalDocs)
      .operationId(operation.getOperationId)
      .parameters(operation.getParameters)
      .requestBody(operation.getRequestBody)
      .responses(operation.getResponses)
      .callbacks(operation.getCallbacks)
      .deprecated(operation.getDeprecated)
      .security(operation.getSecurity)
      .servers(operation.getServers)
      .extensions(operation.getExtensions)

  def copyRequestBody(requestBody: RequestBody): RequestBody =
    new RequestBody()
      .description(requestBody.getDescription)
      .content(requestBody.getContent)
      .required(requestBody.getRequired)
      .$ref(requestBody.get$ref())
      .extensions(requestBody.getExtensions)

  override def extractCommonRequestBodies(components: Tracker[Option[Components]]): Target[Map[String, RequestBody]] =
    Target.pure(components.flatDownField("requestBodies", _.getRequestBodies).unwrapTracker.value.toMap)

  override def extractEnum(enumSchema: Tracker[EnumSchema]) = {
    type EnumValues[A] = (String, Option[String]) => List[A] => Either[String, HeldEnum]
    implicit val wrapNumberValues: EnumValues[Number] = { case (tpe, fmt) =>
      Option(_)
        .filterNot(_.isEmpty)
        .toRight("Model has no enumerations")
        .map(xs =>
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
      _ <- Target.log.debug(s"Args: ${paths.unwrapTracker.value.map { case (a, b) => (a, b.showNotNull) }} (${paths.showHistory})")
      routes <- paths.indexedCosequence.value.flatTraverse { case (pathStr, path) =>
        for {
          operationMap <- path
            .downField("operations", _.readOperationsMap)
            .toNel
            .raiseErrorIfEmpty("No operations defined")
          operationRoutes <- operationMap.indexedCosequence.value.toList.traverse { case (httpMethod, operation) =>
            val securityRequirements: Option[SecurityRequirements] =
              operation.downField[Option[List[SecurityRequirement]]]("security", _.getSecurity()).indexedDistribute.flatMap { srs =>
                NonEmptyList
                  .fromList(srs.indexedDistribute)
                  .fold(globalSecurityRequirements)(SecurityRequirements(_, SecurityRequirements.Local))
              }

            // For some reason the 'resolve' option on the openapi parser doesn't auto-resolve
            // requestBodies, so let's manually fix that up here.
            val updatedOperation: Target[Tracker[Operation]] = operation
              .downField("body", _.getRequestBody)
              .flatDownField("$ref", _.get$ref)
              .refine[Target[Tracker[Operation]]] { case Some(x) => (x, x.split("/").toList) }(tracker =>
                tracker.unwrapTracker match {
                  case (rbref, "#" :: "components" :: "requestBodies" :: name :: Nil) =>
                    commonRequestBodies
                      .get(name)
                      .fold[Target[Tracker[Operation]]](
                        Target.raiseUserError(s"Unable to resolve reference to '$rbref' when attempting to process ${tracker.showHistory}")
                      ) { commonRequestBody =>
                        Target.pure(
                          operation.map(op =>
                            copyOperation(op)
                              .requestBody(copyRequestBody(commonRequestBody))
                          )
                        )
                      }
                  case (rbref, _) =>
                    Target.raiseUserError(s"Invalid request body $$ref name '$rbref' when attempting to process ${tracker.showHistory}")
                }
              )
              .getOrElse(Target.pure(operation))

            updatedOperation.map(op => RouteMeta(Tracker.cloneHistory(path, pathStr), httpMethod, op, securityRequirements))
          }
        } yield operationRoutes
      }
    } yield routes)

  override def extractApiKeySecurityScheme(schemeName: String, securityScheme: Tracker[SwSecurityScheme], tpe: Option[L#Type]) =
    for {
      name <- securityScheme.downField("name", _.getName).raiseErrorIfEmpty(s"Security scheme ${schemeName} is an API Key scheme but has no 'name' property")
      in   <- securityScheme.downField("in", _.getIn).raiseErrorIfEmpty(s"Security scheme ${schemeName} is an API Key scheme but has no 'in' property")
    } yield ApiKeySecurityScheme[L](name.unwrapTracker, in.unwrapTracker, tpe)

  override def extractHttpSecurityScheme(schemeName: String, securityScheme: Tracker[SwSecurityScheme], tpe: Option[L#Type]) =
    for {
      authScheme <- securityScheme.downField("scheme", _.getScheme).raiseErrorIfEmpty(s"Security scheme ${schemeName} is a HTTP scheme but has no auth scheme")
    } yield HttpSecurityScheme[L](authScheme.unwrapTracker, tpe)

  override def extractOpenIdConnectSecurityScheme(
      schemeName: String,
      securityScheme: Tracker[SwSecurityScheme],
      tpe: Option[L#Type]
  ) =
    for {
      url <- securityScheme
        .downField("openIdConnectUrl", _.getOpenIdConnectUrl)
        .raiseErrorIfEmpty(s"Security scheme ${schemeName} has a missing OpenID Connect URL")
      url <- url.map(url => Try(new URI(url)).toOption).raiseErrorIfEmpty(s"Security scheme ${schemeName} has an invalid OpenID Connect URL")
    } yield OpenIdConnectSecurityScheme[L](url.unwrapTracker, tpe)

  override def extractOAuth2SecurityScheme(schemeName: String, securityScheme: Tracker[SwSecurityScheme], tpe: Option[L#Type]) =
    for {
      flows <- securityScheme.downField("flows", _.getFlows).raiseErrorIfEmpty(s"Security scheme ${schemeName} has no OAuth2 flows")
    } yield OAuth2SecurityScheme[L](flows.unwrapTracker, tpe)

  override def extractMutualTLSSecurityScheme(schemeName: String, securityScheme: Tracker[SwSecurityScheme], tpe: Option[L#Type]) =
    Target.pure(MutualTLSSecurityScheme[L](tpe))

  override def getClassName(operation: Tracker[Operation], vendorPrefixes: List[String], tagBehaviour: Context.TagsBehaviour) =
    Target.log.function("getClassName")(for {
      _ <- Target.log.debug(s"Args: ${operation.unwrapTracker.showNotNull}")

      className = ClassPrefix(operation, vendorPrefixes) match {
        case cls @ Some(_) => cls.toList
        case None =>
          val pkg = PackageName(operation, vendorPrefixes)
            .map(_.split('.').toVector)
            .orElse(
              tagBehaviour match {
                case Context.PackageFromTags =>
                  operation
                    .downField("tags", _.getTags)
                    .toNel
                    .indexedCosequence
                    .map { tags =>
                      tags.unwrapTracker.toList
                    }
                case Context.TagsAreIgnored =>
                  None
              }
            )
          val opPkg = operation.downField("operationId", _.getOperationId()).map(_.toList.flatMap(splitOperationParts(_)._1)).unwrapTracker
          pkg.fold(opPkg)(_.toList ++ opPkg)
      }
    } yield className)

  override def getParameterName(parameter: Tracker[Parameter]) =
    parameter
      .downField("name", _.getName())
      .raiseErrorIfEmpty("Name not specified")
      .map(_.unwrapTracker)

  override def getParameterSchema(parameter: Tracker[Parameter], components: Tracker[Option[Components]]) =
    for {
      schema <- parameter
        .downField("schema", _.getSchema())
        .raiseErrorIfEmpty("Schema not specified")
      dereferenced <- schema
        .downField("$ref", _.get$ref())
        .indexedDistribute
        .fold[Target[Tracker[SchemaProjection]]](Target.pure(schema.map(SchemaLiteral)))(ref =>
          dereferenceSchema(ref, components).map(_.map(schema => SchemaRef(SchemaLiteral(schema), ref.unwrapTracker)))
        )
    } yield dereferenced

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

  private def buildExtractor[A](components: Tracker[Option[Components]], label: String, proj: Components => ju.Map[String, A])(
      ref: Tracker[String]
  ): Target[Tracker[A]] = {
    val extract = s"^#/components/$label/([^/]*)$$".r
    ref
      .refine[Target[Tracker[A]]] { case extract(name) => name }(name =>
        components.indexedDistribute
          .fold[Target[Tracker[A]]](Target.raiseException("Attempting to dereference a $ref, but no components defined"))(components =>
            Target.fromOption(
              components.downField(label, proj).indexedDistribute.value.toMap.get(name.unwrapTracker),
              UserError(s"Attempting to dereference a $$ref, but no object found at the specified pointer")
            )
          )
      )
      .orRefineFallback(_ =>
        Target.raiseException(
          s"While attempting to dereference '${label}', encountered a JSON pointer to a different component type: ${ref.unwrapTracker} (${ref.showHistory})"
        )
      )
  }

  def dereferenceHeader(ref: Tracker[String], components: Tracker[Option[Components]]): dev.guardrail.Target[Tracker[headers.Header]] =
    buildExtractor(components, "headers", _.getHeaders())(ref)
  def dereferenceParameter(ref: Tracker[String], components: Tracker[Option[Components]]): dev.guardrail.Target[Tracker[Parameter]] =
    buildExtractor(components, "parameters", _.getParameters())(ref)
  def dereferenceRequestBodie(ref: Tracker[String], components: Tracker[Option[Components]]): dev.guardrail.Target[Tracker[RequestBody]] =
    buildExtractor(components, "requestBodies", _.getRequestBodies())(ref)
  def dereferenceResponse(ref: Tracker[String], components: Tracker[Option[Components]]): dev.guardrail.Target[Tracker[ApiResponse]] =
    buildExtractor(components, "responses", _.getResponses())(ref)
  def dereferenceSchema(ref: Tracker[String], components: Tracker[Option[Components]]): dev.guardrail.Target[Tracker[Schema[_]]] =
    buildExtractor(components, "schemas", _.getSchemas())(ref)

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
