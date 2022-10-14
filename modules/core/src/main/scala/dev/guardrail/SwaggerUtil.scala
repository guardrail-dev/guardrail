package dev.guardrail

import io.swagger.v3.oas.models._
import io.swagger.v3.oas.models.media._
import io.swagger.v3.oas.models.parameters.RequestBody
import io.swagger.v3.oas.models.security.{ SecurityScheme => SwSecurityScheme }
import cats.syntax.all._
import dev.guardrail.core.{ ReifiedRawType, Tracker }
import dev.guardrail.core.implicits._
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, SchemaLiteral, SchemaProjection, SchemaRef, SecurityScheme, SwaggerTerms }
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.core.extract.{ CustomArrayTypeName, CustomMapTypeName, CustomTypeName, Default, Extractable, VendorExtension }
import dev.guardrail.core.extract.VendorExtension.VendorExtensible._
import dev.guardrail.languages.LA
import dev.guardrail.terms.protocol.PropMeta
import scala.jdk.CollectionConverters._

object SwaggerUtil {
  def customTypeName[L <: LA, F[_], A: VendorExtension.VendorExtensible](v: A)(implicit Cl: CollectionsLibTerms[L, F]): F[Option[String]] = {
    import Cl._
    for {
      prefixes <- vendorPrefixes()
    } yield CustomTypeName(v, prefixes)
  }

  def customArrayTypeName[L <: LA, F[_], A: VendorExtension.VendorExtensible](v: A)(implicit Cl: CollectionsLibTerms[L, F]): F[Option[String]] = {
    import Cl._
    for {
      prefixes <- vendorPrefixes()
    } yield CustomArrayTypeName(v, prefixes)
  }

  def customMapTypeName[L <: LA, F[_], A: VendorExtension.VendorExtensible](v: A)(implicit Cl: CollectionsLibTerms[L, F]): F[Option[String]] = {
    import Cl._
    for {
      prefixes <- vendorPrefixes()
    } yield CustomMapTypeName(v, prefixes)
  }

  def modelMetaType[L <: LA, F[_]](
      model: Tracker[Schema[_]],
      components: Tracker[Option[Components]]
  )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: SwaggerTerms[L, F], Fw: FrameworkTerms[L, F]): F[core.ResolvedType[L]] =
    propMetaImpl[L, F](model, components)(Left(_))

  def extractConcreteTypes[L <: LA, F[_]](
      definitions: List[(String, Tracker[Schema[_]])],
      components: Tracker[Option[Components]]
  )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: SwaggerTerms[L, F], F: FrameworkTerms[L, F]): F[List[PropMeta[L]]] = {
    import Sc._
    for {
      entries <- definitions.traverse[F, (String, core.ResolvedType[L])] { case (clsName, schema) =>
        schema
          .refine { case impl: Schema[_] if Option(impl.getProperties()).isDefined || Option(impl.getEnum()).isDefined => impl }(impl =>
            for {
              formattedClsName <- formatTypeName(clsName)
              typeName         <- pureTypeName(formattedClsName)
              widenedTypeName  <- widenTypeName(typeName)
            } yield (clsName, core.Resolved[L](widenedTypeName, None, None, ReifiedRawType.unsafeEmpty): core.ResolvedType[L])
          )
          .orRefine { case comp: ComposedSchema => comp }(comp =>
            for {
              formattedClsName <- formatTypeName(clsName)
              typeName         <- pureTypeName(formattedClsName)
              widenedTypeName  <- widenTypeName(typeName)
              parentSimpleRef = comp
                .downField("allOf", _.getAllOf)
                .indexedDistribute
                .headOption
                .flatMap(_.downField("$ref", _.get$ref).indexedDistribute)
                .map(_.unwrapTracker.split("/").last)
              parentTerm <- parentSimpleRef.traverse(n => pureTermName(n))
              resolvedType = core.Resolved[L](widenedTypeName, parentTerm, None, ReifiedRawType.unsafeEmpty): core.ResolvedType[L]
            } yield (clsName, resolvedType)
          )
          .getOrElse(
            for {
              resolved <- modelMetaType[L, F](schema, components)
            } yield (clsName, resolved)
          )
      }
      result <- core.ResolvedType.resolveReferences[L, F](entries)
    } yield result.map { case (clsName, core.Resolved(tpe, _, _, _)) =>
      PropMeta[L](clsName, tpe) // TODO: We're losing ReifiedRawType here. Perhaps maintain through PropMeta?
    }
  }

  // Standard type conversions, as documented in http://swagger.io/specification/#data-types-12
  def determineTypeName[L <: LA, F[_]](
      rawSchema: Tracker[Schema[_]],
      customType: Tracker[Option[String]],
      components: Tracker[Option[Components]]
  )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: SwaggerTerms[L, F], Fw: FrameworkTerms[L, F]): F[(L#Type, ReifiedRawType)] =
    Sw.log.function(s"determineTypeName(${rawSchema.unwrapTracker}, ${customType.unwrapTracker})") {
      import Sc._
      import Cl._
      import Fw._

      for {
        schemaProjection <- rawSchema
          .downField("$ref", _.get$ref())
          .indexedDistribute
          .fold[F[Tracker[SchemaProjection]]](rawSchema.map(SchemaLiteral(_): SchemaProjection).pure[F]) { ref =>
            Sw.dereferenceSchema(ref, components).map(_.map(schema => SchemaRef(SchemaLiteral(schema), ref.unwrapTracker)))
          }
        customTpe <- customType.indexedDistribute.flatTraverse(x => liftCustomType[L, F](x))
        log = { (fmt: Option[String], t: L#Type) =>
          fmt.foreach { fmt =>
            if (customTpe.isEmpty) {
              println(
                s"Warning: Deprecated behavior: Unsupported format '$fmt' for type '${rawSchema.unwrapTracker
                    .getType()}', falling back to $t. Please switch definitions to x-scala-type for custom types. (${rawSchema.showHistory})"
              )
            }
          }

          t: L#Type
        }

        (renderType, reifiedRawType) <- {
          def extractFormat(func: Option[String] => F[L#Type]): Tracker[Schema[_]] => F[(L#Type, ReifiedRawType)] = { schema =>
            val rawType   = schema.downField("type", _.getType())
            val rawFormat = schema.downField("format", _.getFormat())
            for {
              rendered <- func(rawFormat.unwrapTracker)
            } yield (rendered, ReifiedRawType.of(rawType.unwrapTracker, rawFormat.unwrapTracker))
          }
          def const(value: F[L#Type]): Tracker[Schema[_]] => F[(L#Type, ReifiedRawType)] = extractFormat(_ => value)

          // traverseArraySchema is broken out due to the complexity.
          val traverseArraySchema: Tracker[ArraySchema] => F[(L#Type, ReifiedRawType)] = { schema =>
            schema
              .downField("items", _.getItems())
              .cotraverse(itemsSchema =>
                for {
                  (found, innerRawType) <- determineTypeName(itemsSchema, Tracker.cloneHistory(schema, None), components)
                  customArrayType <- SwaggerUtil
                    .customArrayTypeName(schema)
                    .flatMap(_.flatTraverse(x => parseType(Tracker.cloneHistory(schema, x))))
                  lifted <- liftVectorType(found, customArrayType)
                } yield (lifted, ReifiedRawType.ofVector(innerRawType): ReifiedRawType)
              )
              .getOrElse(arrayType(None).map((_, ReifiedRawType.unsafeEmpty)))
          }

          schemaProjection
            // Dereference SchemaRef
            .refine[F[(L#Type, ReifiedRawType)]] { case SchemaRef(_, ref) => ref }(ref =>
              for {
                refName          <- fallbackType(ref.unwrapTracker.split("/").lastOption, None)
                underlyingSchema <- Sw.dereferenceSchema(ref, components)
              } yield (
                refName,
                ReifiedRawType.of(
                  underlyingSchema.downField("type", _.getType()).unwrapTracker,
                  underlyingSchema.downField("format", _.getFormat()).unwrapTracker
                )
              )
            )
            // Explicitly return `string` if ObjectSchema has enum values. Should this go?
            .orRefine { case SchemaLiteral(x: ObjectSchema) if Option(x.getEnum).map(_.asScala).exists(_.nonEmpty) => x }(
              extractFormat(fmt => stringType(None).map(log(fmt, _)))
            )
            // First, match SchemaLiteral's that have conditionals on either the type or format
            .orRefine { case SchemaLiteral(x: IntegerSchema) if x.getFormat() == "int32" => x }(const(intType()))
            .orRefine { case SchemaLiteral(x: IntegerSchema) if x.getFormat() == "int64" => x }(const(longType()))
            .orRefine { case SchemaLiteral(x: NumberSchema) if x.getFormat() == "double" => x }(const(doubleType()))
            .orRefine { case SchemaLiteral(x: NumberSchema) if x.getFormat() == "float" => x }(const(floatType()))
            .orRefine { case SchemaLiteral(x: StringSchema) if x.getFormat() == "binary" => x }(const(fileType(None)))
            .orRefine { case SchemaLiteral(x: StringSchema) if x.getFormat() == "byte" => x }(const(bytesType()))
            .orRefine { case SchemaLiteral(x: StringSchema) if x.getFormat() == "email" => x }(const(stringType(None)))
            .orRefine { case SchemaLiteral(x: StringSchema) if x.getFormat() == "password" => x }(const(stringType(None)))
            .orRefine { case SchemaLiteral(x: StringSchema) if x.getType() == "file" => x }(const(fileType(None)))
            // Then, fall-back to the base type matchers.
            .orRefine { case SchemaLiteral(x: ArraySchema) => x }(traverseArraySchema)
            .orRefine { case SchemaLiteral(x: BinarySchema) => x }(const(fileType(None)))
            .orRefine { case SchemaLiteral(x: BooleanSchema) => x }(extractFormat(fmt => booleanType(fmt)))
            .orRefine { case SchemaLiteral(x: ByteArraySchema) => x }(const(bytesType()))
            .orRefine { case SchemaLiteral(x: DateSchema) => x }(const(dateType()))
            .orRefine { case SchemaLiteral(x: DateTimeSchema) => x }(const(dateTimeType()))
            .orRefine { case SchemaLiteral(x: EmailSchema) => x }(const(stringType(None)))
            .orRefine { case SchemaLiteral(x: FileSchema) => x }(const(fileType(None)))
            .orRefine { case SchemaLiteral(x: IntegerSchema) => x }(extractFormat(fmt => integerType(fmt)))
            .orRefine { case SchemaLiteral(x: JsonSchema) => x }(extractFormat(fmt => objectType(fmt).map(log(fmt, _))))
            .orRefine { case SchemaLiteral(x: MapSchema) => x }(extractFormat(fmt => objectType(fmt).map(log(fmt, _))))
            .orRefine { case SchemaLiteral(x: NumberSchema) => x }(extractFormat(fmt => numberType(fmt).map(log(fmt, _))))
            .orRefine { case SchemaLiteral(x: ObjectSchema) => x }(extractFormat(fmt => objectType(fmt).map(log(fmt, _))))
            .orRefine { case SchemaLiteral(x: PasswordSchema) => x }(const(stringType(None)))
            .orRefine { case SchemaLiteral(x: StringSchema) => x }(extractFormat(fmt => stringType(None).map(log(fmt, _))))
            .orRefine { case SchemaLiteral(x: UUIDSchema) => x }(const(uuidType()))
            // Finally, attempt to recover if we've gotten all the way down here without knowing what we're dealing with
            .orRefineFallback { schemaProjection =>
              schemaProjection match {
                case Tracker(history, SchemaLiteral(x)) =>
                  println(
                    s"WARNING: Missing type mapping for ${x.getClass} (${history}), please report this at https://github.com/guardrail-dev/guardrail/issues"
                  )
                case Tracker(history, SchemaRef(schema, ref)) =>
                  println(s"WARNING: Unexpected type mapping missing, $ref (${history})")
              }
              val schema = schemaProjection.map {
                case SchemaLiteral(x)               => x
                case SchemaRef(SchemaLiteral(x), _) => x
              }
              val rawType   = schema.downField("type", _.getType())
              val rawFormat = schema.downField("format", _.getFormat())
              for {
                declType <- fallbackType(rawType.unwrapTracker, rawFormat.unwrapTracker)
              } yield (declType, ReifiedRawType.of(rawType.unwrapTracker, rawFormat.unwrapTracker))
            }
        }
        customTpe <- customType.indexedDistribute.flatTraverse(x => liftCustomType[L, F](x))
        result = customTpe.getOrElse(renderType)
        _ <- Sw.log.debug(s"Returning ${result}")
      } yield (result, reifiedRawType)
    }

  def isFile(typeName: String, format: Option[String]): Boolean =
    (typeName, format) match {
      case ("string", Some("binary")) => true
      case ("file", _)                => true
      case ("binary", _)              => true
      case _                          => false
    }

  def propMeta[L <: LA, F[_]](property: Tracker[Schema[_]], components: Tracker[Option[Components]])(implicit
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F],
      Fw: FrameworkTerms[L, F]
  ): F[core.ResolvedType[L]] = propMetaImpl(property, components)(Left(_))

  private[this] def liftCustomType[L <: LA, F[_]](s: Tracker[String])(implicit Sc: LanguageTerms[L, F]): F[Option[L#Type]] = {
    import Sc._
    val tpe = s.map(_.trim)
    if (tpe.unwrapTracker.nonEmpty) {
      parseType(tpe)
    } else Option.empty[L#Type].pure[F]
  }

  def propMetaWithName[L <: LA, F[_]](tpe: L#Type, property: Tracker[Schema[_]], components: Tracker[Option[Components]])(implicit
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F],
      Fw: FrameworkTerms[L, F]
  ): F[core.ResolvedType[L]] =
    propMetaImpl(property, components)(
      _.refine[core.ResolvedType[L]] { case schema: ObjectSchema if Option(schema.getProperties).exists(p => !p.isEmpty) => schema }(_ =>
        core.Resolved[L](tpe, None, None, ReifiedRawType.unsafeEmpty)
      ).orRefine { case c: ComposedSchema => c }(_ => core.Resolved[L](tpe, None, None, ReifiedRawType.unsafeEmpty))
        .orRefine { case schema: StringSchema if Option(schema.getEnum).map(_.asScala).exists(_.nonEmpty) => schema }(_ =>
          core.Resolved[L](tpe, None, None, ReifiedRawType.unsafeEmpty)
        )
        .map(_.pure[F])
    )

  private def resolveScalarTypes[L <: LA, F[_]](
      partial: Either[Tracker[Schema[_]], F[core.ResolvedType[L]]],
      components: Tracker[Option[Components]]
  )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: SwaggerTerms[L, F], Fw: FrameworkTerms[L, F]): F[core.ResolvedType[L]] = {
    import Sw._
    def buildResolveNoDefault[A <: Schema[_]]: Tracker[A] => F[core.ResolvedType[L]] = { a =>
      for {
        customTpeName  <- customTypeName(a)
        (tpe, rawType) <- determineTypeName[L, F](a, Tracker.cloneHistory(a, customTpeName), components)
      } yield core.Resolved[L](tpe, None, None, rawType)
    }

    partial
      .orRefine { case b: BooleanSchema => b }(buildResolveNoDefault)
      .orRefine { case s: StringSchema => s }(buildResolveNoDefault)
      .orRefine { case s: EmailSchema => s }(buildResolveNoDefault)
      .orRefine { case d: DateSchema => d }(buildResolveNoDefault)
      .orRefine { case d: DateTimeSchema => d }(buildResolveNoDefault)
      .orRefine { case i: IntegerSchema => i }(buildResolveNoDefault)
      .orRefine { case d: NumberSchema => d }(buildResolveNoDefault)
      .orRefine { case p: PasswordSchema => p }(buildResolveNoDefault)
      .orRefine { case f: FileSchema => f }(buildResolveNoDefault)
      .orRefine { case b: BinarySchema => b }(buildResolveNoDefault)
      .orRefine { case u: UUIDSchema => u }(buildResolveNoDefault)
      .orRefineFallback(x =>
        fallbackPropertyTypeHandler(x).map(core.Resolved[L](_, None, None, ReifiedRawType.unsafeEmpty))
      ) // This may need to be rawType=string?
  }

  private def enrichWithDefault[L <: LA, F[_]](schema: Tracker[Schema[_]])(implicit
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F],
      Fw: FrameworkTerms[L, F]
  ): core.ResolvedType[L] => F[core.ResolvedType[L]] = { resolved =>
    import Sc._
    def buildResolve[B: Extractable, A <: Schema[_]: Default.GetDefault](transformLit: B => F[L#Term]): Tracker[A] => F[core.ResolvedType[L]] = { a =>
      for {
        default <- Default(a).extract[B].traverse(transformLit(_))
      } yield resolved match {
        case x: core.Resolved[L] => x.copy(defaultValue = default)
        case other               => other
      }
    }
    schema
      .refine[F[core.ResolvedType[L]]] { case b: BooleanSchema => b }(buildResolve(litBoolean))
      .orRefine { case s: StringSchema => s }(buildResolve(litString))
      .orRefine { case s: IntegerSchema => s }(buildResolve(litInt))
      .orRefineFallback(_ => resolved.pure[F])
  }

  private def propMetaImpl[L <: LA, F[_]](property: Tracker[Schema[_]], components: Tracker[Option[Components]])(
      strategy: Tracker[Schema[_]] => Either[Tracker[Schema[_]], F[core.ResolvedType[L]]]
  )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: SwaggerTerms[L, F], Fw: FrameworkTerms[L, F]): F[core.ResolvedType[L]] =
    Sw.log.function("propMeta") {
      import Fw._
      import Sc._
      import Cl._
      import Sw._

      for {
        _ <- log.debug(s"property:\n${log.schemaToString(property.unwrapTracker)} (${property.unwrapTracker.getExtensions()}, ${property.showHistory})")

        res <- strategy(property)
          .orRefine { case o: ObjectSchema => o }(o =>
            for {
              customTpeName <- customTypeName(o)
              customTpe     <- customTpeName.flatTraverse(x => liftCustomType[L, F](Tracker.cloneHistory(o, x)))
              fallback      <- objectType(None)
            } yield core.Resolved[L](customTpe.getOrElse(fallback), None, None, ReifiedRawType.unsafeEmpty)
          )
          .orRefine { case arr: ArraySchema => arr }(arr =>
            for {
              items <- getItems(arr)
              dereferencedItems <- items
                .downField("$ref", _.get$ref())
                .indexedDistribute
                .fold[F[Tracker[Schema[_]]]](items.pure[F])(ref => dereferenceSchema(ref, components))
              meta <- propMetaImpl[L, F](items, components)(strategy)
              itemsRawType   = dereferencedItems.downField("type", _.getType())
              itemsRawFormat = dereferencedItems.downField("format", _.getFormat())
              arrayType <- customArrayTypeName(arr).flatMap(_.flatTraverse(x => parseType(Tracker.cloneHistory(arr, x))))
              res <- meta match {
                case core.Resolved(inner, dep, default, _) =>
                  (liftVectorType(inner, arrayType), default.traverse(liftVectorTerm))
                    .mapN(core.Resolved[L](_, dep, _, ReifiedRawType.ofVector(ReifiedRawType.of(itemsRawType.unwrapTracker, itemsRawFormat.unwrapTracker))))
                case x: core.Deferred[L]      => embedArray(x, arrayType)
                case x: core.DeferredArray[L] => embedArray(x, arrayType)
                case x: core.DeferredMap[L]   => embedArray(x, arrayType)
              }
            } yield res
          )
          .orRefine { case map: MapSchema => map } { map =>
            val rawType   = map.downField("type", _.getType())
            val rawFormat = map.downField("format", _.getFormat())
            for {
              rec <- map
                .downField("additionalProperties", _.getAdditionalProperties())
                .map(_.getOrElse(false))
                .refine[F[core.ResolvedType[L]]] { case b: java.lang.Boolean => b }(_ =>
                  objectType(None).map(core.Resolved[L](_, None, None, ReifiedRawType.ofMap(ReifiedRawType.unsafeEmpty)))
                )
                .orRefine { case s: Schema[_] => s }(s => propMetaImpl[L, F](s, components)(strategy))
                .orRefineFallback { s =>
                  log.debug(s"Unknown structure cannot be reflected: ${s.unwrapTracker} (${s.showHistory})") >> objectType(None)
                    .map(core.Resolved[L](_, None, None, ReifiedRawType.ofMap(ReifiedRawType.of(rawType.unwrapTracker, rawFormat.unwrapTracker))))
                }
              mapType <- customMapTypeName(map).flatMap(_.flatTraverse(x => parseType(Tracker.cloneHistory(map, x))))
              res <- rec match {
                case core.Resolved(inner, dep, _, rawType) => liftMapType(inner, mapType).map(core.Resolved[L](_, dep, None, ReifiedRawType.ofMap(rawType)))
                case x: core.DeferredMap[L]                => embedMap(x, mapType)
                case x: core.DeferredArray[L]              => embedMap(x, mapType)
                case x: core.Deferred[L]                   => embedMap(x, mapType)
              }
            } yield res
          }
          .orRefine { case ref: Schema[_] if Option(ref.get$ref).isDefined => ref }(ref => getSimpleRef(ref.map(Option.apply _)).map(core.Deferred[L]))
          .pure[F]
        scalarResolved <- resolveScalarTypes[L, F](res, components)
        withDefaults   <- enrichWithDefault[L, F](property).apply(scalarResolved)
      } yield withDefaults
    }

  def extractSecuritySchemes[L <: LA, F[_]](
      swagger: OpenAPI,
      prefixes: List[String]
  )(implicit Sw: SwaggerTerms[L, F], Sc: LanguageTerms[L, F]): F[Map[String, SecurityScheme[L]]] = {
    import Sw._
    import Sc._

    Tracker(swagger)
      .downField("components", _.getComponents)
      .flatDownField("securitySchemes", _.getSecuritySchemes)
      .indexedDistribute
      .value
      .flatTraverse { case (schemeName, scheme) =>
        val typeName = CustomTypeName(scheme, prefixes)
        for {
          tpe <- typeName.fold(Option.empty[L#Type].pure[F])(x => parseType(Tracker.cloneHistory(scheme, x)))
          parsedScheme <- scheme.downField("type", _.getType).unwrapTracker.traverse {
            case SwSecurityScheme.Type.APIKEY        => extractApiKeySecurityScheme(schemeName, scheme, tpe).widen[SecurityScheme[L]]
            case SwSecurityScheme.Type.HTTP          => extractHttpSecurityScheme(schemeName, scheme, tpe).widen[SecurityScheme[L]]
            case SwSecurityScheme.Type.OPENIDCONNECT => extractOpenIdConnectSecurityScheme(schemeName, scheme, tpe).widen[SecurityScheme[L]]
            case SwSecurityScheme.Type.OAUTH2        => extractOAuth2SecurityScheme(schemeName, scheme, tpe).widen[SecurityScheme[L]]
            case SwSecurityScheme.Type.MUTUALTLS     => extractMutualTLSSecurityScheme(schemeName, scheme, tpe).widen[SecurityScheme[L]]
          }
        } yield parsedScheme.toList.map(scheme => schemeName -> scheme)
      }
      .map(_.toMap)
  }

  def copyOperation(operation: Operation): Operation =
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
}
