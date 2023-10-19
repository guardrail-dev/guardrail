package dev.guardrail.core.resolvers

import dev.guardrail.core
import dev.guardrail.languages.LA
import dev.guardrail.core.{ ReifiedRawType, Tracker }
import dev.guardrail.core.extract.{ CustomArrayTypeName, CustomMapTypeName, CustomTypeName, Default, Extractable }
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, SchemaLiteral, SchemaProjection, SchemaRef, SwaggerTerms }
import dev.guardrail.terms.framework.FrameworkTerms

import cats.Monad
import cats.syntax.all._
import io.swagger.v3.oas.models._
import io.swagger.v3.oas.models.media._
import scala.jdk.CollectionConverters._

object ModelResolver {
  def propMeta[L <: LA, F[_]: Monad](property: Tracker[Schema[_]], components: Tracker[Option[Components]])(implicit
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F],
      Fw: FrameworkTerms[L, F]
  ): F[core.ResolvedType[L]] = propMetaImpl[L, F](property, components)(Left(_))

  def propMetaWithName[L <: LA, F[_]: Monad](tpe: L#Type, property: Tracker[Schema[_]], components: Tracker[Option[Components]])(implicit
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

  def modelMetaType[L <: LA, F[_]: Monad](
      model: Tracker[Schema[_]],
      components: Tracker[Option[Components]]
  )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: SwaggerTerms[L, F], Fw: FrameworkTerms[L, F]): F[core.ResolvedType[L]] =
    propMetaImpl[L, F](model, components)(Left(_))

  private def propMetaImpl[L <: LA, F[_]: Monad](property: Tracker[Schema[_]], components: Tracker[Option[Components]])(
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
              prefixes  <- vendorPrefixes()
              customTpe <- CustomTypeName(o, prefixes).flatTraverse(x => liftCustomType[L, F](Tracker.cloneHistory(o, x)))
              fallback  <- objectType(None)
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
              prefixes  <- vendorPrefixes()
              arrayType <- CustomArrayTypeName(arr, prefixes).flatTraverse(x => parseType(Tracker.cloneHistory(arr, x)))
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
              prefixes <- vendorPrefixes()
              mapType  <- CustomMapTypeName(map, prefixes).flatTraverse(x => parseType(Tracker.cloneHistory(map, x)))
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

  // Standard type conversions, as documented in http://swagger.io/specification/#data-types-12
  def determineTypeName[L <: LA, F[_]: Monad](
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
                  prefixes              <- vendorPrefixes()
                  customArrayType       <- CustomArrayTypeName(schema, prefixes).flatTraverse(x => parseType(Tracker.cloneHistory(schema, x)))
                  lifted                <- liftVectorType(found, customArrayType)
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
              extractFormat(fmt => stringType(None))
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
            .orRefine { case SchemaLiteral(x: JsonSchema) => x }(extractFormat(fmt => objectType(fmt)))
            .orRefine { case SchemaLiteral(x: MapSchema) => x }(extractFormat(fmt => objectType(fmt)))
            .orRefine { case SchemaLiteral(x: NumberSchema) => x }(extractFormat(fmt => numberType(fmt)))
            .orRefine { case SchemaLiteral(x: ObjectSchema) => x }(extractFormat(fmt => objectType(fmt)))
            .orRefine { case SchemaLiteral(x: PasswordSchema) => x }(const(stringType(None)))
            .orRefine { case SchemaLiteral(x: StringSchema) => x }(extractFormat(fmt => stringType(None)))
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

  private def enrichWithDefault[L <: LA, F[_]: Monad](schema: Tracker[Schema[_]])(implicit
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

  private[this] def liftCustomType[L <: LA, F[_]: Monad](s: Tracker[String])(implicit Sc: LanguageTerms[L, F]): F[Option[L#Type]] = {
    import Sc._
    val tpe = s.map(_.trim)
    if (tpe.unwrapTracker.nonEmpty) {
      parseType(tpe)
    } else Option.empty[L#Type].pure[F]
  }

  private def resolveScalarTypes[L <: LA, F[_]: Monad](
      partial: Either[Tracker[Schema[_]], F[core.ResolvedType[L]]],
      components: Tracker[Option[Components]]
  )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: SwaggerTerms[L, F], Fw: FrameworkTerms[L, F]): F[core.ResolvedType[L]] = {
    import Cl._
    import Sw._
    def buildResolveNoDefault[A <: Schema[_]]: Tracker[A] => F[core.ResolvedType[L]] = { a =>
      for {
        prefixes       <- vendorPrefixes()
        (tpe, rawType) <- determineTypeName[L, F](a, Tracker.cloneHistory(a, CustomTypeName(a, prefixes)), components)
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
}
