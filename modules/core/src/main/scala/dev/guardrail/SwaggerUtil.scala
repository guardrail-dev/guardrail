package dev.guardrail

import io.swagger.v3.oas.models._
import io.swagger.v3.oas.models.media._
import io.swagger.v3.oas.models.parameters.RequestBody
import io.swagger.v3.oas.models.security.{ SecurityScheme => SwSecurityScheme }
import cats.syntax.all._
import dev.guardrail.core.Tracker
import dev.guardrail.core.implicits._
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, SecurityScheme, SwaggerTerms }
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

  sealed class ModelMetaTypePartiallyApplied[L <: LA, F[_]](val dummy: Boolean = true) {
    def apply[T <: Schema[_]](
        model: Tracker[T]
    )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: SwaggerTerms[L, F], Fw: FrameworkTerms[L, F]): F[core.ResolvedType[L]] =
      Sw.log.function("modelMetaType") {
        import Sc._
        import Cl._
        import Sw._
        import Fw._

        def strategy(property: Tracker[T]): Either[Tracker[T], F[core.ResolvedType[L]]] = Left(property)

        log.debug(s"model:\n${log.schemaToString(model.unwrapTracker)}") >> (strategy(model)
          .orRefine({ case arr: ArraySchema => arr })(
            arr =>
              for {
                items <- getItems(arr)
                meta  <- propMetaImpl[L, F](items)(Left(_))
                rawType   = arr.downField("type", _.getType())
                rawFormat = arr.downField("format", _.getFormat())
                arrayType <- customArrayTypeName(arr).flatMap(_.flatTraverse(x => parseType(Tracker.cloneHistory(arr, x))))
                res <- meta match {
                  case core.Resolved(inner, dep, default, _, _) =>
                    (liftVectorType(inner, arrayType), default.traverse(liftVectorTerm))
                      .mapN(core.Resolved[L](_, dep, _, rawType.unwrapTracker, rawFormat.unwrapTracker))
                  case x: core.Deferred[L]      => embedArray(x, arrayType)
                  case x: core.DeferredArray[L] => embedArray(x, arrayType)
                  case x: core.DeferredMap[L]   => embedArray(x, arrayType)
                }
              } yield res
          )
          .orRefine({ case map: MapSchema => map })({ map =>
            val rawType   = map.downField("type", _.getType())
            val rawFormat = map.downField("format", _.getFormat())
            for {
              rec <- map
                .downField("additionalProperties", _.getAdditionalProperties())
                .map(_.getOrElse(false))
                .refine[F[core.ResolvedType[L]]]({ case b: java.lang.Boolean => b })(
                  _ => objectType(None).map(core.Resolved[L](_, None, None, rawType.unwrapTracker, rawFormat.unwrapTracker))
                )
                .orRefine({ case s: Schema[_] => s })(s => propMetaImpl[L, F](s)(Left(_)))
                .orRefineFallback({ s =>
                  log.debug(s"Unknown structure cannot be reflected: ${s.unwrapTracker} (${s.showHistory})") >> objectType(None)
                    .map(core.Resolved[L](_, None, None, rawType.unwrapTracker, rawFormat.unwrapTracker))
                })
              mapType <- customMapTypeName(map).flatMap(_.flatTraverse(x => parseType(Tracker.cloneHistory(map, x))))
              res <- rec match {
                case core.Resolved(inner, dep, _, tpe, fmt) => liftMapType(inner, mapType).map(core.Resolved[L](_, dep, None, tpe, fmt))
                case x: core.DeferredMap[L]                 => embedMap(x, mapType)
                case x: core.DeferredArray[L]               => embedMap(x, mapType)
                case x: core.Deferred[L]                    => embedMap(x, mapType)
              }
            } yield res
          })
          .orRefine({ case ref: Schema[_] if Option(ref.get$ref).isDefined => ref })(ref => getSimpleRef(ref.map(Option.apply _)).map(core.Deferred[L])))
          .pure[F]
          .flatMap(resolveScalarTypes[L, F])
          .flatMap(enrichWithDefault[L, F](model))
      }
  }

  def modelMetaType[L <: LA, F[_]]: ModelMetaTypePartiallyApplied[L, F] = new ModelMetaTypePartiallyApplied[L, F]()

  def extractConcreteTypes[L <: LA, F[_]](
      definitions: List[(String, Tracker[Schema[_]])]
  )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: SwaggerTerms[L, F], F: FrameworkTerms[L, F]): F[List[PropMeta[L]]] = {
    import Sc._
    for {
      entries <- definitions.traverse[F, (String, core.ResolvedType[L])] {
        case (clsName, schema) =>
          schema
            .refine({ case impl: Schema[_] if (Option(impl.getProperties()).isDefined || Option(impl.getEnum()).isDefined) => impl })(
              impl =>
                for {
                  formattedClsName <- formatTypeName(clsName)
                  typeName         <- pureTypeName(formattedClsName)
                  widenedTypeName  <- widenTypeName(typeName)
                } yield (clsName, core.Resolved[L](widenedTypeName, None, None, None, None): core.ResolvedType[L])
            )
            .orRefine({ case comp: ComposedSchema => comp })(
              comp =>
                for {
                  formattedClsName <- formatTypeName(clsName)
                  typeName         <- pureTypeName(formattedClsName)
                  widenedTypeName  <- widenTypeName(typeName)
                  parentSimpleRef = comp.downField("allOf", _.getAllOf).map(_.headOption).flatDownField("$ref", _.get$ref).unwrapTracker.map(_.split("/").last)
                  parentTerm <- parentSimpleRef.traverse(n => pureTermName(n))
                  resolvedType = core.Resolved[L](widenedTypeName, parentTerm, None, None, None): core.ResolvedType[L]
                } yield (clsName, resolvedType)
            )
            .getOrElse(
              for {
                resolved <- SwaggerUtil.modelMetaType[L, F](schema)
              } yield (clsName, resolved)
            )
      }
      result <- core.ResolvedType.resolveReferences[L, F](entries)
    } yield result.map {
      case (clsName, core.Resolved(tpe, _, _, _, _)) =>
        PropMeta[L](clsName, tpe)
    }
  }

  // Standard type conversions, as documented in http://swagger.io/specification/#data-types-12
  def typeName[L <: LA, F[_]](
      typeName: Tracker[Option[String]],
      format: Tracker[Option[String]],
      customType: Tracker[Option[String]]
  )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: SwaggerTerms[L, F], Fw: FrameworkTerms[L, F]): F[L#Type] =
    Sw.log.function(s"typeName(${typeName.unwrapTracker}, ${format.unwrapTracker}, ${customType.unwrapTracker})") {
      import Sc._
      import Cl._
      import Fw._

      def log(fmt: Option[String], t: L#Type): L#Type = {
        fmt.foreach { fmt =>
          println(
            s"Warning: Deprecated behavior: Unsupported format '$fmt' for type '${typeName.unwrapTracker}', falling back to $t. Please switch definitions to x-scala-type for custom types. (${format.showHistory})"
          )
        }

        t
      }

      for {
        customTpe <- customType.indexedDistribute.flatTraverse(x => liftCustomType[L, F](x))
        result <- customTpe.fold({
          (typeName.unwrapTracker, format.unwrapTracker) match {
            case (Some("string"), Some("uuid"))         => uuidType()
            case (Some("string"), Some("password"))     => stringType(None)
            case (Some("string"), Some("email"))        => stringType(None)
            case (Some("string"), Some("date"))         => dateType()
            case (Some("string"), Some("date-time"))    => dateTimeType()
            case (Some("string"), Some("byte"))         => bytesType()
            case (Some("string"), fmt @ Some("binary")) => fileType(None).map(log(fmt, _))
            case (Some("string"), fmt)                  => stringType(None).map(log(fmt, _))
            case (Some("number"), Some("float"))        => floatType()
            case (Some("number"), Some("double"))       => doubleType()
            case (Some("number"), fmt)                  => numberType(fmt).map(log(fmt, _))
            case (Some("integer"), Some("int32"))       => intType()
            case (Some("integer"), Some("int64"))       => longType()
            case (Some("integer"), fmt)                 => integerType(fmt).map(log(fmt, _))
            case (Some("boolean"), fmt)                 => booleanType(fmt).map(log(fmt, _))
            case (Some("array"), fmt)                   => arrayType(fmt).map(log(fmt, _))
            case (Some("file"), fmt) =>
              fileType(None).map(log(fmt, _))
            case (Some("binary"), fmt) =>
              fileType(None).map(log(fmt, _))
            case (Some("object"), fmt) => objectType(fmt).map(log(fmt, _))
            case (tpe, fmt) =>
              fallbackType(tpe, fmt)
          }
        })(_.pure[F])
        _ <- Sw.log.debug(s"Returning ${result}")
      } yield result
    }

  def isFile(typeName: String, format: Option[String]): Boolean =
    (typeName, format) match {
      case ("string", Some("binary")) => true
      case ("file", _)                => true
      case ("binary", _)              => true
      case _                          => false
    }

  def propMeta[L <: LA, F[_]](property: Tracker[Schema[_]])(
      implicit Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F],
      Fw: FrameworkTerms[L, F]
  ): F[core.ResolvedType[L]] = propMetaImpl(property)(Left(_))

  private[this] def liftCustomType[L <: LA, F[_]](s: Tracker[String])(implicit Sc: LanguageTerms[L, F]): F[Option[L#Type]] = {
    import Sc._
    val tpe = s.map(_.trim)
    if (tpe.unwrapTracker.nonEmpty) {
      parseType(tpe)
    } else Option.empty[L#Type].pure[F]
  }

  def propMetaWithName[L <: LA, F[_]](tpe: L#Type, property: Tracker[Schema[_]])(
      implicit Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F],
      Fw: FrameworkTerms[L, F]
  ): F[core.ResolvedType[L]] =
    propMetaImpl(property)(
      _.refine[core.ResolvedType[L]]({ case schema: ObjectSchema if Option(schema.getProperties).exists(p => !p.isEmpty) => schema })(
        _ => core.Resolved[L](tpe, None, None, None, None)
      ).orRefine({ case c: ComposedSchema => c })(_ => core.Resolved[L](tpe, None, None, None, None))
        .orRefine({ case schema: StringSchema if Option(schema.getEnum).map(_.asScala).exists(_.nonEmpty) => schema })(
          _ => core.Resolved[L](tpe, None, None, None, None)
        )
        .map(_.pure[F])
    )

  private def resolveScalarTypes[L <: LA, F[_]](
      partial: Either[Tracker[Schema[_]], F[core.ResolvedType[L]]]
  )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: SwaggerTerms[L, F], Fw: FrameworkTerms[L, F]): F[core.ResolvedType[L]] = {
    import Sw._
    def buildResolveNoDefault[A <: Schema[_]]: Tracker[A] => F[core.ResolvedType[L]] = { a =>
      val rawType   = a.downField("type", _.getType())
      val rawFormat = a.downField("format", _.getFormat())

      for {
        customTpeName <- customTypeName(a)
        tpe           <- typeName[L, F](rawType, rawFormat, Tracker.cloneHistory(a, customTpeName))
      } yield core.Resolved[L](tpe, None, None, rawType.unwrapTracker, rawFormat.unwrapTracker)
    }

    partial
      .orRefine({ case b: BooleanSchema => b })(buildResolveNoDefault)
      .orRefine({ case s: StringSchema => s })(buildResolveNoDefault)
      .orRefine({ case s: EmailSchema => s })(buildResolveNoDefault)
      .orRefine({ case d: DateSchema => d })(buildResolveNoDefault)
      .orRefine({ case d: DateTimeSchema => d })(buildResolveNoDefault)
      .orRefine({ case i: IntegerSchema => i })(buildResolveNoDefault)
      .orRefine({ case d: NumberSchema => d })(buildResolveNoDefault)
      .orRefine({ case p: PasswordSchema => p })(buildResolveNoDefault)
      .orRefine({ case f: FileSchema => f })(buildResolveNoDefault)
      .orRefine({ case b: BinarySchema => b })(buildResolveNoDefault)
      .orRefine({ case u: UUIDSchema => u })(buildResolveNoDefault)
      .orRefineFallback(x => fallbackPropertyTypeHandler(x).map(core.Resolved[L](_, None, None, None, None))) // This may need to be rawType=string?
  }

  private def enrichWithDefault[L <: LA, F[_]](schema: Tracker[Schema[_]])(
      implicit Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F],
      Fw: FrameworkTerms[L, F]
  ): core.ResolvedType[L] => F[core.ResolvedType[L]] = { resolved =>
    import Sc._
    def buildResolve[B: Extractable, A <: Schema[_]: Default.GetDefault](transformLit: B => F[L#Term]): Tracker[A] => F[core.ResolvedType[L]] = { a =>
      for {
        default <- Default(a.unwrapTracker).extract[B].traverse(transformLit(_))
      } yield resolved match {
        case x: core.Resolved[L] => x.copy(defaultValue = default)
        case other               => other
      }
    }
    schema
      .refine[F[core.ResolvedType[L]]]({ case b: BooleanSchema => b })(buildResolve(litBoolean))
      .orRefine({ case s: StringSchema => s })(buildResolve(litString))
      .orRefineFallback(_ => resolved.pure[F])
  }

  private def propMetaImpl[L <: LA, F[_]](property: Tracker[Schema[_]])(
      strategy: Tracker[Schema[_]] => Either[Tracker[Schema[_]], F[core.ResolvedType[L]]]
  )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: SwaggerTerms[L, F], Fw: FrameworkTerms[L, F]): F[core.ResolvedType[L]] =
    Sw.log.function("propMeta") {
      import Fw._
      import Sc._
      import Cl._
      import Sw._

      log.debug(s"property:\n${log.schemaToString(property.unwrapTracker)} (${property.unwrapTracker.getExtensions()}, ${property.showHistory})") >> (
        strategy(property)
          .orRefine({ case o: ObjectSchema => o })(
            o =>
              for {
                customTpeName <- customTypeName(o)
                customTpe     <- customTpeName.flatTraverse(x => liftCustomType[L, F](Tracker.cloneHistory(o, x)))
                fallback      <- objectType(None)
              } yield core.Resolved[L](customTpe.getOrElse(fallback), None, None, None, None)
          )
          .orRefine({ case arr: ArraySchema => arr })(
            arr =>
              for {
                items <- getItems(arr)
                meta  <- propMetaImpl[L, F](items)(strategy)
                rawType   = arr.downField("type", _.getType())
                rawFormat = arr.downField("format", _.getFormat())
                arrayType <- customArrayTypeName(arr).flatMap(_.flatTraverse(x => parseType(Tracker.cloneHistory(arr, x))))
                res <- meta match {
                  case core.Resolved(inner, dep, default, _, _) =>
                    (liftVectorType(inner, arrayType), default.traverse(liftVectorTerm))
                      .mapN(core.Resolved[L](_, dep, _, rawType.unwrapTracker, rawFormat.unwrapTracker))
                  case x: core.Deferred[L]      => embedArray(x, arrayType)
                  case x: core.DeferredArray[L] => embedArray(x, arrayType)
                  case x: core.DeferredMap[L]   => embedArray(x, arrayType)
                }
              } yield res
          )
          .orRefine({ case map: MapSchema => map })({ map =>
            val rawType   = map.downField("type", _.getType())
            val rawFormat = map.downField("format", _.getFormat())
            for {
              rec <- map
                .downField("additionalProperties", _.getAdditionalProperties())
                .map(_.getOrElse(false))
                .refine[F[core.ResolvedType[L]]]({ case b: java.lang.Boolean => b })(
                  _ => objectType(None).map(core.Resolved[L](_, None, None, rawType.unwrapTracker, rawFormat.unwrapTracker))
                )
                .orRefine({ case s: Schema[_] => s })(s => propMetaImpl[L, F](s)(strategy))
                .orRefineFallback({ s =>
                  log.debug(s"Unknown structure cannot be reflected: ${s.unwrapTracker} (${s.showHistory})") >> objectType(None)
                    .map(core.Resolved[L](_, None, None, rawType.unwrapTracker, rawFormat.unwrapTracker))
                })
              mapType <- customMapTypeName(map).flatMap(_.flatTraverse(x => parseType(Tracker.cloneHistory(map, x))))
              res <- rec match {
                case core.Resolved(inner, dep, _, tpe, fmt) => liftMapType(inner, mapType).map(core.Resolved[L](_, dep, None, tpe, fmt))
                case x: core.DeferredMap[L]                 => embedMap(x, mapType)
                case x: core.DeferredArray[L]               => embedMap(x, mapType)
                case x: core.Deferred[L]                    => embedMap(x, mapType)
              }
            } yield res
          })
          .orRefine({ case ref: Schema[_] if Option(ref.get$ref).isDefined => ref })(ref => getSimpleRef(ref.map(Option.apply _)).map(core.Deferred[L]))
        )
        .pure[F]
        .flatMap(resolveScalarTypes[L, F])
        .flatMap(enrichWithDefault[L, F](property))
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
      .flatTraverse({
        case (schemeName, scheme) =>
          val typeName = CustomTypeName(scheme, prefixes)
          for {
            tpe <- typeName.fold(Option.empty[L#Type].pure[F])(x => parseType(Tracker.cloneHistory(scheme, x)))
            parsedScheme <- scheme.downField("type", _.getType).unwrapTracker.traverse {
              case SwSecurityScheme.Type.APIKEY        => extractApiKeySecurityScheme(schemeName, scheme.unwrapTracker, tpe).widen[SecurityScheme[L]]
              case SwSecurityScheme.Type.HTTP          => extractHttpSecurityScheme(schemeName, scheme.unwrapTracker, tpe).widen[SecurityScheme[L]]
              case SwSecurityScheme.Type.OPENIDCONNECT => extractOpenIdConnectSecurityScheme(schemeName, scheme.unwrapTracker, tpe).widen[SecurityScheme[L]]
              case SwSecurityScheme.Type.OAUTH2        => extractOAuth2SecurityScheme(schemeName, scheme.unwrapTracker, tpe).widen[SecurityScheme[L]]
            }
          } yield parsedScheme.toList.map(scheme => schemeName -> scheme)
      })
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
