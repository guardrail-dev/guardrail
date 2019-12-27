package com.twilio.guardrail

import cats.data.NonEmptyList
import io.swagger.v3.oas.models._
import io.swagger.v3.oas.models.media._
import io.swagger.v3.oas.models.parameters.RequestBody
import io.swagger.v3.oas.models.security.{ SecurityScheme => SwSecurityScheme }
import cats.{ FlatMap, Foldable }
import cats.free.Free
import cats.implicits._
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.core.implicits._
import com.twilio.guardrail.terms.{ ScalaTerms, SecurityScheme, SwaggerTerms }
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.extract.{ CustomArrayTypeName, CustomMapTypeName, CustomTypeName, Default, Extractable, VendorExtension }
import com.twilio.guardrail.extract.VendorExtension.VendorExtensible._
import com.twilio.guardrail.generators.ScalaParameter
import com.twilio.guardrail.languages.{ LA, ScalaLanguage }
import scala.meta._
import com.twilio.guardrail.protocol.terms.protocol.PropMeta
import scala.collection.JavaConverters._

object SwaggerUtil {
  sealed trait ResolvedType[L <: LA]
  case class Resolved[L <: LA](tpe: L#Type, classDep: Option[L#TermName], defaultValue: Option[L#Term], rawType: Option[String], rawFormat: Option[String])
      extends ResolvedType[L]
  sealed trait LazyResolvedType[L <: LA]                                         extends ResolvedType[L]
  case class Deferred[L <: LA](value: String)                                    extends LazyResolvedType[L]
  case class DeferredArray[L <: LA](value: String, containerTpe: Option[L#Type]) extends LazyResolvedType[L]
  case class DeferredMap[L <: LA](value: String, containerTpe: Option[L#Type])   extends LazyResolvedType[L]
  object ResolvedType {
    def resolveReferences[L <: LA, F[_]](
        values: List[(String, ResolvedType[L])]
    )(implicit Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F]): Free[F, List[(String, Resolved[L])]] = Sw.log.function("resolveReferences") {
      import Sc._
      import Sw._
      val (lazyTypes, resolvedTypes) = Foldable[List].partitionEither(values) {
        case (clsName, x: Resolved[L])         => Right((clsName, x))
        case (clsName, x: LazyResolvedType[L]) => Left((clsName, x))
      }

      def lookupTypeName(clsName: String, tpeName: String, resolvedTypes: List[(String, Resolved[L])])(
          f: L#Type => Free[F, L#Type]
      ): Free[F, Option[(String, Resolved[L])]] =
        resolvedTypes
          .find(_._1 == tpeName)
          .map(_._2)
          .traverse(x => f(x.tpe).map(tpe => (clsName, x.copy(tpe = tpe))))

      log.debug(s"resolve ${values.length} references") >> FlatMap[Free[F, ?]]
        .tailRecM[(List[(String, LazyResolvedType[L])], List[(String, Resolved[L])]), List[(String, Resolved[L])]](
          (lazyTypes, resolvedTypes)
        ) {
          case (lazyTypes, resolvedTypes) =>
            if (lazyTypes.isEmpty) {
              Free.pure(Right(resolvedTypes))
            } else {
              def partitionEitherM[G[_], A, B, C](
                  fa: List[A]
              )(f: A => G[Either[B, C]])(implicit A: cats.Alternative[List], M: cats.Monad[G]): G[(List[B], List[C])] = {
                import cats.instances.tuple._

                implicit val mb: cats.Monoid[List[B]] = A.algebra[B]
                implicit val mc: cats.Monoid[List[C]] = A.algebra[C]

                Foldable[List].foldMapM[G, A, (List[B], List[C])](fa)(
                  a =>
                    f(a).map {
                      case Right(c) => (A.empty[B], A.pure(c))
                      case Left(b)  => (A.pure(b), A.empty[C])
                    }
                )
              }

              (partitionEitherM(lazyTypes) {
                case x @ (clsName, Deferred(tpeName)) =>
                  lookupTypeName(clsName, tpeName, resolvedTypes)(Free.pure _).map(Either.fromOption(_, x))
                case x @ (clsName, DeferredArray(tpeName, containerTpe)) =>
                  lookupTypeName(clsName, tpeName, resolvedTypes)(liftVectorType(_, containerTpe)).map(Either.fromOption(_, x))
                case x @ (clsName, DeferredMap(tpeName, containerTpe)) =>
                  lookupTypeName(clsName, tpeName, resolvedTypes)(liftMapType(_, containerTpe)).map(Either.fromOption(_, x))
              }).map({
                case (newLazyTypes, newResolvedTypes) =>
                  Left((newLazyTypes, resolvedTypes ++ newResolvedTypes))
              })
            }
        }
    }

    def resolve[L <: LA, F[_]](
        value: ResolvedType[L],
        protocolElems: List[StrictProtocolElems[L]]
    )(implicit Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F]): Free[F, Resolved[L]] = {
      import Sc._
      import Sw._
      log.debug(s"value: ${value} in ${protocolElems.length} protocol elements") >> (value match {
        case x @ Resolved(_, _, _, _, _) => Free.pure(x)
        case Deferred(name) =>
          resolveType(name, protocolElems)
            .flatMap {
              case RandomType(name, tpe) =>
                Free.pure(Resolved[L](tpe, None, None, None, None))
              case ClassDefinition(name, _, fullType, cls, _, _) =>
                Free.pure(Resolved[L](fullType, None, None, None, None))
              case EnumDefinition(name, _, fullType, elems, cls, _) =>
                Free.pure(Resolved[L](fullType, None, None, Some("string"), None))
              case ADT(_, _, fullType, _, _) =>
                Free.pure(Resolved[L](fullType, None, None, None, None))
            }
        case DeferredArray(name, containerTpe) =>
          resolveType(name, protocolElems)
            .flatMap {
              case RandomType(name, tpe) =>
                liftVectorType(tpe, containerTpe).map(Resolved[L](_, None, None, None, None))
              case ClassDefinition(name, _, fullType, cls, _, _) =>
                liftVectorType(fullType, containerTpe).map(Resolved[L](_, None, None, None, None))
              case EnumDefinition(name, _, fullType, elems, cls, _) =>
                liftVectorType(fullType, containerTpe).map(Resolved[L](_, None, None, None, None))
              case ADT(_, _, fullType, _, _) =>
                liftVectorType(fullType, containerTpe).map(Resolved[L](_, None, None, None, None))
            }
        case DeferredMap(name, containerTpe) =>
          resolveType(name, protocolElems)
            .flatMap {
              case RandomType(name, tpe) =>
                liftMapType(tpe, containerTpe).map(Resolved[L](_, None, None, None, None))
              case ClassDefinition(_, _, fullType, _, _, _) =>
                liftMapType(fullType, containerTpe).map(Resolved[L](_, None, None, None, None))
              case EnumDefinition(_, _, fullType, _, _, _) =>
                liftMapType(fullType, containerTpe).map(Resolved[L](_, None, None, None, None))
              case ADT(_, _, fullType, _, _) =>
                liftMapType(fullType, containerTpe).map(Resolved[L](_, None, None, None, None))
            }
      })
    }
  }

  def customTypeName[L <: LA, F[_], A: VendorExtension.VendorExtensible](v: A)(implicit S: ScalaTerms[L, F]): Free[F, Option[String]] =
    for {
      prefixes <- S.vendorPrefixes()
    } yield CustomTypeName(v, prefixes)

  def customArrayTypeName[L <: LA, F[_], A: VendorExtension.VendorExtensible](v: A)(implicit S: ScalaTerms[L, F]): Free[F, Option[String]] =
    for {
      prefixes <- S.vendorPrefixes()
    } yield CustomArrayTypeName(v, prefixes)

  def customMapTypeName[L <: LA, F[_], A: VendorExtension.VendorExtensible](v: A)(implicit S: ScalaTerms[L, F]): Free[F, Option[String]] =
    for {
      prefixes <- S.vendorPrefixes()
    } yield CustomMapTypeName(v, prefixes)

  sealed class ModelMetaTypePartiallyApplied[L <: LA, F[_]](val dummy: Boolean = true) {
    def apply[T <: Schema[_]](
        model: Tracker[T]
    )(implicit Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F], Fw: FrameworkTerms[L, F]): Free[F, ResolvedType[L]] =
      Sw.log.function("modelMetaType") {
        import Sc._
        import Sw._
        import Fw._
        log.debug(s"model:\n${log.schemaToString(model.get)}") >> (model
          .refine[Free[F, ResolvedType[L]]]({ case ref: Schema[_] if Option(ref.get$ref).isDefined => ref })(
            ref =>
              for {
                ref <- getSimpleRef(ref.map(Option(_)))
              } yield Deferred[L](ref)
          )
          .orRefine({ case arr: ArraySchema => arr })(
            arr =>
              for {
                items     <- getItems(arr)
                _         <- log.debug(s"items:\n${log.schemaToString(items.unwrapTracker)}")
                meta      <- propMeta[L, F](items)
                _         <- log.debug(s"meta: ${meta}")
                arrayType <- customArrayTypeName(arr).flatMap(_.flatTraverse(parseType))
                res <- meta match {
                  case Resolved(inner, dep, default, _, _) =>
                    (liftVectorType(inner, arrayType), default.traverse(liftVectorTerm(_))).mapN(Resolved[L](_, dep, _, None, None))
                  case x: Deferred[L]      => embedArray(x, arrayType)
                  case x: DeferredArray[L] => embedArray(x, arrayType)
                  case x: DeferredMap[L]   => embedArray(x, arrayType)
                }
              } yield res
          )
          .orRefine({ case map: MapSchema => map })({ map =>
            for {
              rec <- map
                .downField("additionalProperties", _.getAdditionalProperties())
                .map(_.getOrElse(false))
                .refine[Free[F, ResolvedType[L]]]({ case b: java.lang.Boolean => b })(_ => objectType(None).map(Resolved[L](_, None, None, None, None)))
                .orRefine({ case s: Schema[_] => s })(propMeta[L, F](_))
                .orRefineFallback({ s =>
                  log.debug(s"Unknown structure cannot be reflected: ${s.unwrapTracker} (${s.showHistory})") >> objectType(None)
                    .map(Resolved[L](_, None, None, None, None))
                })
              mapType <- customMapTypeName(map).flatMap(_.flatTraverse(parseType))
              res <- rec match {
                case Resolved(inner, dep, _, _, _) => liftMapType(inner, mapType).map(Resolved[L](_, dep, None, None, None))
                case x: DeferredMap[L]             => embedMap(x, mapType)
                case x: DeferredArray[L]           => embedMap(x, mapType)
                case x: Deferred[L]                => embedMap(x, mapType)
              }
            } yield res
          })
          .orRefineFallback(
            impl =>
              for {
                tpeName       <- getType(impl)
                customTpeName <- customTypeName(impl)
                fmt = impl.downField("format", _.getFormat())
                tpe <- typeName[L, F](tpeName.map(Option(_)), fmt, customTpeName)
              } yield Resolved[L](tpe, None, None, Some(tpeName.get), fmt.get)
          ))
      }
  }

  def modelMetaType[L <: LA, F[_]]: ModelMetaTypePartiallyApplied[L, F] = new ModelMetaTypePartiallyApplied[L, F]()

  def extractConcreteTypes[L <: LA, F[_]](
      definitions: List[(String, Tracker[Schema[_]])]
  )(implicit Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F], F: FrameworkTerms[L, F]): Free[F, List[PropMeta[L]]] = {
    import Sc._
    for {
      entries <- definitions.traverse[Free[F, ?], (String, SwaggerUtil.ResolvedType[L])] {
        case (clsName, schema) =>
          schema
            .refine({ case impl: Schema[_] if (Option(impl.getProperties()).isDefined || Option(impl.getEnum()).isDefined) => impl })(
              impl =>
                pureTypeName(clsName)
                  .flatMap(widenTypeName)
                  .map(x => (clsName, SwaggerUtil.Resolved[L](x, None, None, None, None): SwaggerUtil.ResolvedType[L]))
            )
            .orRefine({ case comp: ComposedSchema => comp })(
              comp =>
                for {
                  x <- pureTypeName(clsName).flatMap(widenTypeName)
                  parentSimpleRef = comp.downField("allOf", _.getAllOf).map(_.headOption).flatDownField("$ref", _.get$ref).unwrapTracker.map(_.split("/").last)
                  parentTerm <- parentSimpleRef.traverse(n => pureTermName(n))
                  resolvedType = SwaggerUtil.Resolved[L](x, parentTerm, None, None, None): SwaggerUtil.ResolvedType[L]
                } yield (clsName, resolvedType)
            )
            .getOrElse(
              for {
                resolved <- SwaggerUtil.modelMetaType[L, F](schema)
              } yield (clsName, resolved)
            )
      }
      result <- SwaggerUtil.ResolvedType.resolveReferences[L, F](entries)
    } yield result.map {
      case (clsName, SwaggerUtil.Resolved(tpe, _, _, _, _)) =>
        PropMeta[L](clsName, tpe)
    }
  }

  // Standard type conversions, as documented in http://swagger.io/specification/#data-types-12
  def typeName[L <: LA, F[_]](
      typeName: Tracker[Option[String]],
      format: Tracker[Option[String]],
      customType: Option[String]
  )(implicit Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F], Fw: FrameworkTerms[L, F]): Free[F, L#Type] =
    Sw.log.function(s"typeName(${typeName.unwrapTracker}, ${format.unwrapTracker}, ${customType})") {
      import Sc._
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
        customTpe <- customType.flatTraverse(liftCustomType[L, F] _)
        result <- customTpe.fold({
          (typeName.get, format.get) match {
            case (Some("string"), Some("uuid"))         => uuidType()
            case (Some("string"), Some("password"))     => stringType(None)
            case (Some("string"), Some("email"))        => stringType(None)
            case (Some("string"), Some("date"))         => dateType()
            case (Some("string"), Some("date-time"))    => dateTimeType()
            case (Some("string"), fmt @ Some("binary")) => fileType(None).map(log(fmt, _))
            case (Some("string"), fmt)                  => stringType(fmt).map(log(fmt, _))
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
        })(Free.pure(_))
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
      implicit Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F],
      Fw: FrameworkTerms[L, F]
  ): Free[F, ResolvedType[L]] = {
    import Fw._
    propMetaImpl(property) {
      case o: ObjectSchema =>
        for {
          customTpeName <- customTypeName(o)
          customTpe     <- customTpeName.flatTraverse(liftCustomType[L, F] _)
          fallback      <- objectType(None)
        } yield Resolved[L](customTpe.getOrElse(fallback), None, None, None, None)
    }
  }

  def liftCustomType[L <: LA, F[_]](s: String)(implicit Sc: ScalaTerms[L, F]): Free[F, Option[L#Type]] = {
    import Sc._
    val tpe = s.trim
    if (tpe.nonEmpty) {
      parseType(tpe)
    } else Free.pure(Option.empty[L#Type])
  }

  def propMetaWithName[L <: LA, F[_]](tpe: L#Type, property: Tracker[Schema[_]])(
      implicit Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F],
      Fw: FrameworkTerms[L, F]
  ): Free[F, ResolvedType[L]] = {
    import Fw._
    propMetaImpl(property) {
      case schema: ObjectSchema if Option(schema.getProperties).exists(p => !p.isEmpty) =>
        Free.pure(Resolved[L](tpe, None, None, None, None))
      case o: ObjectSchema =>
        for {
          customTpeName <- customTypeName(o)
          customTpe     <- customTpeName.flatTraverse(liftCustomType[L, F] _)
          fallback      <- objectType(None)
        } yield Resolved[L](customTpe.getOrElse(fallback), None, None, None, None)
      case _: ComposedSchema =>
        Free.pure(Resolved[L](tpe, None, None, None, None))
      case schema: StringSchema if Option(schema.getEnum).map(_.asScala).exists(_.nonEmpty) =>
        Free.pure(Resolved[L](tpe, None, None, None, None))
    }
  }

  private def propMetaImpl[L <: LA, F[_]](property: Tracker[Schema[_]])(
      strategy: PartialFunction[Schema[_], Free[F, ResolvedType[L]]]
  )(implicit Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F], Fw: FrameworkTerms[L, F]): Free[F, ResolvedType[L]] =
    Sw.log.function("propMeta") {
      import Fw._
      import Sc._
      import Sw._

      def buildResolveNoDefault[A <: Schema[_]]: Tracker[A] => Free[F, ResolvedType[L]] = { a =>
        val rawType   = a.downField("type", _.getType())
        val rawFormat = a.downField("format", _.getFormat())

        for {
          customTpeName <- customTypeName(a)
          tpe           <- typeName[L, F](rawType, rawFormat, customTpeName)
        } yield Resolved[L](tpe, None, None, rawType.get, rawFormat.get)
      }
      def buildResolve[B: Extractable, A <: Schema[_]: Default.GetDefault](transformLit: B => Free[F, L#Term]): Tracker[A] => Free[F, ResolvedType[L]] = { a =>
        val rawType   = a.downField("type", _.getType())
        val rawFormat = a.downField("format", _.getFormat())
        for {
          customTpeName <- customTypeName(a)
          res <- (
            typeName[L, F](rawType, rawFormat, customTpeName),
            Default(a.get).extract[B].traverse(transformLit(_))
          ).mapN(Resolved[L](_, None, _, rawType.get, rawFormat.get))
        } yield res
      }

      log.debug(s"property:\n${log.schemaToString(property.get)} (${property.get.getExtensions()})").flatMap { _ =>
        property
          .refine[Free[F, ResolvedType[L]]](strategy)(_.get)
          .orRefine({ case a: ArraySchema => a })(
            p =>
              for {
                items     <- getItems(p)
                rec       <- propMetaImpl[L, F](items)(strategy)
                arrayType <- customArrayTypeName(p).flatMap(_.flatTraverse(parseType))
                res <- rec match {
                  case Resolved(inner, dep, default, _, _) =>
                    (liftVectorType(inner, arrayType), default.traverse(liftVectorTerm))
                      .mapN(Resolved[L](_, dep, _, None, None): ResolvedType[L])
                  case x: DeferredMap[L]   => embedArray(x, arrayType)
                  case x: DeferredArray[L] => embedArray(x, arrayType)
                  case x: Deferred[L]      => embedArray(x, arrayType)
                }
              } yield res
          )
          .orRefine({ case m: MapSchema => m })(
            m =>
              for {
                rec <- m
                  .downField("additionalProperties", _.getAdditionalProperties())
                  .map(_.getOrElse(false))
                  .refine[Free[F, ResolvedType[L]]]({ case b: java.lang.Boolean => b })(_ => objectType(None).map(Resolved[L](_, None, None, None, None)))
                  .orRefine({ case s: Schema[_] => s })(propMetaImpl[L, F](_)(strategy))
                  .orRefineFallback({ s =>
                    log.debug(s"Unknown structure cannot be reflected: ${s.unwrapTracker} (${s.showHistory})") >> objectType(None).map(
                      Resolved[L](_, None, None, None, None)
                    )
                  })
                mapType <- customArrayTypeName(m).flatMap(_.flatTraverse(parseType))
                res <- rec match {
                  case Resolved(inner, dep, _, _, _) =>
                    liftMapType(inner, mapType).map(Resolved[L](_, dep, None, None, None))
                  case x: DeferredMap[L]   => embedMap(x, mapType)
                  case x: DeferredArray[L] => embedMap(x, mapType)
                  case x: Deferred[L]      => embedMap(x, mapType)
                }
              } yield res
          )
          .orRefine({ case ref: Schema[_] if Option(ref.get$ref).isDefined => ref })(ref => getSimpleRef(ref.map(Option.apply _)).map(Deferred[L]))
          .orRefine({ case b: BooleanSchema => b })(buildResolve(litBoolean))
          .orRefine({ case s: StringSchema => s })(buildResolve(litString))
          .orRefine({ case s: EmailSchema => s })(buildResolveNoDefault)
          .orRefine({ case d: DateSchema => d })(buildResolveNoDefault)
          .orRefine({ case d: DateTimeSchema => d })(buildResolveNoDefault)
          .orRefine({ case i: IntegerSchema => i })(buildResolveNoDefault)
          .orRefine({ case d: NumberSchema => d })(buildResolveNoDefault)
          .orRefine({ case p: PasswordSchema => p })(buildResolveNoDefault)
          .orRefine({ case f: FileSchema => f })(buildResolveNoDefault)
          .orRefine({ case u: UUIDSchema => u })(buildResolveNoDefault)
          .orRefineFallback(x => fallbackPropertyTypeHandler(x).map(Resolved[L](_, None, None, None, None))) // This may need to be rawType=string?
      }
    }

  def extractSecuritySchemes[L <: LA, F[_]](
      swagger: OpenAPI,
      prefixes: List[String]
  )(implicit Sw: SwaggerTerms[L, F], Sc: ScalaTerms[L, F]): Free[F, Map[String, SecurityScheme[L]]] = {
    import Sw._
    import Sc._

    Option(swagger.getComponents)
      .flatMap(components => Option(components.getSecuritySchemes))
      .fold(Map.empty[String, SwSecurityScheme])(_.asScala.toMap)
      .toList
      .traverse({
        case (schemeName, scheme) =>
          val typeName = CustomTypeName(scheme, prefixes)
          for {
            tpe <- typeName.fold(Free.pure[F, Option[L#Type]](Option.empty[L#Type]))(parseType)
            parsedScheme <- scheme.getType match {
              case SwSecurityScheme.Type.APIKEY        => extractApiKeySecurityScheme(schemeName, scheme, tpe).widen[SecurityScheme[L]]
              case SwSecurityScheme.Type.HTTP          => extractHttpSecurityScheme(schemeName, scheme, tpe).widen[SecurityScheme[L]]
              case SwSecurityScheme.Type.OPENIDCONNECT => extractOpenIdConnectSecurityScheme(schemeName, scheme, tpe).widen[SecurityScheme[L]]
              case SwSecurityScheme.Type.OAUTH2        => extractOAuth2SecurityScheme(schemeName, scheme, tpe).widen[SecurityScheme[L]]
            }
          } yield (schemeName, parsedScheme)
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

  object paths {
    import atto._, Atto._

    private[this] def lookupName[L <: LA, T](bindingName: String, pathArgs: List[ScalaParameter[L]])(f: ScalaParameter[L] => atto.Parser[T]): atto.Parser[T] =
      pathArgs
        .find(_.argName.value == bindingName)
        .fold[atto.Parser[T]](
          err(s"Unable to find argument ${bindingName}")
        )(param => f(param))

    private[this] val variable: atto.Parser[String] = char('{') ~> many(notChar('}'))
            .map(_.mkString("")) <~ char('}')

    def generateUrlPathParams[L <: LA](
        path: Tracker[String],
        pathArgs: List[ScalaParameter[L]],
        showLiteralPathComponent: String => L#Term,
        showInterpolatedPathComponent: L#TermName => L#Term,
        initialPathTerm: L#Term,
        combinePathTerms: (L#Term, L#Term) => L#Term
    ): Target[L#Term] = {
      val term: atto.Parser[L#Term] = variable.flatMap { binding =>
        lookupName(binding, pathArgs) { param =>
          ok(showInterpolatedPathComponent(param.paramName))
        }
      }
      val other: atto.Parser[String]                         = many1(notChar('{')).map(_.toList.mkString)
      val pattern: atto.Parser[List[Either[String, L#Term]]] = many(either(term, other).map(_.swap: Either[String, L#Term]))

      for {
        parts <- path.map(pattern.parseOnly(_).either).raiseErrorIfLeft
        result = parts.get
          .map({
            case Left(part)  => showLiteralPathComponent(part)
            case Right(term) => term
          })
          .foldLeft[L#Term](initialPathTerm)((a, b) => combinePathTerms(a, b))
      } yield result
    }

    class Extractors[T, TN <: T](
        pathSegmentConverter: (ScalaParameter[ScalaLanguage], Option[T]) => Either[String, T],
        buildParamConstraint: ((String, String)) => T,
        joinParams: (T, T) => T,
        stringPath: String => T,
        liftBinding: Term.Name => TN,
        litRegex: (String, Term.Name, String) => T
    ) {
      // (Option[TN], T) is (Option[Binding], Segment)
      type P  = Parser[(Option[TN], T)]
      type LP = Parser[List[(Option[TN], T)]]

      val plainString      = many(noneOf("{}/?")).map(_.mkString)
      val plainNEString    = many1(noneOf("{}/?")).map(_.toList.mkString)
      val stringSegment: P = plainNEString.map(s => (None, stringPath(s)))
      def regexSegment(implicit pathArgs: List[ScalaParameter[ScalaLanguage]]): P =
        (plainString ~ variable ~ plainString).flatMap {
          case ((before, binding), after) =>
            lookupName[ScalaLanguage, (Option[TN], T)](binding, pathArgs) {
              case param @ ScalaParameter(_, _, paramName, argName, _) =>
                val value = if (before.nonEmpty || after.nonEmpty) {
                  pathSegmentConverter(param, Some(litRegex(before.mkString, paramName, after.mkString)))
                    .fold(err, ok)
                } else {
                  pathSegmentConverter(param, None).fold(err, ok)
                }
                value.map((Some(liftBinding(paramName)), _))
            }
        }

      def segments(implicit pathArgs: List[ScalaParameter[ScalaLanguage]]): LP =
        sepBy1(choice(regexSegment(pathArgs), stringSegment), char('/'))
          .map(_.toList)

      val qsValueOnly: Parser[(String, String)] = ok("") ~ (char('=') ~> opt(many(noneOf("&")))
                  .map(_.fold("")(_.mkString)))
      val staticQSArg: Parser[(String, String)] = many1(noneOf("=&"))
          .map(_.toList.mkString) ~ opt(char('=') ~> many(noneOf("&")))
              .map(_.fold("")(_.mkString))
      val staticQSTerm: Parser[T] =
        choice(staticQSArg, qsValueOnly).map(buildParamConstraint)
      val queryPart: Parser[T]                                               = sepBy1(staticQSTerm, char('&')).map(_.reduceLeft(joinParams))
      val leadingSlash: Parser[Option[Char]]                                 = opt(char('/'))
      val trailingSlash: Parser[Boolean]                                     = opt(char('/')).map(_.nonEmpty)
      val staticQS: Parser[Option[T]]                                        = (char('?') ~> queryPart.map(Option.apply _)) | char('?').map(_ => Option.empty[T]) | ok(Option.empty[T])
      val emptyPath: Parser[(List[(Option[TN], T)], (Boolean, Option[T]))]   = ok((List.empty[(Option[TN], T)], (false, None)))
      val emptyPathQS: Parser[(List[(Option[TN], T)], (Boolean, Option[T]))] = ok(List.empty[(Option[TN], T)]) ~ (ok(false) ~ staticQS)
      def pattern(implicit pathArgs: List[ScalaParameter[ScalaLanguage]]): Parser[(List[(Option[TN], T)], (Boolean, Option[T]))] =
        opt(leadingSlash) ~> ((segments ~ (trailingSlash ~ staticQS)) | emptyPathQS | emptyPath) <~ endOfInput
      def runParse(path: Tracker[String], pathArgs: List[ScalaParameter[ScalaLanguage]]): Target[(List[(Option[TN], T)], (Boolean, Option[T]))] =
        pattern(pathArgs)
          .parse(path.unwrapTracker)
          .done match {
          case ParseResult.Done(input, result)         => Target.pure(result)
          case ParseResult.Fail(input, stack, message) => Target.raiseError(s"Failed to parse URL: ${message} (unparsed: ${input}) (${path.showHistory})")
          case ParseResult.Partial(k)                  => Target.raiseError(s"Unexpected parser state attempting to parse ${path} (${path.showHistory})")
        }
    }

    object akkaExtractor
        extends Extractors[Term, Term.Name](
          pathSegmentConverter = {
            case (ScalaParameter(_, param, _, argName, argType), base) =>
              base.fold {
                argType match {
                  case t"String" => Right(q"Segment")
                  case t"Double" => Right(q"DoubleNumber")
                  case t"BigDecimal" =>
                    Right(q"Segment.map(BigDecimal.apply _)")
                  case t"Int"    => Right(q"IntNumber")
                  case t"Long"   => Right(q"LongNumber")
                  case t"BigInt" => Right(q"Segment.map(BigInt.apply _)")
                  case tpe =>
                    Right(q"Segment.flatMap(str => io.circe.Json.fromString(str).as[${tpe}].toOption)")
                }
              } { segment =>
                argType match {
                  case t"String" => Right(segment)
                  case t"BigDecimal" =>
                    Right(q"${segment}.map(BigDecimal.apply _)")
                  case t"BigInt" => Right(q"${segment}.map(BigInt.apply _)")
                  case tpe =>
                    Right(q"${segment}.flatMap(str => io.circe.Json.fromString(str).as[${tpe}].toOption)")
                }
              }
          },
          buildParamConstraint = {
            case (k, v) =>
              q" parameter(${Lit.String(k)}).require(_ == ${Lit.String(v)}) "
          },
          joinParams = { (l, r) =>
            q"${l} & ${r}"
          },
          stringPath = Lit.String(_),
          liftBinding = identity,
          litRegex = (before, _, after) =>
            q"""new scala.util.matching.Regex("^" + ${Lit
              .String(before)} + "(.*)" + ${Lit.String(after)} + ${Lit.String("$")})"""
        )

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    object http4sExtractor
        extends Extractors[Pat, Term.Name](
          pathSegmentConverter = {
            case (ScalaParameter(_, param, paramName, argName, argType), base) =>
              base.fold[Either[String, Pat]] {
                argType match {
                  case t"Int"                         => Right(p"IntVar(${Pat.Var(paramName)})")
                  case t"Long"                        => Right(p"LongVar(${Pat.Var(paramName)})")
                  case t"String"                      => Right(Pat.Var(paramName))
                  case t"java.util.UUID"              => Right(p"UUIDVar(${Pat.Var(paramName)})")
                  case Type.Name(tpe)                 => Right(p"${Term.Name(s"${tpe}Var")}(${Pat.Var(paramName)})")
                  case Type.Select(_, Type.Name(tpe)) => Right(p"${Term.Name(s"${tpe}Var")}(${Pat.Var(paramName)})")
                  case tpe =>
                    println(s"Doing our best turning ${tpe} into an extractor")
                    Right(p"${Term.Name(s"${tpe}Var")}(${Pat.Var(paramName)})")
                }
              } { _ =>
                //todo add support for regex segment
                Left("Unsupported feature")
              }
          },
          buildParamConstraint = {
            case (k, v) =>
              p"${Term.Name(s"${k.capitalize}Matcher")}(${Lit.String(v)})"
          },
          joinParams = { (l, r) =>
            p"${l} +& ${r}"
          },
          stringPath = Lit.String(_),
          liftBinding = identity,
          litRegex = (before, _, after) =>
            //todo add support for regex segment
            throw new UnsupportedOperationException
        )

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    object endpointsExtractor
        extends Extractors[Term, Term.Name](
          pathSegmentConverter = {
            case (ScalaParameter(_, param, paramName, argName, argType), base) =>
              base.fold[Either[String, Term]] {
                import com.twilio.guardrail.generators.syntax.Scala._
                Right(q"showSegment[$argType](${argName.toLit}, None)")
              } { _ =>
                //todo add support for regex segment
                Left("Unsupported feature")
              }
          },
          buildParamConstraint = {
            case (k, v) =>
              q"showStaticQs[String](${Lit.String(k)}, ${Lit.String(v)})"
          },
          joinParams = { (l, r) =>
            q"${l} & ${r}"
          },
          stringPath = Lit.String(_),
          liftBinding = identity,
          litRegex = (before, _, after) =>
            //todo add support for regex segment
            throw new UnsupportedOperationException
        )

    def generateUrlAkkaPathExtractors(path: Tracker[String], pathArgs: List[ScalaParameter[ScalaLanguage]]): Target[NonEmptyList[(Term, List[Term.Name])]] = {
      import akkaExtractor._
      for {
        (parts, (trailingSlash, queryParams)) <- runParse(path, pathArgs)
        allPairs = parts
          .foldLeft[NonEmptyList[(Term, List[Term.Name])]](NonEmptyList.one((q"pathEnd", List.empty)))({
            case (NonEmptyList((q"pathEnd   ", bindings), xs), (termName, b)) =>
              NonEmptyList((q"path(${b}       )", bindings ++ termName), xs)
            case (NonEmptyList((q"path(${a })", bindings), xs), (termName, c)) =>
              val newBindings = bindings ++ termName
              if (newBindings.length < 22) {
                NonEmptyList((q"path(${a} / ${c})", newBindings), xs)
              } else {
                NonEmptyList((q"pathEnd", List.empty), (q"pathPrefix(${a} / ${c})", newBindings) :: xs)
              }
          })
        trailingSlashed = if (trailingSlash) {
          allPairs match {
            case NonEmptyList((q"path(${a })", bindings), xs) => NonEmptyList((q"pathPrefix(${a}) & pathEndOrSingleSlash", bindings), xs)
            case NonEmptyList((q"pathEnd", bindings), xs)     => NonEmptyList((q"pathEndOrSingleSlash", bindings), xs)
          }
        } else allPairs
        result = queryParams.fold(trailingSlashed) { qs =>
          val NonEmptyList((directives, bindings), xs) = trailingSlashed
          NonEmptyList((q"${directives} & ${qs}", bindings), xs)
        }
      } yield result.reverse
    }

    def generateUrlHttp4sPathExtractors(path: Tracker[String], pathArgs: List[ScalaParameter[ScalaLanguage]]): Target[(Pat, Option[Pat])] = {
      import http4sExtractor._
      for {
        (parts, (trailingSlash, queryParams)) <- runParse(path, pathArgs)
        (directive, bindings) = parts
          .foldLeft[(Pat, List[Term.Name])]((p"${Term.Name("Root")}", List.empty))({
            case ((acc, bindings), (termName, c)) =>
              (p"$acc / ${c}", bindings ++ termName)
          })
        trailingSlashed = if (trailingSlash) {
          p"$directive / ${Lit.String("")}"
        } else directive
      } yield (trailingSlashed, queryParams)
    }

    def generateUrlEndpointsPathExtractors(path: Tracker[String], pathArgs: List[ScalaParameter[ScalaLanguage]]): Target[(Term, Option[Term])] = {
      import endpointsExtractor._
      for {
        (parts, (trailingSlash, queryParams)) <- path.map(pattern(pathArgs).parseOnly(_).either).raiseErrorIfLeft.map(_.unwrapTracker)
        (directive, bindings) = parts
          .foldLeft[(Term, List[Term.Name])]((q"pathRoot", List.empty))({
            case ((acc, bindings), (termName, c)) =>
              (q"$acc / ${c}", bindings ++ termName)
          })
        trailingSlashed = if (trailingSlash) {
          q"$directive / path"
        } else directive
      } yield (trailingSlashed, queryParams)
    }
  }
}
