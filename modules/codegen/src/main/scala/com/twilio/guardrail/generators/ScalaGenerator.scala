package com.twilio.guardrail.generators

import cats.Monad
import cats.data.NonEmptyList
import cats.implicits._
import cats.~>
import com.twilio.guardrail.Common.resolveFile
import com.twilio.guardrail.SwaggerUtil.LazyResolvedType
import com.twilio.guardrail._
import com.twilio.guardrail.generators.syntax.RichString
import com.twilio.guardrail.generators.syntax.Scala._
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.terms._
import java.nio.charset.StandardCharsets
import java.nio.file.Path

import scala.meta._

object ScalaGenerator {
  private def sourceToBytes(source: Source): Array[Byte] = (GENERATED_CODE_COMMENT + source.syntax).getBytes(StandardCharsets.UTF_8)

  object ScalaInterp extends (ScalaTerm[ScalaLanguage, ?] ~> Target) {
    // TODO: Very interesting bug. 2.11.12 barfs if these two definitions are
    // defined inside `apply`. Once 2.11 is dropped, these can be moved back.
    val matchImplicit: PartialFunction[Stat, Defn.Val] = {
      case x: Defn.Val if (x match { case q"implicit val $_: $_ = $_" => true; case _ => false }) => x
    }
    val partitionImplicits: PartialFunction[Stat, Boolean] = matchImplicit.andThen(_ => true).orElse({ case _ => false })

    val buildPkgTerm: List[String] => Term.Ref =
      _.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)

    type F[A] = Target[A]
    type L    = ScalaLanguage

    implicit def MonadF: Monad[F]         = Target.targetInstances
    def vendorPrefixes(): F[List[String]] = Target.pure(List("x-scala", "x-jvm"))

    def litString(value: String): F[L#Term]                                 = Target.pure(Lit.String(value))
    def litFloat(value: Float): F[L#Term]                                   = Target.pure(Lit.Float(value))
    def litDouble(value: Double): F[L#Term]                                 = Target.pure(Lit.Double(value))
    def litInt(value: Int): F[L#Term]                                       = Target.pure(Lit.Int(value))
    def litLong(value: Long): F[L#Term]                                     = Target.pure(Lit.Long(value))
    def litBoolean(value: Boolean): F[L#Term]                               = Target.pure(Lit.Boolean(value))
    def liftOptionalType(value: L#Type): F[L#Type]                          = Target.pure(t"Option[$value]")
    def liftOptionalTerm(value: L#Term): F[L#Term]                          = Target.pure(q"Option($value)")
    def emptyArray(): F[L#Term]                                             = Target.pure(q"Vector.empty")
    def emptyMap(): F[L#Term]                                               = Target.pure(q"Map.empty")
    def emptyOptionalTerm(): F[L#Term]                                      = Target.pure(q"None")
    def liftVectorType(value: L#Type, customTpe: Option[L#Type]): F[L#Type] = Target.pure(t"${customTpe.getOrElse(t"Vector")}[$value]")
    def liftVectorTerm(value: L#Term): F[L#Term]                            = Target.pure(q"Vector($value)")
    def liftMapType(value: L#Type, customTpe: Option[L#Type]): F[L#Type]    = Target.pure(t"${customTpe.getOrElse(t"Map")}[String, $value]")

    def fullyQualifyPackageName(rawPkgName: List[String]): F[List[String]] = Target.pure("_root_" +: rawPkgName)

    def lookupEnumDefaultValue(tpe: L#TypeName, defaultValue: L#Term, values: List[(String, L#TermName, L#TermSelect)]): F[L#TermSelect] =
      defaultValue match {
        case Lit.String(name) =>
          values
            .find(_._1 == name)
            .fold(Target.raiseUserError[Term.Select](s"Enumeration $tpe is not defined for default value $name"))(value => Target.pure(value._3))
        case _ =>
          Target.raiseUserError[Term.Select](s"Enumeration $tpe somehow has a default value that isn't a string")
      }
    def formatEnumName(enumValue: String): F[String] = Target.pure(enumValue.toPascalCase)

    def embedArray(tpe: LazyResolvedType[L], containerTpe: Option[L#Type]): F[LazyResolvedType[L]] = tpe match {
      case SwaggerUtil.Deferred(tpe) =>
        Target.pure(SwaggerUtil.DeferredArray[L](tpe, containerTpe))
      case SwaggerUtil.DeferredArray(_, _) =>
        Target.raiseUserError("FIXME: Got an Array of Arrays, currently not supported")
      case SwaggerUtil.DeferredMap(_, _) =>
        Target.raiseUserError("FIXME: Got an Array of Maps, currently not supported")
    }
    def embedMap(tpe: LazyResolvedType[L], containerTpe: Option[L#Type]): F[LazyResolvedType[L]] = tpe match {
      case SwaggerUtil.Deferred(inner) =>
        Target.pure(SwaggerUtil.DeferredMap[L](inner, containerTpe))
      case SwaggerUtil.DeferredMap(_, _) =>
        Target.raiseUserError("FIXME: Got a map of maps, currently not supported")
      case SwaggerUtil.DeferredArray(_, _) =>
        Target.raiseUserError("FIXME: Got a map of arrays, currently not supported")
    }

    def parseType(tpe: String): F[Option[L#Type]] =
      Target.pure(
        tpe
          .parse[Type]
          .fold({ err =>
            println(s"Warning: Unparsable x-scala-type: $tpe $err")
            None
          }, Option.apply _)
      )
    def parseTypeName(tpe: String): F[Option[L#TypeName]] = Target.pure(Option(tpe.trim).filterNot(_.isEmpty).map(Type.Name(_)))
    def pureTermName(tpe: String): F[L#TermName] =
      Target.fromOption(Option(tpe.trim).filterNot(_.isEmpty).map(Term.Name(_)), UserError("A structure's name is empty"))
    def pureTypeName(tpe: String): F[L#TypeName] =
      Target.fromOption(Option(tpe.trim).filterNot(_.isEmpty).map(Type.Name(_)), UserError("A structure's name is empty"))

    def pureMethodParameter(name: L#TermName, tpe: L#Type, default: Option[L#Term]): F[L#MethodParameter] =
      Target.pure(param"$name: $tpe".copy(default = default))
    def typeNamesEqual(a: L#TypeName, b: L#TypeName): F[Boolean] = Target.pure(a.value == b.value)
    def typesEqual(a: L#Type, b: L#Type): F[Boolean]             = Target.pure(a.structure == b.structure)
    def extractTypeName(tpe: L#Type): F[Option[L#TypeName]] =
      Target.pure(tpe match {
        case x: Type.Name =>
          Option(x)
        case _ =>
          Option.empty
      })
    def extractTermName(term: L#TermName): F[String] = {
      val Term.Name(name) = term
      Target.pure(name)
    }
    def selectType(typeNames: NonEmptyList[String]): F[L#Type] = {
      val tpe   = Type.Name(typeNames.last)
      val names = typeNames.init.map(Term.Name.apply _)
      val result = names match {
        case Nil =>
          tpe
        case x :: xs =>
          val term = xs.foldLeft[Term.Ref](x)(Term.Select.apply _)
          Type.Select(term, tpe)
      }
      Target.pure(result)
    }
    def selectTerm(termNames: NonEmptyList[String]): F[L#Term] = {
      val result = termNames.tail.foldLeft[Term](Term.Name(termNames.head))({
        case (current, next) =>
          Term.Select(current, Term.Name(next))
      })
      Target.pure(result)
    }
    def alterMethodParameterName(param: L#MethodParameter, name: L#TermName): F[L#MethodParameter] = Target.pure(param.copy(name = name))

    def bytesType(): F[L#Type]                                               = Target.pure(t"Base64String")
    def uuidType(): F[L#Type]                                                = Target.pure(t"java.util.UUID")
    def dateType(): F[L#Type]                                                = Target.pure(t"java.time.LocalDate")
    def dateTimeType(): F[L#Type]                                            = Target.pure(t"java.time.OffsetDateTime")
    def stringType(format: Option[String]): F[L#Type]                        = Target.pure(format.fold(t"String")(Type.Name(_)))
    def floatType(): F[L#Type]                                               = Target.pure(t"Float")
    def doubleType(): F[L#Type]                                              = Target.pure(t"Double")
    def numberType(format: Option[String]): F[L#Type]                        = Target.pure(t"BigDecimal")
    def intType(): F[L#Type]                                                 = Target.pure(t"Int")
    def longType(): F[L#Type]                                                = Target.pure(t"Long")
    def integerType(format: Option[String]): F[L#Type]                       = Target.pure(t"BigInt")
    def booleanType(format: Option[String]): F[L#Type]                       = Target.pure(t"Boolean")
    def arrayType(format: Option[String]): F[L#Type]                         = Target.pure(t"Iterable[String]")
    def fallbackType(tpe: Option[String], format: Option[String]): F[L#Type] = Target.fromOption(tpe, UserError("Missing type")).map(Type.Name(_))

    def widenTypeName(tpe: L#TypeName): F[L#Type]                         = Target.pure(tpe)
    def widenTermSelect(value: L#TermSelect): F[L#Term]                   = Target.pure(value)
    def widenClassDefinition(value: L#ClassDefinition): F[L#Definition]   = Target.pure(value)
    def widenObjectDefinition(value: L#ObjectDefinition): F[L#Definition] = Target.pure(value)

    def findCommonDefaultValue(history: String, a: Option[L#Term], b: Option[L#Term]): F[Option[L#Term]] = (a, b) match {
      case (Some(va), Some(vb)) =>
        if (va.structure == vb.structure) {
          Target.pure(Some(va))
        } else {
          Target.raiseUserError(
            s"There is a mismatch at $history between default values $va and $vb. This parameter is defined at multiple places and those definitions are incompatible with each other. They must have the same name, type and default value. ($history)"
          )
        }
      case (va, vb) =>
        Target.pure(va.orElse(vb))
    }
    def findCommonRawType(history: String, a: RawParameterType, b: RawParameterType): F[RawParameterType] =
      if (a == b) {
        Target.pure(a)
      } else {
        Target.raiseUserError(
          s"There is a mismatch at $history between types $a and $b. Conflicting definitions between types and inherited types are not supported."
        )
      }

    def renderImplicits(
        pkgPath: Path,
        pkgName: List[String],
        frameworkImports: List[L#Import],
        jsonImports: List[L#Import],
        customImports: List[L#Import]
    ): F[Option[WriteTree]] = {
      val pkg: Term.Ref = pkgName.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)
      val implicits     = source"""
            package $pkg

            ..$jsonImports

            import cats.implicits._
            import cats.data.EitherT

            object Implicits {
              abstract class AddArg[T] {
                def addArg(key: String, v: T): String
              }

              object AddArg {
                def build[T](f: String => T => String): AddArg[T] = new AddArg[T] {
                  def addArg(key: String, v: T): String = f(key)(v)
                }

                implicit def addArgSeq[T](implicit ev: AddArg[T]): AddArg[List[T]] = build[List[T]](key => vs => vs.map(v => ev.addArg(key, v)).mkString("&"))
                implicit def addArgIterable[T](implicit ev: AddArg[T]): AddArg[Iterable[T]] = build[Iterable[T]](key => vs => vs.map(v => ev.addArg(key, v)).mkString("&"))
                implicit def addArgOption[T](implicit ev: AddArg[T]): AddArg[Option[T]] = build[Option[T]](key => v => v.map(ev.addArg(key, _)).getOrElse(""))
              }

              abstract class AddPath[T] {
                def addPath(v: T): String
              }

              object AddPath {
                def build[T](f: T => String): AddPath[T] = new AddPath[T] {
                  def addPath(v: T): String = f(v)
                }
              }

              abstract class Show[T] {
                def show(v: T): String
              }

              object Show {
                def build[T](f: T => String): Show[T] = new Show[T] {
                  def show(v: T): String = f(v)
                }

                implicit val showString = build[String](Predef.identity)
                implicit val showInt = build[Int](_.toString)
                implicit val showLong = build[Long](_.toString)
                implicit val showFloat = build[Float](_.toString)
                implicit val showDouble = build[Double](_.toString)
                implicit val showBigInt = build[BigInt](_.toString)
                implicit val showBigDecimal = build[BigDecimal](_.toString)
                implicit val showBoolean = build[Boolean](_.toString)
                implicit val showLocalDate = build[java.time.LocalDate](_.format(java.time.format.DateTimeFormatter.ISO_DATE))
                implicit val showOffsetDateTime = build[java.time.OffsetDateTime](_.format(java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME))
                implicit val showJavaURL = build[java.net.URI](_.toString)
                implicit val showUUID = build[java.util.UUID](_.toString)
              }

              object Formatter {
                def show[T](value: T)(implicit ev: Show[T]): String = {
                  ev.show(value)
                }

                def addArg[T](key: String, value: T)(implicit ev: AddArg[T]): String = {
                  s"&$${ev.addArg(key, value)}"
                }

                def addPath[T](value: T)(implicit ev: AddPath[T]): String = {
                  ev.addPath(value)
                }
              }

              class Base64String(val data: Array[Byte]) extends AnyVal {
                override def toString() = "Base64String(" + data.toString() + ")"
              }
              object Base64String {
                def apply(bytes: Array[Byte]): Base64String = new Base64String(bytes)
                def unapply(value: Base64String): Option[Array[Byte]] = Some(value.data)
                private[this] val encoder = java.util.Base64.getEncoder
                implicit val encode: Encoder[Base64String] =
                  Encoder[String].contramap[Base64String](v => new String(encoder.encode(v.data)))

                private[this] val decoder = java.util.Base64.getDecoder
                implicit val decode: Decoder[Base64String] =
                  Decoder[String].emapTry(v => scala.util.Try(decoder.decode(v))).map(new Base64String(_))
              }

            }
          """
      Target.pure(Some(WriteTree(pkgPath.resolve("Implicits.scala"), sourceToBytes(implicits))))
    }
    def renderFrameworkImplicits(
        pkgPath: Path,
        pkgName: List[String],
        frameworkImports: List[L#Import],
        jsonImports: List[L#Import],
        frameworkImplicits: L#ObjectDefinition,
        frameworkImplicitName: L#TermName
    ): F[WriteTree] = {
      val pkg: Term.Ref          = pkgName.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)
      val implicitsRef: Term.Ref = (pkgName.map(Term.Name.apply _) ++ List(q"Implicits")).foldLeft[Term.Ref](q"_root_")(Term.Select.apply _)
      val frameworkImplicitsFile = source"""
            package $pkg

            ..$jsonImports

            ..$frameworkImports

            import cats.implicits._
            import cats.data.EitherT

            import $implicitsRef._

            $frameworkImplicits
          """
      Target.pure(WriteTree(pkgPath.resolve(s"${frameworkImplicitName.value}.scala"), sourceToBytes(frameworkImplicitsFile)))
    }
    def renderFrameworkDefinitions(
        pkgPath: Path,
        pkgName: List[String],
        frameworkImports: List[L#Import],
        frameworkDefinitions: L#ClassDefinition,
        frameworkDefinitionsName: L#TermName
    ): F[WriteTree] = {
      val pkg: Term.Ref            = pkgName.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)
      val implicitsRef: Term.Ref   = (pkgName.map(Term.Name.apply _) ++ List(q"Implicits")).foldLeft[Term.Ref](q"_root_")(Term.Select.apply _)
      val frameworkDefinitionsFile = source"""
            package $pkg

            ..$frameworkImports

            import cats.implicits._
            import cats.data.EitherT

            import $implicitsRef._

            $frameworkDefinitions
          """
      Target.pure(WriteTree(pkgPath.resolve(s"${frameworkDefinitionsName.value}.scala"), sourceToBytes(frameworkDefinitionsFile)))
    }

    def writePackageObject(
        dtoPackagePath: Path,
        dtoComponents: Option[NonEmptyList[String]],
        customImports: List[L#Import],
        packageObjectImports: List[L#Import],
        protocolImports: List[L#Import],
        packageObjectContents: List[L#ValueDefinition],
        extraTypes: List[L#Statement]
    ): F[Option[WriteTree]] =
      dtoComponents.traverse({
        case dtoComponents @ NonEmptyList(dtoHead, dtoRest) =>
          for (dtoRestNel <- Target.fromOption(NonEmptyList.fromList(dtoRest), UserError("DTO Components not quite long enough"))) yield {
            val dtoPkg = dtoRestNel.init.foldLeft[Term.Ref](Term.Name(dtoHead))({
              case (acc, next) =>
                Term.Select(acc, Term.Name(next))
            })
            val companion                 = Term.Name(s"${dtoRestNel.last}$$")
            val (_, statements)           = packageObjectContents.partition(partitionImplicits)
            val implicits: List[Defn.Val] = packageObjectContents.collect(matchImplicit)
            val mirroredImplicits = implicits.map { stat =>
              val List(Pat.Var(mirror)) = stat.pats
              stat.copy(rhs = q"$companion.$mirror")
            }
            WriteTree(
              dtoPackagePath.resolve("package.scala"),
              sourceToBytes(source"""
                package $dtoPkg

                ..${customImports ++ packageObjectImports ++ protocolImports}

                object $companion {
                  ..${implicits.map(_.copy(mods = List.empty))}
                }

                package object ${Term.Name(dtoComponents.last)} {
                  ..${(mirroredImplicits ++ statements ++ extraTypes).to[List]}
                }
                """)
            )
          }
      })
    def writeProtocolDefinition(
        outputPath: Path,
        pkgName: List[String],
        definitions: List[String],
        dtoComponents: List[String],
        imports: List[L#Import],
        elem: StrictProtocolElems[L]
    ): F[(List[WriteTree], List[L#Statement])] =
      Target.pure(elem match {
        case EnumDefinition(_, _, _, _, cls, staticDefns) =>
          (
            List(
              WriteTree(
                resolveFile(outputPath)(dtoComponents).resolve(s"${cls.name.value}.scala"),
                sourceToBytes(source"""
              package ${buildPkgTerm(dtoComponents)}
                import ${buildPkgTerm(List("_root_") ++ pkgName ++ List("Implicits"))}._;
                ..$imports

                $cls
                ${companionForStaticDefns(staticDefns)}
              """)
              )
            ),
            List.empty[Stat]
          )
        case ClassDefinition(_, _, _, cls, staticDefns, _) =>
          (
            List(
              WriteTree(
                resolveFile(outputPath)(dtoComponents).resolve(s"${cls.name.value}.scala"),
                sourceToBytes(source"""
              package ${buildPkgTerm(dtoComponents)}
                ..$imports
                import ${buildPkgTerm(List("_root_") ++ pkgName ++ List("Implicits"))}._;
                $cls
                ${companionForStaticDefns(staticDefns)}
              """)
              )
            ),
            List.empty[Stat]
          )
        case ADT(name, tpe, _, trt, staticDefns) =>
          val polyImports: Import = q"import cats.syntax.either._"
          (
            List(
              WriteTree(
                resolveFile(outputPath)(dtoComponents).resolve(s"$name.scala"),
                sourceToBytes(source"""
                    package ${buildPkgTerm(dtoComponents)}

                    ..$imports
                    $polyImports
                    $trt
                    ${companionForStaticDefns(staticDefns)}
                  """)
              )
            ),
            List.empty[Stat]
          )
        case RandomType(_, _) =>
          (List.empty, List.empty)
      })
    def writeClient(
        pkgPath: Path,
        pkgName: List[String],
        customImports: List[L#Import],
        frameworkImplicitName: Option[L#TermName],
        dtoComponents: Option[List[String]],
        _client: Client[L]
    ): F[List[WriteTree]] = {
      val Client(pkg, clientName, imports, staticDefns, client, responseDefinitions) = _client
      Target.pure(
        List(
          WriteTree(
            resolveFile(pkgPath)(pkg :+ (s"$clientName.scala")),
            sourceToBytes(source"""
              package ${buildPkgTerm(pkgName ++ pkg)}
              import ${buildPkgTerm(List("_root_") ++ pkgName ++ List("Implicits"))}._
              ..${frameworkImplicitName.map(name => q"import ${buildPkgTerm(List("_root_") ++ pkgName)}.$name._")}
              ..${dtoComponents.map(x => q"import ${buildPkgTerm(List("_root_") ++ x)}._")}
              ..$customImports;
              ..$imports;
              ${companionForStaticDefns(staticDefns)};
              ..${client.toList.map(_.merge)};
              ..$responseDefinitions
              """)
          )
        )
      )
    }
    def writeServer(
        pkgPath: Path,
        pkgName: List[String],
        customImports: List[L#Import],
        frameworkImplicitName: Option[L#TermName],
        dtoComponents: Option[List[String]],
        server: Server[L]
    ): F[List[WriteTree]] = {
      val Server(pkg, extraImports, handlerDefinition, serverDefinitions) = server
      Target.pure(
        List(
          WriteTree(
            resolveFile(pkgPath)(pkg.toList :+ "Routes.scala"),
            sourceToBytes(source"""
              package ${buildPkgTerm(pkgName ++ pkg.toList)}
              ..$extraImports
              import ${buildPkgTerm(List("_root_") ++ pkgName ++ List("Implicits"))}._
              ..${frameworkImplicitName.map(name => q"import ${buildPkgTerm(List("_root_") ++ pkgName)}.$name._")}
              ..${dtoComponents.map(x => q"import ${buildPkgTerm(List("_root_") ++ x)}._")}
              ..$customImports
              $handlerDefinition
              ..$serverDefinitions
              """)
          )
        )
      )
    }

    def wrapToObject(name: L#TermName, imports: List[L#Import], definitions: List[L#Definition]): F[L#ObjectDefinition] = Target.pure(q"""
             object $name {
                 ..$imports
                 ..$definitions
             }
           """)
    def apply[T](term: ScalaTerm[ScalaLanguage, T]): Target[T] = term match {
      case VendorPrefixes() => vendorPrefixes()

      case LitString(value)                                  => litString(value)
      case LitFloat(value)                                   => litFloat(value)
      case LitDouble(value)                                  => litDouble(value)
      case LitInt(value)                                     => litInt(value)
      case LitLong(value)                                    => litLong(value)
      case LitBoolean(value)                                 => litBoolean(value)
      case LiftOptionalType(value)                           => liftOptionalType(value)
      case LiftOptionalTerm(value)                           => liftOptionalTerm(value)
      case EmptyOptionalTerm()                               => emptyOptionalTerm()
      case EmptyArray()                                      => emptyArray()
      case EmptyMap()                                        => emptyMap()
      case LiftVectorType(value, customTpe)                  => liftVectorType(value, customTpe)
      case LiftVectorTerm(value)                             => liftVectorTerm(value)
      case LiftMapType(value, customTpe)                     => liftMapType(value, customTpe)
      case FullyQualifyPackageName(rawPkgName)               => fullyQualifyPackageName(rawPkgName)
      case LookupEnumDefaultValue(tpe, defaultValue, values) => lookupEnumDefaultValue(tpe, defaultValue, values)
      case FormatEnumName(enumValue)                         => formatEnumName(enumValue)
      case EmbedArray(tpe, customTpe) =>
        embedArray(tpe, customTpe)
      case EmbedMap(tpe, customTpe) =>
        embedMap(tpe, customTpe)
      case ParseType(value) =>
        parseType(value)
      case ParseTypeName(value) =>
        parseTypeName(value)

      case PureTermName(value) =>
        pureTermName(value)

      case PureTypeName(value) =>
        pureTypeName(value)

      case PureMethodParameter(name, tpe, default) =>
        pureMethodParameter(name, tpe, default)

      case TypeNamesEqual(a, b) =>
        typeNamesEqual(a, b)

      case TypesEqual(a, b) =>
        typesEqual(a, b)

      case ExtractTypeName(tpe) =>
        extractTypeName(tpe)
      case ExtractTermName(term) =>
        extractTermName(term)
      case SelectType(typeNames) =>
        selectType(typeNames)
      case SelectTerm(termNames) =>
        selectTerm(termNames)
      case AlterMethodParameterName(param, name) =>
        alterMethodParameterName(param, name)

      case BytesType()               => bytesType()
      case DateType()                => dateType()
      case DateTimeType()            => dateTimeType()
      case UUIDType()                => uuidType()
      case StringType(format)        => stringType(format)
      case FloatType()               => floatType()
      case DoubleType()              => doubleType()
      case NumberType(format)        => numberType(format)
      case IntType()                 => intType()
      case LongType()                => longType()
      case IntegerType(format)       => integerType(format)
      case BooleanType(format)       => booleanType(format)
      case ArrayType(format)         => arrayType(format)
      case FallbackType(tpe, format) => fallbackType(tpe, format)

      case WidenTypeName(tpe)           => widenTypeName(tpe)
      case WidenTermSelect(value)       => widenTermSelect(value)
      case WidenClassDefinition(value)  => widenClassDefinition(value)
      case WidenObjectDefinition(value) => widenObjectDefinition(value)

      case FindCommonDefaultValue(history, a, b) =>
        findCommonDefaultValue(history, a, b)
      case FindCommonRawType(history, a, b) =>
        findCommonRawType(history, a, b)

      case RenderImplicits(pkgPath, pkgName, frameworkImports, jsonImports, customImports) =>
        renderImplicits(pkgPath, pkgName, frameworkImports, jsonImports, customImports)
      case RenderFrameworkImplicits(pkgPath, pkgName, frameworkImports, jsonImports, frameworkImplicits, frameworkImplicitName) =>
        renderFrameworkImplicits(pkgPath, pkgName, frameworkImports, jsonImports, frameworkImplicits, frameworkImplicitName)
      case RenderFrameworkDefinitions(pkgPath, pkgName, frameworkImports, frameworkDefinitions, frameworkDefinitionsName) =>
        renderFrameworkDefinitions(pkgPath, pkgName, frameworkImports, frameworkDefinitions, frameworkDefinitionsName)
      case WritePackageObject(dtoPackagePath, dtoComponents, customImports, packageObjectImports, protocolImports, packageObjectContents, extraTypes) =>
        writePackageObject(dtoPackagePath, dtoComponents, customImports, packageObjectImports, protocolImports, packageObjectContents, extraTypes)
      case WriteProtocolDefinition(outputPath, pkgName, definitions, dtoComponents, imports, elem) =>
        writeProtocolDefinition(outputPath, pkgName, definitions, dtoComponents, imports, elem)
      case WriteClient(pkgPath, pkgName, customImports, frameworkImplicitName, dtoComponents, client) =>
        writeClient(pkgPath, pkgName, customImports, frameworkImplicitName, dtoComponents, client)
      case WriteServer(pkgPath, pkgName, customImports, frameworkImplicitName, dtoComponents, server) =>
        writeServer(pkgPath, pkgName, customImports, frameworkImplicitName, dtoComponents, server)
      case WrapToObject(name, imports, definitions) =>
        wrapToObject(name, imports, definitions)
    }
  }
}
