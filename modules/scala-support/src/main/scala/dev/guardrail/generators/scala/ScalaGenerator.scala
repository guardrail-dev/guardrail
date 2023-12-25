package dev.guardrail.generators.scala

import cats.data.NonEmptyList
import cats.syntax.all._
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.meta._
import scala.reflect.runtime.universe.typeTag

import dev.guardrail.Common.resolveFile
import dev.guardrail._
import dev.guardrail.core.{ ReifiedRawType, Tracker }
import dev.guardrail.generators.scala.syntax._
import dev.guardrail.generators.spi.{ LanguageLoader, ModuleLoadResult }
import dev.guardrail.generators.syntax.RichString
import dev.guardrail.generators.{ Client, Server }
import dev.guardrail.terms._
import dev.guardrail.terms.protocol._

class ScalaGeneratorLoader extends LanguageLoader {
  type L = ScalaLanguage
  def reified = typeTag[Target[ScalaLanguage]]
  val apply   = ModuleLoadResult.emitDefault(ScalaGenerator())
}

object ScalaGenerator {
  def apply(): LanguageTerms[ScalaLanguage, Target] =
    new ScalaGenerator

  val mapping: Map[String, LanguageTerms[ScalaLanguage, Target]] = Map(
    "scala-language" -> apply()
  )
}

class ScalaGenerator private extends LanguageTerms[ScalaLanguage, Target] {
  private def sourceToBytes(path: Path, source: Source): WriteTree =
    WriteTree(
      path,
      Future {
        Target.pure((GENERATED_CODE_COMMENT + source.syntax).getBytes(StandardCharsets.UTF_8))
      }
    )

  private val buildTermSelect: NonEmptyList[String] => Term.Ref = { case NonEmptyList(start, rest) =>
    rest.map(Term.Name.apply _).foldLeft[Term.Ref](Term.Name(start))(Term.Select.apply _)
  }

  override def litString(value: String): Target[scala.meta.Term]   = Target.pure(Lit.String(value))
  override def litFloat(value: Float): Target[scala.meta.Term]     = Target.pure(Lit.Float(value))
  override def litDouble(value: Double): Target[scala.meta.Term]   = Target.pure(Lit.Double(value))
  override def litInt(value: Int): Target[scala.meta.Term]         = Target.pure(Lit.Int(value))
  override def litLong(value: Long): Target[scala.meta.Term]       = Target.pure(Lit.Long(value))
  override def litBoolean(value: Boolean): Target[scala.meta.Term] = Target.pure(Lit.Boolean(value))

  override def fullyQualifyPackageName(rawPkgName: NonEmptyList[String]): Target[NonEmptyList[String]] = Target.pure("_root_" :: rawPkgName)

  override def lookupEnumDefaultValue(
      tpe: scala.meta.Type.Name,
      defaultValue: scala.meta.Term,
      values: RenderedEnum[ScalaLanguage]
  ): Target[scala.meta.Term.Select] =
    (defaultValue, values) match {
      case (Lit.String(name), RenderedStringEnum(values)) =>
        values
          .find(_._1 == name)
          .fold(Target.raiseUserError[Term.Select](s"Enumeration $tpe is not defined for default value $name"))(value => Target.pure(value._3))
      case (Lit.Int(name), RenderedIntEnum(values)) =>
        values
          .find(_._1 == name)
          .fold(Target.raiseUserError[Term.Select](s"Enumeration $tpe is not defined for default value $name"))(value => Target.pure(value._3))
      case (Lit.Long(name), RenderedLongEnum(values)) =>
        values
          .find(_._1 == name)
          .fold(Target.raiseUserError[Term.Select](s"Enumeration $tpe is not defined for default value $name"))(value => Target.pure(value._3))
      case _ =>
        Target.raiseUserError[Term.Select](s"Enumeration $tpe somehow has a default value that doesn't match its type")
    }

  override def formatPackageName(packageName: List[String]): Target[NonEmptyList[String]] =
    Target.fromOption(NonEmptyList.fromList(packageName.map(_.toCamelCase)), UserError("Empty packageName"))
  override def formatTypeName(typeName: String, suffix: Option[String] = None): Target[String] =
    Target.pure(typeName.toPascalCase + suffix.fold("")(_.toPascalCase))
  override def formatFieldName(fieldName: String): Target[String]         = Target.pure(fieldName.toCamelCase)
  override def formatMethodName(methodName: String): Target[String]       = Target.pure(methodName.toCamelCase)
  override def formatMethodArgName(methodArgName: String): Target[String] = Target.pure(methodArgName.toCamelCase)
  override def formatEnumName(enumValue: String): Target[String]          = Target.pure(enumValue.toPascalCase)

  override def parseType(tpe: Tracker[String]): Target[Option[scala.meta.Type]] =
    Target.pure(
      tpe.unwrapTracker
        .parse[Type]
        .fold(
          { err =>
            println(s"Warning: Unparsable x-scala-type: ${tpe.unwrapTracker} $err (${tpe.showHistory})")
            None
          },
          Option.apply _
        )
    )
  override def parseTypeName(tpe: String): Target[Option[scala.meta.Type.Name]] = Target.pure(Option(tpe.trim).filterNot(_.isEmpty).map(Type.Name(_)))
  override def pureTermName(tpe: String): Target[scala.meta.Term.Name] =
    Target.fromOption(Option(tpe.trim).filterNot(_.isEmpty).map(Term.Name(_)), UserError("A structure's name is empty"))
  override def pureTypeName(tpe: String): Target[scala.meta.Type.Name] =
    Target.fromOption(Option(tpe.trim).filterNot(_.isEmpty).map(Type.Name(_)), UserError("A structure's name is empty"))

  override def pureMethodParameter(name: scala.meta.Term.Name, tpe: scala.meta.Type, default: Option[scala.meta.Term]): Target[scala.meta.Term.Param] =
    Target.pure(param"$name: $tpe".copy(default = default))
  override def typeNamesEqual(a: scala.meta.Type.Name, b: scala.meta.Type.Name): Target[Boolean] = Target.pure(a.value == b.value)
  override def typesEqual(a: scala.meta.Type, b: scala.meta.Type): Target[Boolean]               = Target.pure(a.structure == b.structure)
  override def extractTypeName(tpe: scala.meta.Type): Target[Option[scala.meta.Type.Name]] =
    Target.pure(tpe match {
      case x: Type.Name =>
        Option(x)
      case _ =>
        Option.empty
    })
  override def extractTermName(term: scala.meta.Term.Name): Target[String] =
    term match {
      case Term.Name(name) => Target.pure(name)
      case _               => Target.raiseException("Impossible 2.13.4+ excitement, see https://github.com/scala/bug/issues/12232")
    }
  override def extractTermNameFromParam(param: scala.meta.Term.Param): Target[String] = Target.pure(param.name.value)
  override def selectType(typeNames: NonEmptyList[String]): Target[scala.meta.Type] = {
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
  override def selectTerm(termNames: NonEmptyList[String]): Target[scala.meta.Term] = {
    val result = termNames.tail.foldLeft[Term](Term.Name(termNames.head)) { case (current, next) =>
      Term.Select(current, Term.Name(next))
    }
    Target.pure(result)
  }
  override def alterMethodParameterName(param: scala.meta.Term.Param, name: scala.meta.Term.Name): Target[scala.meta.Term.Param] =
    Target.pure(param.copy(name = name))

  override def bytesType(): Target[scala.meta.Type]                         = Target.pure(t"Base64String")
  override def uuidType(): Target[scala.meta.Type]                          = Target.pure(t"java.util.UUID")
  override def dateType(): Target[scala.meta.Type]                          = Target.pure(t"java.time.LocalDate")
  override def dateTimeType(): Target[scala.meta.Type]                      = Target.pure(t"java.time.OffsetDateTime")
  override def stringType(format: Option[String]): Target[scala.meta.Type]  = Target.pure(format.fold(t"String")(Type.Name(_)))
  override def floatType(): Target[scala.meta.Type]                         = Target.pure(t"Float")
  override def doubleType(): Target[scala.meta.Type]                        = Target.pure(t"Double")
  override def numberType(format: Option[String]): Target[scala.meta.Type]  = Target.pure(t"BigDecimal")
  override def intType(): Target[scala.meta.Type]                           = Target.pure(t"Int")
  override def longType(): Target[scala.meta.Type]                          = Target.pure(t"Long")
  override def integerType(format: Option[String]): Target[scala.meta.Type] = Target.pure(t"BigInt")
  override def booleanType(format: Option[String]): Target[scala.meta.Type] = Target.pure(t"Boolean")
  override def fallbackType(tpe: Option[String], format: Option[String]): Target[scala.meta.Type] =
    Target.fromOption(tpe, UserError("Missing type")).map(Type.Name(_))

  override def widenTypeName(tpe: scala.meta.Type.Name): Target[scala.meta.Type]             = Target.pure(tpe)
  override def widenTermSelect(value: scala.meta.Term.Select): Target[scala.meta.Term]       = Target.pure(value)
  override def widenClassDefinition(value: scala.meta.Defn.Class): Target[scala.meta.Defn]   = Target.pure(value)
  override def widenObjectDefinition(value: scala.meta.Defn.Object): Target[scala.meta.Defn] = Target.pure(value)

  override def findCommonDefaultValue(history: String, a: Option[scala.meta.Term], b: Option[scala.meta.Term]): Target[Option[scala.meta.Term]] = (a, b) match {
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

  override def findCommonRawType(history: String, a: ReifiedRawType, b: ReifiedRawType): Target[ReifiedRawType] =
    if (a == b) {
      Target.pure(a)
    } else {
      Target.raiseUserError(
        s"There is a mismatch at $history between types $a and $b. Conflicting definitions between types and inherited types are not supported."
      )
    }

  override def renderImplicits(
      pkgPath: Path,
      pkgName: NonEmptyList[String],
      frameworkImports: List[scala.meta.Import],
      jsonImports: List[scala.meta.Import],
      customImports: List[scala.meta.Import]
  ): Target[Option[WriteTree]] = {
    val pkg: Term.Ref = buildTermSelect(pkgName)
    val implicits = source"""
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

              implicit def addArgSeq[T](implicit ev: AddArg[T]): AddArg[Seq[T]] = build[Seq[T]](key => vs => vs.map(v => ev.addArg(key, v)).mkString("&"))
              implicit def addArgIndexedSeq[T](implicit ev: AddArg[T]): AddArg[IndexedSeq[T]] = build[IndexedSeq[T]](key => vs => vs.map(v => ev.addArg(key, v)).mkString("&"))
              implicit def addArgIterable[T](implicit ev: AddArg[T]): AddArg[Iterable[T]] = build[Iterable[T]](key => vs => vs.map(v => ev.addArg(key, v)).mkString("&"))
              implicit def addArgList[T](implicit ev: AddArg[T]): AddArg[List[T]] = build[List[T]](key => vs => vs.map(v => ev.addArg(key, v)).mkString("&"))
              implicit def addArgVector[T](implicit ev: AddArg[T]): AddArg[Vector[T]] = build[Vector[T]](key => vs => vs.map(v => ev.addArg(key, v)).mkString("&"))
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

            abstract class Show[T] { self =>
              def show(v: T): String
              def contramap[A](f: A => T): Show[A] = new Show[A] {
                def show(v: A): String = self.show(f(v))
              }
            }

            object Show {
              def apply[A](implicit ev: Show[A]): Show[A] = ev
              def build[T](f: T => String): Show[T] = new Show[T] {
                def show(v: T): String = f(v)
              }

              implicit val showString: Show[String] = build[String](Predef.identity)
              implicit val showInt: Show[Int] = build[Int](_.toString)
              implicit val showLong: Show[Long] = build[Long](_.toString)
              implicit val showFloat: Show[Float] = build[Float](_.toString)
              implicit val showDouble: Show[Double] = build[Double](_.toString)
              implicit val showBigInt: Show[BigInt] = build[BigInt](_.toString)
              implicit val showBigDecimal: Show[BigDecimal] = build[BigDecimal](_.toString)
              implicit val showBoolean: Show[Boolean] = build[Boolean](_.toString)
              implicit val showLocalDate: Show[java.time.LocalDate] = build(_.format(java.time.format.DateTimeFormatter.ISO_DATE))
              implicit val showOffsetDateTime: Show[java.time.OffsetDateTime] = build(_.format(java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME))
              implicit val showJavaURL: Show[java.net.URI] = build(_.toString)
              implicit val showUUID: Show[java.util.UUID] = build(_.toString)
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
              def unapply(value: Base64String): Some[Array[Byte]] = Some(value.data)
            }

          }
        """
    Target.pure(Some(sourceToBytes(pkgPath.resolve("Implicits.scala"), implicits)))
  }

  override def renderFrameworkImplicits(
      pkgPath: Path,
      pkgName: NonEmptyList[String],
      frameworkImports: List[scala.meta.Import],
      frameworkImplicitImportNames: List[scala.meta.Term.Name],
      jsonImports: List[scala.meta.Import],
      frameworkImplicits: scala.meta.Defn.Object,
      frameworkImplicitName: scala.meta.Term.Name
  ): Target[WriteTree] = {
    val pkg: Term.Ref            = buildTermSelect(pkgName)
    val implicitsRef: Term.Ref   = (pkgName.map(Term.Name.apply _) ++ List(q"Implicits")).foldLeft[Term.Ref](q"_root_")(Term.Select.apply _)
    val frameworkImplicitImports = frameworkImplicitImportNames.map(name => q"import ${buildTermSelect(("_root_" :: pkgName) :+ name.value)}._")
    val frameworkImplicitsFile = source"""
          package $pkg

          ..$jsonImports

          ..$frameworkImports

          import cats.implicits._
          import cats.data.EitherT

          import $implicitsRef._
          ..$frameworkImplicitImports

          $frameworkImplicits
        """
    Target.pure(sourceToBytes(pkgPath.resolve(s"${frameworkImplicitName.value}.scala"), frameworkImplicitsFile))
  }

  override def renderFrameworkDefinitions(
      pkgPath: Path,
      pkgName: NonEmptyList[String],
      frameworkImports: List[scala.meta.Import],
      frameworkDefinitions: List[scala.meta.Defn],
      frameworkDefinitionsName: scala.meta.Term.Name
  ): Target[WriteTree] = {
    val pkg: Term.Ref = buildTermSelect(pkgName)
    val frameworkDefinitionsFile = source"""
          package $pkg

          ..$frameworkImports

          ..$frameworkDefinitions
        """
    Target.pure(sourceToBytes(pkgPath.resolve(s"${frameworkDefinitionsName.value}.scala"), frameworkDefinitionsFile))
  }

  override def writePackageObject(
      dtoPackagePath: Path,
      pkgComponents: NonEmptyList[String],
      dtoComponents: Option[NonEmptyList[String]],
      customImports: List[scala.meta.Import],
      packageObjectImports: List[scala.meta.Import],
      protocolImports: List[scala.meta.Import],
      packageObjectContents: List[scala.meta.Stat],
      extraTypes: List[scala.meta.Stat]
  ): Target[Option[WriteTree]] = {
    val pkgImplicitsImport = q"import ${buildTermSelect("_root_" :: pkgComponents)}.Implicits._"

    val matchImplicit: PartialFunction[Stat, Defn.Val] = {
      case x: Defn.Val if (x match { case q"implicit val $_: $_ = $_" => true; case _ => false }) => x
    }

    val partitionImplicits: PartialFunction[Stat, Boolean] = matchImplicit.andThen(_ => true).orElse { case _ => false }

    dtoComponents.traverse { case dtoComponents @ NonEmptyList(dtoHead, dtoRest) =>
      for {
        dtoRestNel <- Target.fromOption(NonEmptyList.fromList(dtoRest), UserError("DTO Components not quite long enough"))
        dtoPkg = dtoRestNel.init.foldLeft[Term.Ref](Term.Name(dtoHead)) { case (acc, next) =>
          Term.Select(acc, Term.Name(next))
        }
        companion       = Term.Name(s"${dtoRestNel.last}$$")
        (_, statements) = packageObjectContents.partition(partitionImplicits)
        implicits       = packageObjectContents.collect(matchImplicit)
        mirroredImplicits <- implicits.traverse { stat =>
          stat.pats match {
            case List(Pat.Var(mirror)) => Target.pure(stat.copy(rhs = q"$companion.$mirror"))
            case other                 => Target.raiseUserError(s"Attempt to mirror implicits failed, expected List(Pat.Var(...)), got ${other}")
          }
        }
      } yield sourceToBytes(
        dtoPackagePath.resolve("package.scala"),
        source"""
            package $dtoPkg

            ..${customImports ++ packageObjectImports ++ protocolImports :+ pkgImplicitsImport}

            object $companion {
              ..${implicits.map(_.copy(mods = List.empty))}
            }

            package object ${Term.Name(dtoComponents.last)} {
              ..${(mirroredImplicits ++ statements ++ extraTypes).toList}
            }
            """
      )
    }
  }

  override def writeProtocolDefinition(
      outputPath: Path,
      pkgName: NonEmptyList[String],
      definitions: List[String],
      dtoComponents: NonEmptyList[String],
      imports: List[scala.meta.Import],
      protoImplicitName: Option[scala.meta.Term.Name],
      elem: StrictProtocolElems[ScalaLanguage]
  ): Target[(List[WriteTree], List[scala.meta.Stat])] = {
    val implicitImports = (List("Implicits") ++ protoImplicitName.map(_.value))
      .map(name => q"import ${buildTermSelect(("_root_" :: pkgName) :+ name)}._")
    Target.pure(elem match {
      case EnumDefinition(_, _, _, _, cls, staticDefns) =>
        (
          List(
            sourceToBytes(
              resolveFile(outputPath)(dtoComponents.toList).resolve(s"${cls.name.value}.scala"),
              source"""
            package ${buildTermSelect(dtoComponents)}
              ..$imports
              ..$implicitImports
              $cls
              ${companionForStaticDefns(staticDefns)}
            """
            )
          ),
          List.empty[Stat]
        )
      case ClassDefinition(_, _, _, cls, staticDefns, _) =>
        (
          List(
            sourceToBytes(
              resolveFile(outputPath)(dtoComponents.toList).resolve(s"${cls.name.value}.scala"),
              source"""
            package ${buildTermSelect(dtoComponents)}
              ..$imports
              ..$implicitImports
              $cls
              ${companionForStaticDefns(staticDefns)}
            """
            )
          ),
          List.empty[Stat]
        )
      case ADT(name, tpe, _, trt, staticDefns) =>
        val polyImports: Import = q"import cats.syntax.either._"
        (
          List(
            sourceToBytes(
              resolveFile(outputPath)(dtoComponents.toList).resolve(s"$name.scala"),
              source"""
                  package ${buildTermSelect(dtoComponents)}
                  ..$imports
                  ..$implicitImports
                  $polyImports
                  $trt
                  ${companionForStaticDefns(staticDefns)}
                """
            )
          ),
          List.empty[Stat]
        )
      case RandomType(_, _) =>
        (List.empty, List.empty)
    })
  }

  override def writeClient(
      pkgPath: Path,
      pkgName: NonEmptyList[String],
      customImports: List[scala.meta.Import],
      frameworkImplicitNames: List[scala.meta.Term.Name],
      dtoComponents: Option[NonEmptyList[String]],
      _client: Client[ScalaLanguage]
  ): Target[List[WriteTree]] = {
    val Client(pkg, clientName, imports, staticDefns, client, responseDefinitions) = _client
    Target.pure(
      List(
        sourceToBytes(
          resolveFile(pkgPath)(pkg :+ s"$clientName.scala"),
          source"""
            package ${buildTermSelect(pkgName ++ pkg)}
            import ${buildTermSelect(("_root_" :: pkgName) :+ "Implicits")}._
            ..${frameworkImplicitNames.map(name => q"import ${buildTermSelect("_root_" :: pkgName)}.$name._")}
            ..${dtoComponents.map(xs => q"import ${buildTermSelect("_root_" :: xs)}._")}
            ..$customImports;
            ..$imports;
            ${companionForStaticDefns(staticDefns)};
            ..${client.toList.map(_.merge)};
            ..$responseDefinitions
            """
        )
      )
    )
  }

  override def writeServer(
      pkgPath: Path,
      pkgName: NonEmptyList[String],
      customImports: List[scala.meta.Import],
      frameworkImplicitNames: List[scala.meta.Term.Name],
      dtoComponents: Option[NonEmptyList[String]],
      server: Server[ScalaLanguage]
  ): Target[List[WriteTree]] = {
    val Server(pkg, extraImports, handlerDefinition, serverDefinitions) = server
    Target.pure(
      List(
        sourceToBytes(
          resolveFile(pkgPath)(pkg.toList :+ "Routes.scala"),
          source"""
            package ${buildTermSelect(pkgName ++ pkg.toList)}
            ..$extraImports
            import ${buildTermSelect(("_root_" :: pkgName) :+ "Implicits")}._
            ..${frameworkImplicitNames.map(name => q"import ${buildTermSelect("_root_" :: pkgName)}.$name._")}
            ..${dtoComponents.map(xs => q"import ${buildTermSelect("_root_" :: xs)}._")}
            ..$customImports
            $handlerDefinition
            ..$serverDefinitions
            """
        )
      )
    )
  }

  override def wrapToObject(
      name: scala.meta.Term.Name,
      imports: List[scala.meta.Import],
      definitions: List[scala.meta.Defn],
      statements: List[scala.meta.Stat]
  ): Target[Option[scala.meta.Defn.Object]] =
    Target.pure(Some(q"""
           object $name {
               ..$imports
               ..$definitions
               ..$statements
           }
         """))
}
