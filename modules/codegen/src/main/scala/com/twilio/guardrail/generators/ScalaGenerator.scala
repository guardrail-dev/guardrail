package com.twilio.guardrail
package generators

import cats.syntax.either._
import cats.~>
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.terms._
import scala.meta._
import java.nio.file.{ Path, Paths }

object ScalaGenerator {
  object ScalaInterp extends (ScalaTerm[ScalaLanguage, ?] ~> Target) {
    // TODO: Very interesting bug. 2.11.12 barfs if these two definitions are
    // defined inside `apply`. Once 2.11 is dropped, these can be moved back.
    val matchImplicit: PartialFunction[Stat, Defn.Val] = {
      case x: Defn.Val if (x match { case q"implicit val $_: $_ = $_" => true; case _ => false }) => x
    }
    val partitionImplicits: PartialFunction[Stat, Boolean] = matchImplicit.andThen(_ => true).orElse({ case _ => false })

    val utf8                                      = java.nio.charset.Charset.availableCharsets.get("UTF-8")
    val resolveFile: Path => List[String] => Path = root => _.foldLeft(root)(_.resolve(_))
    val buildPkgTerm: List[String] => Term.Ref =
      _.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)
    def apply[T](term: ScalaTerm[ScalaLanguage, T]): Target[T] = term match {

      case LitString(value)        => Target.pure(Lit.String(value))
      case LitFloat(value)         => Target.pure(Lit.Float(value))
      case LitDouble(value)        => Target.pure(Lit.Double(value))
      case LitInt(value)           => Target.pure(Lit.Int(value))
      case LitLong(value)          => Target.pure(Lit.Long(value))
      case LitBoolean(value)       => Target.pure(Lit.Boolean(value))
      case LiftOptionalType(value) => Target.pure(t"Option[${value}]")
      case LiftOptionalTerm(value) => Target.pure(q"Option(${value})")
      case EmptyOptionalTerm()     => Target.pure(q"None")
      case LiftVectorType(value)   => Target.pure(t"IndexedSeq[${value}]")
      case LiftVectorTerm(value)   => Target.pure(q"IndexedSeq(${value})")
      case LiftMapType(value)      => Target.pure(t"Map[String, ${value}]")
      case LookupEnumDefaultValue(tpe, defaultValue, values) => {
        // FIXME: Is there a better way to do this? There's a gap of coverage here
        defaultValue match {
          case Lit.String(name) =>
            values
              .find(_._1 == name)
              .fold(Target.raiseError[Term.Select](s"Enumeration ${tpe} is not defined for default value ${name}"))(value => Target.pure(value._3))
          case _ =>
            Target.raiseError[Term.Select](s"Enumeration ${tpe} somehow has a default value that isn't a string")
        }
      }
      case JsonType() => Target.getGeneratorSettings.map(_.jsonType)
      case EmbedArray(tpe) =>
        tpe match {
          case SwaggerUtil.Deferred(tpe) =>
            Target.pure(SwaggerUtil.DeferredArray(tpe))
          case SwaggerUtil.DeferredArray(_) =>
            Target.raiseError("FIXME: Got an Array of Arrays, currently not supported")
          case SwaggerUtil.DeferredMap(_) =>
            Target.raiseError("FIXME: Got an Array of Maps, currently not supported")
        }
      case EmbedMap(tpe) =>
        (tpe match {
          case SwaggerUtil.Deferred(inner) => Target.pure(SwaggerUtil.DeferredMap(inner))
          case SwaggerUtil.DeferredMap(_) =>
            Target.raiseError("FIXME: Got a map of maps, currently not supported")
          case SwaggerUtil.DeferredArray(_) =>
            Target.raiseError("FIXME: Got a map of arrays, currently not supported")
        })
      case ParseType(tpe) =>
        Target.pure(
          tpe
            .parse[Type]
            .fold({ err =>
              println(s"Warning: Unparsable x-scala-type: ${tpe} ${err}")
              None
            }, Option.apply _)
        )
      case ParseTypeName(tpe) =>
        Target.pure(Option(tpe.trim).filterNot(_.isEmpty).map(Type.Name(_)))

      case PureTermName(tpe) =>
        Target.fromOption(Option(tpe.trim).filterNot(_.isEmpty).map(Term.Name(_)), "A structure's name is empty")

      case PureTypeName(tpe) =>
        Target.fromOption(Option(tpe.trim).filterNot(_.isEmpty).map(Type.Name(_)), "A structure's name is empty")

      case PureMethodParameter(name, tpe, default) =>
        Target.pure(param"${name}: ${tpe}".copy(default = default))

      case TypeNamesEqual(a, b) =>
        Target.pure(a.value == b.value)

      case TypesEqual(a, b) =>
        Target.pure(a.structure == b.structure)

      case ExtractTypeName(tpe) =>
        Target.pure(tpe match {
          case x: Type.Name => Option(x)
          case _            => Option.empty
        })
      case ExtractTermName(term) =>
        val Term.Name(name) = term
        Target.pure(name)

      case AlterMethodParameterName(param, name) =>
        Target.pure(param.copy(name = name))

      case DateType()                => Target.pure(t"java.time.LocalDate")
      case DateTimeType()            => Target.pure(t"java.time.OffsetDateTime")
      case StringType(format)        => Target.pure(format.fold(t"String")(Type.Name(_)))
      case FloatType()               => Target.pure(t"Float")
      case DoubleType()              => Target.pure(t"Double")
      case NumberType(format)        => Target.pure(t"BigDecimal")
      case IntType()                 => Target.pure(t"Int")
      case LongType()                => Target.pure(t"Long")
      case IntegerType(format)       => Target.pure(t"BigInt")
      case BooleanType(format)       => Target.pure(t"Boolean")
      case ArrayType(format)         => Target.pure(t"Iterable[String]")
      case FileType(format)          => Target.getGeneratorSettings.map(gs => format.fold(gs.fileType)(Type.Name(_)))
      case ObjectType(format)        => Target.getGeneratorSettings.map(_.jsonType)
      case FallbackType(tpe, format) => Target.pure(Type.Name(tpe))

      case WidenTypeName(tpe)     => Target.pure(tpe)
      case WidenTermSelect(value) => Target.pure(value)

      case RenderImplicits(pkgPath, pkgName, frameworkImports, jsonImports, customImports) =>
        val pkg: Term.Ref =
          pkgName.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)

        val implicits = source"""
            package ${pkg}

            ..${jsonImports}

            import cats.implicits._
            import cats.data.EitherT

            import scala.concurrent.Future

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

                implicit val showString = build[String](identity)
                implicit val showInt = build[Int](_.toString)
                implicit val showLong = build[Long](_.toString)
                implicit val showFloat = build[Float](_.toString)
                implicit val showDouble = build[Double](_.toString)
                implicit val showBigInt = build[BigInt](_.toString)
                implicit val showBigDecimal = build[BigDecimal](_.toString)
                implicit val showBoolean = build[Boolean](_.toString)
                implicit val showOffsetDateTime = build[java.time.OffsetDateTime](_.format(java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME))
                implicit val showJavaURL = build[java.net.URL](_.toString)
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
            }
          """
        Target.pure(WriteTree(pkgPath.resolve("Implicits.scala"), implicits.syntax.getBytes(utf8)))

      case RenderFrameworkImplicits(pkgPath, pkgName, frameworkImports, jsonImports, frameworkImplicits, frameworkImplicitName) =>
        val pkg: Term.Ref =
          pkgName.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)
        val frameworkImplicitsFile = source"""
            package ${pkg}

            ..${jsonImports}

            ..${frameworkImports}

            import cats.implicits._
            import cats.data.EitherT

            import scala.concurrent.Future

            import ${pkg}.Implicits._

            ${frameworkImplicits}
          """
        Target.pure(WriteTree(pkgPath.resolve(s"${frameworkImplicitName.value}.scala"), frameworkImplicitsFile.syntax.getBytes(utf8)))

      case WritePackageObject(dtoPackagePath, dtoComponents, customImports, packageObjectImports, protocolImports, packageObjectContents, extraTypes) =>
        val dtoHead :: dtoRest = dtoComponents
        val dtoPkg = dtoRest.init
          .foldLeft[Term.Ref](Term.Name(dtoHead)) {
            case (acc, next) => Term.Select(acc, Term.Name(next))
          }
        val companion = Term.Name(s"${dtoComponents.last}$$")

        val (_, statements) =
          packageObjectContents.partition(partitionImplicits)
        val implicits: List[Defn.Val] = packageObjectContents.collect(matchImplicit)

        val mirroredImplicits = implicits
          .map({ stat =>
            val List(Pat.Var(mirror)) = stat.pats
            stat.copy(rhs = q"${companion}.${mirror}")
          })

        Target.pure(
          WriteTree(
            dtoPackagePath.resolve("package.scala"),
            source"""
            package ${dtoPkg}

            ..${customImports ++ packageObjectImports ++ protocolImports}

            object ${companion} {
              ..${implicits.map(_.copy(mods = List.empty))}
            }

            package object ${Term.Name(dtoComponents.last)} {
              ..${(mirroredImplicits ++ statements ++ extraTypes).to[List]}
            }
            """.syntax.getBytes(utf8)
          )
        )
      case WriteProtocolDefinition(outputPath, pkgName, definitions, dtoComponents, imports, elem) =>
        Target.pure(elem match {
          case EnumDefinition(_, _, _, cls, obj) =>
            (List(
               WriteTree(
                 resolveFile(outputPath)(dtoComponents).resolve(s"${cls.name.value}.scala"),
                 source"""
              package ${buildPkgTerm(definitions)}
                import ${buildPkgTerm(List("_root_") ++ pkgName ++ List("Implicits"))}._;
                ..${imports}

                $cls
                $obj
              """.syntax.getBytes(utf8)
               )
             ),
             List.empty[Stat])

          case ClassDefinition(_, _, cls, obj, _) =>
            (List(
               WriteTree(
                 resolveFile(outputPath)(dtoComponents).resolve(s"${cls.name.value}.scala"),
                 source"""
              package ${buildPkgTerm(dtoComponents)}
                ..${imports}
                $cls
                $obj
              """.syntax.getBytes(utf8)
               )
             ),
             List.empty[Stat])

          case ADT(name, tpe, trt, obj) =>
            val polyImports: Import = q"""import cats.syntax.either._"""

            (
              List(
                WriteTree(
                  resolveFile(outputPath)(dtoComponents).resolve(s"$name.scala"),
                  source"""
                    package ${buildPkgTerm(dtoComponents)}

                    ..$imports
                    $polyImports
                    $trt
                    $obj
                  """.syntax.getBytes(utf8)
                )
              ),
              List.empty[Stat]
            )

          case RandomType(_, _) =>
            (List.empty, List.empty)
        })
      case WriteClient(pkgPath,
                       pkgName,
                       customImports,
                       frameworkImplicitName,
                       dtoComponents,
                       Client(pkg, clientName, imports, companion, client, responseDefinitions)) =>
        Target.pure(
          WriteTree(
            resolveFile(pkgPath)(pkg :+ s"${clientName}.scala"),
            source"""
            package ${buildPkgTerm(pkgName ++ pkg)}
            import ${buildPkgTerm(List("_root_") ++ pkgName ++ List("Implicits"))}._
            import ${buildPkgTerm(List("_root_") ++ pkgName)}.${frameworkImplicitName}._
            import ${buildPkgTerm(List("_root_") ++ dtoComponents)}._
            ..${customImports};
            ..${imports};
            ${companion};
            ${client};
            ..${responseDefinitions}
            """.syntax.getBytes(utf8)
          )
        )
      case WriteServer(pkgPath, pkgName, customImports, frameworkImplicitName, dtoComponents, Server(pkg, extraImports, src)) =>
        Target.pure(
          WriteTree(
            resolveFile(pkgPath)(pkg.toList :+ "Routes.scala"),
            source"""
              package ${buildPkgTerm((pkgName ++ pkg.toList))}
              ..${extraImports}
              import ${buildPkgTerm(List("_root_") ++ pkgName ++ List("Implicits"))}._
              import ${buildPkgTerm(List("_root_") ++ pkgName)}.${frameworkImplicitName}._
              import ${buildPkgTerm(List("_root_") ++ dtoComponents)}._
              ..${customImports}
              ..$src
              """.syntax.getBytes(utf8)
          )
        )
    }
  }
}
