package com.twilio.guardrail
package generators

import cats.syntax.either._
import cats.~>
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.terms._
import scala.meta._

object ScalaGenerator {
  object ScalaInterp extends (ScalaTerm[ScalaLanguage, ?] ~> Target) {
    // TODO: Very interesting bug. 2.11.12 barfs if these two definitions are
    // defined inside `apply`. Once 2.11 is dropped, these can be moved back.
    val matchImplicit: PartialFunction[Stat, Defn.Val] = {
      case x: Defn.Val if (x match { case q"implicit val $_: $_ = $_" => true; case _ => false }) => x
    }
    val partitionImplicits: PartialFunction[Stat, Boolean] = matchImplicit.andThen(_ => true).orElse({ case _ => false })

    def apply[T](term: ScalaTerm[ScalaLanguage, T]): Target[T] = term match {

      case LitString(value)        => Target.pure(Lit.String(value))
      case LitFloat(value)         => Target.pure(Lit.Float(value))
      case LitDouble(value)        => Target.pure(Lit.Double(value))
      case LitInt(value)           => Target.pure(Lit.Int(value))
      case LitLong(value)          => Target.pure(Lit.Long(value))
      case LitBoolean(value)       => Target.pure(Lit.Boolean(value))
      case LiftOptionalType(value) => Target.pure(t"Option[${value}]")
      case LiftOptionalTerm(value) => Target.pure(q"Option(${value})")
      case EmptyOptionalTerm()     => Target.pure(q"Option.empty")
      case LiftVectorType(value)   => Target.pure(t"IndexedSeq[${value}]")
      case LiftVectorTerm(value)   => Target.pure(q"IndexedSeq(${value})")
      case LiftMapType(value)      => Target.pure(t"Map[String, ${value}]")
      case JsonType()              => Target.getGeneratorSettings.map(_.jsonType)
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

      case WidenTypeName(tpe) => Target.pure(tpe)

      case RenderImplicits(pkgName, frameworkImports, jsonImports, customImports) =>
        val pkg: Term.Ref =
          pkgName.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)
        Target.pure(
          source"""
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
        )

      case RenderFrameworkImplicits(pkgName, frameworkImports, jsonImports, frameworkImplicits) =>
        val pkg: Term.Ref =
          pkgName.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)
        Target.pure(
          source"""
            package ${pkg}

            ..${jsonImports}

            ..${frameworkImports}

            import cats.implicits._
            import cats.data.EitherT

            import scala.concurrent.Future

            import ${pkg}.Implicits._

            ${frameworkImplicits}
          """
        )

      case WritePackageObject(dtoPackagePath, dtoComponents, customImports, packageObjectImports, protocolImports, packageObjectContents, extraTypes) =>
        val utf8 = java.nio.charset.Charset.availableCharsets.get("UTF-8")

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
    }
  }
}
