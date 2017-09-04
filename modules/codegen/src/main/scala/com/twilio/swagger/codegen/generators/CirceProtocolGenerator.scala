package com.twilio.swagger.codegen
package generators

import _root_.io.swagger.models.properties._
import cats.syntax.either._
import cats.~>
import com.twilio.swagger.codegen.extract.{Default, ScalaEmptyIsNull, ScalaType}
import com.twilio.swagger.codegen.terms.protocol._
import java.util.Locale
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import scala.meta._

object CirceProtocolGenerator {
  import ProtocolGenerator._

  def suffixClsName(prefix: String, clsName: String) = Pat.Var.Term(Term.Name(s"${prefix}${clsName}"))

  object EnumProtocolTermInterp extends (EnumProtocolTerm ~> Target) {
    def apply[T](term: EnumProtocolTerm[T]): Target[T] = term match {
      case ExtractEnum(swagger) =>
        Target.pure(Either.fromOption(Option(swagger.getEnum).map(_.asScala.to[Seq]), "Model has no enumerations"))

      case ExtractType(swagger) =>
        Target.pure(Either.fromOption(Option(swagger.getType).map(SwaggerUtil.typeName(_, None, ScalaType(swagger))), "Unable to determine type"))

      case RenderMembers(clsName, elems) =>
        Target.pure(q"""
          object members {
            ..${elems.map({ case (value, pascalValue) =>
              q"""case object ${pascalValue} extends ${Ctor.Ref.Name(clsName)}(${Lit.String(value)})"""
            }).to[Seq]}
          }
        """)

      case EncodeEnum(clsName) =>
        Target.pure(q"""
          implicit val ${suffixClsName("encode", clsName)}: Encoder[${Type.Name(clsName)}] =
            Encoder[String].contramap(_.value)
        """)

      case DecodeEnum(clsName) =>
        Target.pure(q"""
          implicit val ${suffixClsName("decode", clsName)}: Decoder[${Type.Name(clsName)}] =
            Decoder[String].emap(value => parse(value).toRight(${Term.Interpolate(Term.Name("s"), Seq(Lit.String(""), Lit.String(s" not a member of ${clsName}")), Seq(Term.Name("value")))}))
        """)

      case RenderClass(clsName, tpe) =>
        Target.pure(q"""
          sealed abstract class ${Type.Name(clsName)}(val value: ${tpe}) {
            override def toString: String = value.toString
          }
        """)

      case RenderCompanion(clsName, members, accessors, values, encoder, decoder) =>
        Target.pure(q"""
          object ${Term.Name(clsName)} {
            ..${
              Seq(members) ++
              accessors ++
              Seq(values) ++
              Seq(q"def parse(value: String): Option[${Type.Name(clsName)}] = values.find(_.value == value)") ++
              Seq(encoder) ++
              Seq(decoder) ++
              Seq(q"implicit val ${Pat.Var.Term(Term.Name(s"addPath${clsName}"))}: AddPath[${Type.Name(clsName)}] = AddPath.build(_.value)") ++
              Seq(q"implicit val ${Pat.Var.Term(Term.Name(s"show${clsName}"))}: Show[${Type.Name(clsName)}] = Show.build(_.value)")
            }
          }
        """)
    }
  }

  object ModelProtocolTermInterp extends (ModelProtocolTerm ~> Target) {
    def apply[T](term: ModelProtocolTerm[T]): Target[T] = term match {
      case ExtractProperties(swagger) =>
        Target.pure(Either.fromOption(Option(swagger.getProperties()).map(_.asScala.toList), "Model has no properties"))

      case TransformProperty(clsName, name, property, needCamelSnakeConversion) =>
        def toCamelCase(s: String): String = "[_\\.]([a-z])".r.replaceAllIn(s, m => m.group(1).toUpperCase(Locale.US))

        val argName = if (needCamelSnakeConversion) toCamelCase(name) else name
        val (term, ref) = {
          val defaultValue: Option[Term] = property match {
            case _: MapProperty =>
              Option(q"Map.empty")
            case _: ArrayProperty =>
              Option(q"IndexedSeq.empty")
            case p: BooleanProperty =>
              Default(p).extract[Boolean].map(Lit.Boolean(_))
            case p: DoubleProperty =>
              Default(p).extract[Double].map(Lit.Double(_))
            case p: FloatProperty =>
              Default(p).extract[Float].map(Lit.Float(_))
            case p: IntegerProperty =>
              Default(p).extract[Int].map(Lit.Int(_))
            case p: LongProperty =>
              Default(p).extract[Long].map(Lit.Long(_))
            case p: StringProperty =>
              Default(p).extract[String].map(Lit.String(_))
            case _ =>
              None
          }

          val SwaggerUtil.PropMeta(declType: Type, dep, _) = SwaggerUtil.propMeta(property)

          val (finalDeclType, finalDefaultValue) =
            Option(property.getRequired)
              .filterNot(_ == false)
              .fold[(Type, Option[Term])](
                (t"Option[${declType}]", Some(defaultValue.fold[Term](q"None")(t => q"Option($t)")))
              )(Function.const((declType, defaultValue)) _)

          (param"${Term.Name(argName)}: ${finalDeclType}".copy(default=finalDefaultValue), dep)
        }

        val dep = ref.filterNot(_.value == clsName) // Filter out our own class name
        val readOnly = Option(name).filter(_ => Option(property.getReadOnly).exists(Boolean.unbox))
        val needsEmptyToNull = property match {
          case d: DateProperty => ScalaEmptyIsNull(d)
          case dt: DateTimeProperty => ScalaEmptyIsNull(dt)
          case s: StringProperty => ScalaEmptyIsNull(s)
          case _ => None
        }
        Target.pure(ProtocolParameter(term, name, dep, readOnly, needsEmptyToNull.filter(_ == true).map(_ => argName)))

      case RenderDTOClass(clsName, terms) =>
        Target.pure(q"""
          case class ${Type.Name(clsName)}(..${terms})
        """)

      case EncodeModel(clsName, needCamelSnakeConversion, params) =>
        val readOnlyKeys: List[String] = params.flatMap(_.readOnlyKey).toList
        val paramCount = params.length
        val typeName = Type.Name(clsName)
        val encVal = if (paramCount == 1) {
          val (names, fields): (Seq[Lit], Seq[Term.Name]) = params.map(param => (Lit.String(param.name), Term.Name(param.term.name.value))).to[Seq].unzip
          val Seq(name) = names
          val Seq(field) = fields
          q"""
            Encoder.forProduct1(${name})((o: ${Type.Name(clsName)}) => o.${field})
          """
        } else if (paramCount >= 2 && paramCount <= 22) {
          val (names, fields): (Seq[Lit], Seq[Term.Name]) = params.map(param => (Lit.String(param.name), Term.Name(param.term.name.value))).to[Seq].unzip
          val tupleFields = fields.map({ field =>
              Term.Select(Term.Name("o"), field)
            }).to[Seq]

          val unapply: Term.Function = Term.Function(
            Seq(param"o: ${Type.Name(clsName)}"),
            Term.Tuple(tupleFields)
          )
          q"""
            Encoder.${Term.Name(s"forProduct${paramCount}")}(..${names})(${unapply})
          """
        } else {
          val pairs: Seq[Term.Tuple] = params.map(param => q"""(${Lit.String(param.name)}, a.${Term.Name(param.term.name.value)}.asJson)""").to[Seq]
          q"""
            new ObjectEncoder[${Type.Name(clsName)}] {
              final def encodeObject(a: ${Type.Name(clsName)}): JsonObject = JsonObject.fromIterable(Vector(..${pairs}))
            }
          """
        }
        Target.pure(q"""
          implicit val ${suffixClsName("encode", clsName)} = {
            val readOnlyKeys = Set[String](..${readOnlyKeys.map(Lit.String(_))})
            $encVal.mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          }
        """)

      case DecodeModel(clsName, needCamelSnakeConversion, params) =>
        val emptyToNullKeys: List[String] = params.flatMap(_.emptyToNullKey).toList
        val paramCount = params.length
        val decVal = if (paramCount <= 22 && emptyToNullKeys.isEmpty) {
          val names: Seq[Lit] = params.map(_.name).map(Lit.String(_)).to[Seq]
          q"""
            Decoder.${Term.Name(s"forProduct${paramCount}")}(..${names})(${Term.Name(clsName)}.apply _)
          """
        } else {
          val (terms, enumerators): (Seq[Term.Name], Seq[Enumerator.Generator]) = params.map({ param =>
            val tpe: Type = param.term.decltpe.flatMap({
              case tpe: Type => Some(tpe)
              case x =>
                println(s"Unsure how to map ${x.structure}, please report this bug!")
                None
            }).get
            val term = Term.Name(param.term.name.value)
            val enum = if (emptyToNullKeys contains param.name) {
              enumerator"""
                ${Pat.Var.Term(term)} <- c.downField(${Lit.String(param.name)}).withFocus(j => j.asString.fold(j)(s => if(s.isEmpty) Json.Null else j)).as[${tpe}]
              """
            } else {
              enumerator"""
                ${Pat.Var.Term(term)} <- c.downField(${Lit.String(param.name)}).as[${tpe}]
              """
            }
            (term, enum)
          }).to[Seq].unzip
          q"""
          new Decoder[${Type.Name(clsName)}] {
            final def apply(c: HCursor): Decoder.Result[${Type.Name(clsName)}] =
              for {
                ..${enumerators}
              } yield ${Term.Name(clsName)}(..${terms})
          }
          """
        }
        Target.pure(q"""
          implicit val ${suffixClsName("decode", clsName)} = $decVal
        """)

      case RenderDTOCompanion(clsName, deps, encoder, decoder) =>
        val extraImports: Seq[Import] = deps.map { term =>
          q"import ${term}._"
        }
        Target.pure(
          q"""object ${Term.Name(clsName)} {
            ..${
              extraImports :+
              encoder :+
              decoder
            }
          }
          """)
    }
  }

  object AliasProtocolTermInterp extends (AliasProtocolTerm ~> Target) {
    def apply[T](term: AliasProtocolTerm[T]): Target[T] = term match {
      case RenderAlias(clsName, tpe) =>
        Target.pure(q"type ${Type.Name(clsName)} = ${tpe}")

      case RenderAliasCompanion(clsName) =>
        Target.pure(q"object ${Term.Name(clsName)}")
    }
  }

  object ProtocolSupportTermInterp extends (ProtocolSupportTerm ~> Target) {
    def apply[T](term: ProtocolSupportTerm[T]): Target[T] = term match {
      case ProtocolImports() =>
        Target.pure(Seq(
          q"import io.circe._"
        , q"import io.circe.syntax._"
        , q"import io.circe.generic.semiauto._"
        , q"import cats.syntax.either._"
        ))

      case PackageObjectImports() =>
        Target.pure(Seq(
          q"import java.time._"
        , q"import io.circe.java8.{ time => j8time }"
        ))

      case PackageObjectContents() =>
        Target.pure(q"""
          val decodeLong = implicitly[Decoder[Long]]

          implicit def decodeInstant: Decoder[Instant] = j8time.decodeInstant.or(decodeLong.map(Instant.ofEpochMilli))
          implicit def decodeLocalDate: Decoder[LocalDate] = j8time.decodeLocalDateDefault.or(decodeInstant.map(_.atZone(ZoneOffset.UTC).toLocalDate))
          implicit def decodeOffsetDateTime: Decoder[OffsetDateTime] = j8time.decodeOffsetDateTimeDefault.or(decodeInstant.map(_.atZone(ZoneOffset.UTC).toOffsetDateTime))

          // Unused
          //implicit def decodeLocalDateTime: Decoder[Instant] = ???
          //implicit def decodeLocalTime: Decoder[Instant] = ???
          // implicit def decodeZonedDateTime: Decoder[Instant] = ???

          // Mirror
          implicit val encodeInstant = j8time.encodeInstant
          implicit val encodeLocalDateDefault = j8time.encodeLocalDateDefault
          implicit val encodeLocalDateTimeDefault = j8time.encodeLocalDateTimeDefault
          implicit val encodeLocalTimeDefault = j8time.encodeLocalTimeDefault
          implicit val encodeOffsetDateTimeDefault = j8time.encodeOffsetDateTimeDefault
          implicit val encodeZonedDateTimeDefault = j8time.encodeZonedDateTimeDefault
        """.stats)
    }
  }
}
