package com.twilio.guardrail.generators.circe.protocol

import java.util.Locale

import _root_.io.swagger.models.ModelImpl
import _root_.io.swagger.models.properties._
import cats.data.EitherT
import cats.implicits._
import cats.kernel.Semigroup
import cats.~>
import com.twilio.guardrail.extract.{ Default, ScalaEmptyIsNull, ScalaType }
import com.twilio.guardrail.generators.GeneratorSettings
import com.twilio.guardrail.protocol.terms.protocol._
import com.twilio.guardrail.swagger.SwaggerUtil
import com.twilio.guardrail.{ ProtocolParameter, Target }

import scala.collection.JavaConverters._
import scala.collection.immutable
import scala.meta._

object CirceProtocolGenerator {

  def suffixClsName(prefix: String, clsName: String) = Pat.Var(Term.Name(s"$prefix${clsName}"))

  def lookupTypeName(tpeName: String, concreteTypes: List[PropMeta])(f: Type => Type): Option[Type] =
    concreteTypes
      .find(_.clsName == tpeName)
      .map(_.tpe)
      .map(f)

  object EnumProtocolTermInterp extends (EnumProtocolTerm ~> Target) {
    def apply[T](term: EnumProtocolTerm[T]): Target[T] = term match {
      case ExtractEnum(swagger) =>
        Target.pure(Either.fromOption(Option(swagger.getEnum()).map(_.asScala.to[List]), "Model has no enumerations"))

      case ExtractType(swagger) =>
        // Default to `string` for untyped enums.
        // Currently, only plain strings are correctly supported anyway, so no big loss.
        val tpeName = Option(swagger.getType).getOrElse("string")
        Target.getGeneratorSettings.map { implicit gs: GeneratorSettings =>
          Either.right(SwaggerUtil.typeName(tpeName, Option(swagger.getFormat), ScalaType(swagger)))
        }

      case RenderMembers(clsName, elems) =>
        Target.pure(q"""
          object members {
            ..${elems
          .map({
            case (value, termName, defaultTerm) =>
              q"""case object ${termName} extends ${Type.Name(clsName)}(${Lit.String(value)})"""
          })
          .to[List]}
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
            Decoder[String].emap(value => parse(value).toRight(${Term
          .Interpolate(Term.Name("s"), List(Lit.String(""), Lit.String(s" not a member of ${clsName}")), List(Term.Name("value")))}))
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
            ..${List(members) ++
          accessors ++
          List(values) ++
          List(q"def parse(value: String): Option[${Type.Name(clsName)}] = values.find(_.value == value)") ++
          List(encoder) ++
          List(decoder) ++
          List(q"implicit val ${Pat.Var(Term.Name(s"addPath${clsName}"))}: AddPath[${Type.Name(clsName)}] = AddPath.build(_.value)") ++
          List(q"implicit val ${Pat.Var(Term.Name(s"show${clsName}"))}: Show[${Type.Name(clsName)}] = Show.build(_.value)")}
          }
        """)
    }
  }

  def toCamelCase(s: String): String =
    "[_\\.]([a-z])".r.replaceAllIn(s, m => m.group(1).toUpperCase(Locale.US))

  object ModelProtocolTermInterp extends (ModelProtocolTerm ~> Target) {
    def apply[T](term: ModelProtocolTerm[T]): Target[T] = term match {
      case ExtractChildProperties(parent, child, discriminator) =>
        val allProps = Semigroup[Option[List[(String, Property)]]].combine(
          Option(parent.getProperties).map(_.asScala.toList),
          Option(child.getChild.getProperties).map(_.asScala.toList)
        )
        val omitDiscriminator = allProps.map(_.filter { case (p, _) => p != discriminator })

        Target.pure(Either.fromOption(omitDiscriminator, "Model has no properties"))

      case ExtractProperties(swagger) =>
        Target.pure(Either.fromOption(Option(swagger.getProperties()).map(_.asScala.toList), "Model has no properties"))

      case TransformProperty(clsName, name, property, needCamelSnakeConversion, concreteTypes) =>
        Target.getGeneratorSettings.flatMap { implicit gs =>
          for {
            _ <- Target.log.debug("definitions", "circe", "modelProtocolTerm")(s"Generated ProtocolParameter(${term}, ${name}, ...)")

            argName = if (needCamelSnakeConversion) toCamelCase(name) else name
            meta <- SwaggerUtil.propMeta(property)

            defaultValue = property match {
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

            readOnlyKey = Option(name).filter(_ => Option(property.getReadOnly).contains(true))
            needsEmptyToNull = property match {
              case d: DateProperty      => ScalaEmptyIsNull(d)
              case dt: DateTimeProperty => ScalaEmptyIsNull(dt)
              case s: StringProperty    => ScalaEmptyIsNull(s)
              case _                    => None
            }
            emptyToNullKey = needsEmptyToNull.filter(_ == true).map(_ => argName)

            (tpe, rawDep) = meta match {
              case SwaggerUtil.Resolved(declType, rawDep, _) =>
                (declType, rawDep)
              case SwaggerUtil.Deferred(tpeName) =>
                val tpe = concreteTypes.find(_.clsName == tpeName).map(_.tpe).getOrElse {
                  println(s"Unable to find definition for ${tpeName}, just inlining")
                  Type.Name(tpeName)
                }
                (tpe, Option.empty)
              case SwaggerUtil.DeferredArray(tpeName) =>
                (t"IndexedSeq[${Type.Name(tpeName)}]", Option.empty)
              case SwaggerUtil.DeferredMap(tpeName) =>
                (t"Map[String, ${Type.Name(tpeName)}]", Option.empty)
            }

            (finalDeclType, finalDefaultValue) = Option(property.getRequired)
              .filterNot(_ == false)
              .fold[(Type, Option[Term])](
                (t"Option[${tpe}]", Some(defaultValue.fold[Term](q"None")(t => q"Option($t)")))
              )(Function.const((tpe, defaultValue)) _)
            term = param"${Term.Name(argName)}: ${finalDeclType}".copy(default = finalDefaultValue)
            dep  = rawDep.filterNot(_.value == clsName) // Filter out our own class name
          } yield ProtocolParameter(term, name, dep, readOnlyKey, emptyToNullKey)
        }

      case RenderDTOClass(clsName, terms, parentNameOpt) =>
        val code = parentNameOpt
          .fold(q"""case class ${Type.Name(clsName)}(..${terms})""")(
            parentName => q"""case class ${Type.Name(clsName)}(..${terms}) extends AbstractPet"""
          )

        Target.pure(code)

      case EncodeModel(clsName, needCamelSnakeConversion, params) =>
        val readOnlyKeys: List[String] = params.flatMap(_.readOnlyKey).toList
        val paramCount                 = params.length
        val typeName                   = Type.Name(clsName)
        val encVal = if (paramCount == 1) {
          val (names, fields): (List[Lit], List[Term.Name]) = params
            .map(param => (Lit.String(param.name), Term.Name(param.term.name.value)))
            .unzip

          val List(name)  = names
          val List(field) = fields
          q"""
            Encoder.forProduct1(${name})((o: ${Type.Name(clsName)}) => o.${field})
          """
        } else if (paramCount >= 2 && paramCount <= 22) {
          val (names, fields): (List[Lit], List[Term.Name]) = params
            .map(param => (Lit.String(param.name), Term.Name(param.term.name.value)))
            .unzip
          val tupleFields = fields
            .map({ field =>
              Term.Select(Term.Name("o"), field)
            })

          val unapply: Term.Function = Term.Function(
            List(param"o: ${Type.Name(clsName)}"),
            Term.Tuple(tupleFields)
          )
          q"""
            Encoder.${Term.Name(s"forProduct${paramCount}")}(..${names})(${unapply})
          """
        } else {
          val pairs: List[Term.Tuple] = params
            .map(param => q"""(${Lit.String(param.name)}, a.${Term.Name(param.term.name.value)}.asJson)""")
          q"""
            new ObjectEncoder[${Type.Name(clsName)}] {
              final def encodeObject(a: ${Type
            .Name(clsName)}): JsonObject = JsonObject.fromIterable(Vector(..${pairs}))
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
        val paramCount                    = params.length
        val decVal = if (paramCount <= 22 && emptyToNullKeys.isEmpty) {
          val names: List[Lit] = params.map(_.name).map(Lit.String(_)).to[List]
          q"""
            Decoder.${Term.Name(s"forProduct${paramCount}")}(..${names})(${Term
            .Name(clsName)}.apply _)
          """
        } else {
          val (terms, enumerators): (List[Term.Name], List[Enumerator.Generator]) = params
            .map({ param =>
              val tpe: Type = param.term.decltpe
                .flatMap({
                  case tpe: Type => Some(tpe)
                  case x =>
                    println(s"Unsure how to map ${x.structure}, please report this bug!")
                    None
                })
                .get
              val term = Term.Name(param.term.name.value)
              val enum = if (emptyToNullKeys contains param.name) {
                enumerator"""
                ${Pat.Var(term)} <- c.downField(${Lit
                  .String(param.name)}).withFocus(j => j.asString.fold(j)(s => if(s.isEmpty) Json.Null else j)).as[${tpe}]
              """
              } else {
                enumerator"""
                ${Pat.Var(term)} <- c.downField(${Lit.String(param.name)}).as[${tpe}]
              """
              }
              (term, enum)
            })
            .to[List]
            .unzip
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
        val extraImports: List[Import] = deps.map { term =>
          q"import ${term}._"
        }
        Target.pure(
          q"""object ${Term.Name(clsName)} {
            ..${extraImports :+
            encoder :+
            decoder}
          }
          """
        )
    }
  }

  object AliasProtocolTermInterp extends (AliasProtocolTerm ~> Target) {
    def apply[T](term: AliasProtocolTerm[T]): Target[T] = term match {
      case _ => ???
    }
  }

  object ArrayProtocolTermInterp extends (ArrayProtocolTerm ~> Target) {
    def apply[T](term: ArrayProtocolTerm[T]): Target[T] = term match {
      case ExtractArrayType(arr, concreteTypes) =>
        SwaggerUtil.modelMetaType(arr).flatMap {
          case SwaggerUtil.Resolved(tpe, dep, default) =>
            Target.pure(tpe)
          case SwaggerUtil.Deferred(tpeName) =>
            Target.fromOption(lookupTypeName(tpeName, concreteTypes)(identity), s"Unresolved reference $tpeName")
          case SwaggerUtil.DeferredArray(tpeName) =>
            Target.fromOption(lookupTypeName(tpeName, concreteTypes)(tpe => t"IndexedSeq[$tpe]"), s"Unresolved reference $tpeName")
          case SwaggerUtil.DeferredMap(tpeName) =>
            Target
              .fromOption(lookupTypeName(tpeName, concreteTypes)(tpe => t"IndexedSeq[Map[String, $tpe]]"), s"Unresolved reference $tpeName")
        }
    }
  }

  //fixme checkpoint
  object PolyProtocolTermInterp extends (PolyProtocolTerm ~> Target) {
    override def apply[A](fa: PolyProtocolTerm[A]): Target[A] = fa match {
      case RenderADTCompanion(clsName, discriminator, encoder, decoder) =>
        val code = q"""object ${Term.Name(clsName)} extends AutoDerivation {
            ..${List(discriminator, encoder, decoder)}
          }
          """

        Target.pure(code)

      case RenderDiscriminator(discriminator) =>
        val code = q"""implicit val configuration: Configuration = Configuration.default.withDiscriminator($discriminator)"""
        Target.pure(code)

      case DecodeADT(clsName, needCamelSnakeConversion, params) =>
        val code = if (needCamelSnakeConversion) {
          q"""implicit val decoder: Decoder[AbstractPet] = deriveDecoder[AbstractPet](io.circe.derivation.renaming.snakeCase)"""
        } else {
          q"""implicit val decoder: Decoder[AbstractPet] = deriveDecoder[AbstractPet]"""
        }

        Target.pure(code)

      case EncodeADT(clsName, needCamelSnakeConversion, params) =>
        // import io.circe.generic.extras.semiauto._
        val code = if (needCamelSnakeConversion) {
          q"""implicit val encoder: Encoder[AbstractPet] = deriveEncoder[AbstractPet](io.circe.derivation.renaming.snakeCase)"""
        } else {
          q"""implicit val encoder: Encoder[AbstractPet] = deriveEncoder[AbstractPet]"""
        }

        Target.pure(code)

      case RenderSealedTrait(className, terms, discriminator: String) =>
        //fixme: Discriminator shouldn't be rendered
        val testTerms = terms
          .filter(_.name.value != discriminator)
          .map { t =>
            q"""def ${Term.Name(t.name.value)} : ${t.decltpe.getOrElse(Type.Name("Any"))}"""
          }

        Target.pure {
          q"""sealed trait ${Type.Name(className)} {..${testTerms}}"""
        }
    }
  }

}
