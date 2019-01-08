package com.twilio.guardrail
package generators

import _root_.io.swagger.v3.oas.models.media._
import cats.implicits._
import cats.~>
import cats.data.NonEmptyList
import com.twilio.guardrail.extract.{ Default, ScalaEmptyIsNull, ScalaType }
import com.twilio.guardrail.shims._
import com.twilio.guardrail.terms
import java.util.Locale
import com.twilio.guardrail.languages.{ LA, ScalaLanguage }
import com.twilio.guardrail.protocol.terms.protocol._
import scala.collection.JavaConverters._
import scala.meta._

object CirceProtocolGenerator {
  import ProtocolGenerator._

  def suffixClsName(prefix: String, clsName: String) = Pat.Var(Term.Name(s"${prefix}${clsName}"))

  def lookupTypeName(tpeName: String, concreteTypes: List[PropMeta[ScalaLanguage]])(f: Type => Type): Option[Type] =
    concreteTypes
      .find(_.clsName == tpeName)
      .map(_.tpe)
      .map(f)

  object EnumProtocolTermInterp extends (EnumProtocolTerm[ScalaLanguage, ?] ~> Target) {
    def apply[T](term: EnumProtocolTerm[ScalaLanguage, T]): Target[T] = term match {
      case ExtractEnum(swagger) =>
        Target.pure(Either.fromOption(Option[java.util.List[String]](swagger.getEnum()).map(_.asScala.toList), "Model has no enumerations"))

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
            Decoder[String].emap(value => parse(value).toRight(${Term.Interpolate(Term.Name("s"),
                                                                                  List(Lit.String(""), Lit.String(s" not a member of ${clsName}")),
                                                                                  List(Term.Name("value")))}))
        """)

      case RenderClass(clsName, tpe) =>
        Target.pure(q"""
          sealed abstract class ${Type.Name(clsName)}(val value: ${tpe}) {
            override def toString: String = value.toString
          }
        """)

      case RenderStaticDefns(clsName, members, accessors, encoder, decoder) =>
        val terms: List[Defn.Val] = accessors
          .map({ pascalValue =>
            q"val ${Pat.Var(pascalValue)}: ${Type.Name(clsName)} = members.${pascalValue}"
          })
          .to[List]
        val values: Defn.Val = q"val values = Vector(..$accessors)"
        val implicits: List[Defn.Val] = List(
          q"implicit val ${Pat.Var(Term.Name(s"addPath${clsName}"))}: AddPath[${Type.Name(clsName)}] = AddPath.build(_.value)",
          q"implicit val ${Pat.Var(Term.Name(s"show${clsName}"))}: Show[${Type.Name(clsName)}] = Show.build(_.value)"
        )
        Target.pure(
          StaticDefns[ScalaLanguage](
            className = clsName,
            extraImports = List.empty[Import],
            members = List(members),
            definitions = List(
              q"def parse(value: String): Option[${Type.Name(clsName)}] = values.find(_.value == value)"
            ),
            values = terms ++ List(values) ++ List(encoder, decoder) ++ implicits
          )
        )
      case BuildAccessor(clsName, termName) =>
        Target.pure(q"${Term.Name(clsName)}.${Term.Name(termName)}")
    }
  }

  object ModelProtocolTermInterp extends (ModelProtocolTerm[ScalaLanguage, ?] ~> Target) {
    def apply[T](term: ModelProtocolTerm[ScalaLanguage, T]): Target[T] = term match {
      case ExtractProperties(swagger) =>
        Target.pure(
          (swagger match {
            case m: ObjectSchema      => Option(m.getProperties)
            case comp: ComposedSchema => comp.getAllOf().asScala.toList.lastOption.flatMap(prop => Option(prop.getProperties))
            case comp: Schema[_] if comp.getSimpleRef.isDefined =>
              Option(comp.getProperties)
            case _ => None
          }).map(_.asScala.toList).toList.flatten
        )

      case TransformProperty(clsName, name, property, meta, needCamelSnakeConversion, concreteTypes, isRequired) =>
        def toCamelCase(s: String): String =
          "[_\\.]([a-z])".r.replaceAllIn(s, m => m.group(1).toUpperCase(Locale.US))

        for {
          _ <- Target.log.debug("definitions", "circe", "modelProtocolTerm")(s"Generated ProtocolParameter(${term}, ${name}, ...)")

          argName = if (needCamelSnakeConversion) toCamelCase(name) else name

          defaultValue = property match {
            case _: MapSchema =>
              Option(q"Map.empty")
            case _: ArraySchema =>
              Option(q"IndexedSeq.empty")
            case p: BooleanSchema =>
              Default(p).extract[Boolean].map(Lit.Boolean(_))
            case p: NumberSchema if p.getFormat == "double" =>
              Default(p).extract[Double].map(Lit.Double(_))
            case p: NumberSchema if p.getFormat == "float"  =>
              Default(p).extract[Float].map(Lit.Float(_))
            case p: IntegerSchema if p.getFormat == "int32" =>
              Default(p).extract[Int].map(Lit.Int(_))
            case p: IntegerSchema if p.getFormat == "int64" =>
              Default(p).extract[Long].map(Lit.Long(_))
            case p: StringSchema                            =>
              Default(p).extract[String].map(Lit.String(_))
            case _ =>
              None
          }

          readOnlyKey = Option(name).filter(_ => Option(property.getReadOnly).contains(true))
          emptyToNull = (property match {
            case d: DateSchema      => ScalaEmptyIsNull(d)
            case dt: DateTimeSchema => ScalaEmptyIsNull(dt)
            case s: StringSchema    => ScalaEmptyIsNull(s)
            case _                  => None
          }).getOrElse(EmptyIsEmpty)

          (tpe, classDep) = meta match {
            case SwaggerUtil.Resolved(declType, classDep, _) =>
              (declType, classDep)
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

          (finalDeclType, finalDefaultValue) = Option(isRequired)
            .filterNot(_ == false)
            .fold[(Type, Option[Term])](
              (t"Option[${tpe}]", Some(defaultValue.fold[Term](q"None")(t => q"Option($t)")))
            )(Function.const((tpe, defaultValue)) _)
          term = param"${Term.Name(argName)}: ${finalDeclType}".copy(default = finalDefaultValue)
          dep  = classDep.filterNot(_.value == clsName) // Filter out our own class name
        } yield ProtocolParameter[ScalaLanguage](term, name, dep, readOnlyKey, emptyToNull)

      case RenderDTOClass(clsName, selfTerms, parents) =>
        val discriminators = parents.flatMap(_.discriminators)
        val parenOpt       = parents.headOption
        val terms = (parents.reverse.flatMap(_.params.map(_.term)) ++ selfTerms).filterNot(
          param => discriminators.contains(param.name.value)
        )
        val code = parenOpt
          .fold(q"""case class ${Type.Name(clsName)}(..${terms})""")(
            parent =>
              q"""case class ${Type.Name(clsName)}(..${terms}) extends ${template"..${init"${Type.Name(parent.clsName)}(...$Nil)" :: parent.interfaces
                .map(a => init"${Type.Name(a)}(...$Nil)")}"}"""
          )

        Target.pure(code)

      case EncodeModel(clsName, needCamelSnakeConversion, selfParams, parents) =>
        val discriminators = parents.flatMap(_.discriminators)
        val params = (parents.reverse.flatMap(_.params) ++ selfParams).filterNot(
          param => discriminators.contains(param.name)
        )
        val readOnlyKeys: List[String] = params.flatMap(_.readOnlyKey).toList
        val paramCount                 = params.length
        val typeName                   = Type.Name(clsName)
        val encVal = if (paramCount == 1) {
          val (names, fields): (List[Lit], List[Term.Name]) = params
            .map(param => (Lit.String(param.name), Term.Name(param.term.name.value)))
            .to[List]
            .unzip
          val List(name)  = names
          val List(field) = fields
          q"""
            Encoder.forProduct1(${name})((o: ${Type.Name(clsName)}) => o.${field})
          """
        } else if (paramCount >= 2 && paramCount <= 22) {
          val (names, fields): (List[Lit], List[Term.Name]) = params
            .map(param => (Lit.String(param.name), Term.Name(param.term.name.value)))
            .to[List]
            .unzip
          val tupleFields = fields
            .map({ field =>
              Term.Select(Term.Name("o"), field)
            })
            .to[List]

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
            .to[List]
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

      case DecodeModel(clsName, needCamelSnakeConversion, selfParams, parents) =>
        val discriminators = parents.flatMap(_.discriminators)
        val params = (parents.reverse.flatMap(_.params) ++ selfParams).filterNot(
          param => discriminators.contains(param.name)
        )
        val needsEmptyToNull: Boolean = params.exists(_.emptyToNull == EmptyIsNull)
        val paramCount                = params.length
        val decVal = if (paramCount <= 22 && !needsEmptyToNull) {
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
              val enum = if (param.emptyToNull == EmptyIsNull) {
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

      case RenderDTOStaticDefns(clsName, deps, encoder, decoder) =>
        val extraImports: List[Import] = deps.map { term =>
          q"import ${term}._"
        }
        Target.pure(
          StaticDefns[ScalaLanguage](
            className = clsName,
            extraImports = extraImports,
            members = List.empty,
            definitions = List.empty,
            values = List(encoder, decoder)
          )
        )
    }
  }

  object ArrayProtocolTermInterp extends (ArrayProtocolTerm[ScalaLanguage, ?] ~> Target) {
    def apply[T](term: ArrayProtocolTerm[ScalaLanguage, T]): Target[T] = term match {
      case ExtractArrayType(arr, concreteTypes) =>
        for {
          result <- arr match {
            case SwaggerUtil.Resolved(tpe, dep, default) => Target.pure(tpe)
            case SwaggerUtil.Deferred(tpeName) =>
              Target.fromOption(lookupTypeName(tpeName, concreteTypes)(identity), s"Unresolved reference ${tpeName}")
            case SwaggerUtil.DeferredArray(tpeName) =>
              Target.fromOption(lookupTypeName(tpeName, concreteTypes)(tpe => t"IndexedSeq[${tpe}]"), s"Unresolved reference ${tpeName}")
            case SwaggerUtil.DeferredMap(tpeName) =>
              Target.fromOption(lookupTypeName(tpeName, concreteTypes)(tpe => t"IndexedSeq[Map[String, ${tpe}]]"), s"Unresolved reference ${tpeName}")
          }
        } yield result
    }
  }

  object ProtocolSupportTermInterp extends (ProtocolSupportTerm[ScalaLanguage, ?] ~> Target) {
    def apply[T](term: ProtocolSupportTerm[ScalaLanguage, T]): Target[T] = term match {
      case ExtractConcreteTypes(definitions) =>
        definitions.fold[Target[List[PropMeta[ScalaLanguage]]]](Target.raiseError _, Target.pure _)

      case ProtocolImports() =>
        Target.pure(
          List(
            q"import io.circe._",
            q"import io.circe.syntax._",
            q"import io.circe.generic.semiauto._"
          )
        )

      case PackageObjectImports() =>
        Target.pure(
          List(
            q"import java.time._",
            q"import io.circe.java8.time._"
          )
        )

      case PackageObjectContents() =>
        Target.pure(
          List(
            q"implicit val guardrailDecodeInstant: Decoder[Instant] = Decoder[Instant].or(Decoder[Long].map(Instant.ofEpochMilli))",
            q"implicit val guardrailDecodeLocalDate: Decoder[LocalDate] = Decoder[LocalDate].or(Decoder[Instant].map(_.atZone(ZoneOffset.UTC).toLocalDate))",
            q"implicit val guardrailDecodeLocalDateTime: Decoder[LocalDateTime] = Decoder[LocalDateTime]",
            q"implicit val guardrailDecodeLocalTime: Decoder[LocalTime] = Decoder[LocalTime]",
            q"implicit val guardrailDecodeOffsetDateTime: Decoder[OffsetDateTime] = Decoder[OffsetDateTime].or(Decoder[Instant].map(_.atZone(ZoneOffset.UTC).toOffsetDateTime))",
            q"implicit val guardrailDecodeZonedDateTime: Decoder[ZonedDateTime] = Decoder[ZonedDateTime]",
            q"implicit val guardrailEncodeInstant: Encoder[Instant] = Encoder[Instant]",
            q"implicit val guardrailEncodeLocalDate: Encoder[LocalDate] = Encoder[LocalDate]",
            q"implicit val guardrailEncodeLocalDateTime: Encoder[LocalDateTime] = Encoder[LocalDateTime]",
            q"implicit val guardrailEncodeLocalTime: Encoder[LocalTime] = Encoder[LocalTime]",
            q"implicit val guardrailEncodeOffsetDateTime: Encoder[OffsetDateTime] = Encoder[OffsetDateTime]",
            q"implicit val guardrailEncodeZonedDateTime: Encoder[ZonedDateTime] = Encoder[ZonedDateTime]"
          )
        )
    }
  }

  object PolyProtocolTermInterp extends (PolyProtocolTerm[ScalaLanguage, ?] ~> Target) {
    override def apply[A](fa: PolyProtocolTerm[ScalaLanguage, A]): Target[A] = fa match {
      case ExtractSuperClass(swagger, definitions) =>
        def allParents(model: Schema[_]): List[(String, Schema[_], List[Schema[_]])] =
          (model match {
            case elem: ComposedSchema =>
              NonEmptyList
                .fromList(Option(elem.getAllOf).map(_.asScala.toList).getOrElse(List.empty))
                .flatMap({
                  case NonEmptyList(head, tail) =>
                    definitions.collectFirst {
                      case (clsName, e) if head.getSimpleRef == clsName =>
                        (clsName, e, tail)
                    }
                })
            case _ => None
          }) match {
            case Some(x @ (_, el, _)) => x :: allParents(el)
            case _                    => Nil
          }

        Target.pure(allParents(swagger))

      case RenderADTStaticDefns(clsName, discriminator, encoder, decoder) =>
        Target.pure(
          StaticDefns(
            className = clsName,
            extraImports = List.empty,
            members = List.empty,
            definitions = List.empty,
            values = List(
              q"val discriminator: String = ${Lit.String(discriminator)}",
              encoder,
              decoder
            )
          )
        )

      case DecodeADT(clsName, children) =>
        val childrenCases = children.map(
          child => p"case ${Lit.String(child)} => c.as[${Type.Name(child)}]"
        )
        val code =
          q"""implicit val decoder: Decoder[${Type.Name(clsName)}] = Decoder.instance({ c =>
                 val discriminatorCursor = c.downField(discriminator)
                 discriminatorCursor.as[String].flatMap {
                   ..case $childrenCases;
                   case tpe =>
                     Left(DecodingFailure("Unknown value " ++ tpe ++ ${Lit.String(s" (valid: ${children.mkString(", ")})")}, discriminatorCursor.history))
                 }
            })"""
        Target.pure(code)

      case EncodeADT(clsName, children) =>
        val childrenCases = children.map(
          child => p"case e:${Type.Name(child)} => e.asJsonObject.add(discriminator, ${Lit.String(child)}.asJson).asJson"
        )
        val code =
          q"""implicit val encoder: Encoder[${Type.Name(clsName)}] = Encoder.instance {
              ..case $childrenCases
          }"""
        Target.pure(code)

      case RenderSealedTrait(className, terms, discriminator, parents) =>
        for {
          testTerms <- (
            terms
              .filter(_.name.value != discriminator)
              .traverse { t =>
                for {
                  tpe <- Target.fromOption(
                    t.decltpe
                      .flatMap({
                        case tpe: Type => Some(tpe)
                        case x         => None
                      }),
                    t.decltpe.fold("Nothing to map")(x => s"Unsure how to map ${x.structure}, please report this bug!")
                  )
                } yield q"""def ${Term.Name(t.name.value)}: ${tpe}"""
              }
          )
        } yield {
          parents.headOption
            .fold(q"""trait ${Type.Name(className)} {..${testTerms}}""")(
              parent => q"""trait ${Type.Name(className)} extends ${template"${init"${Type.Name(parent.clsName)}(...$Nil)"}{..${testTerms}}"} """
            )
        }
    }
  }
}
