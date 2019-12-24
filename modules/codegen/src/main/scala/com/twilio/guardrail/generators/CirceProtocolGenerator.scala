package com.twilio.guardrail
package generators

import _root_.io.swagger.v3.oas.models.media._
import cats.implicits._
import cats.~>
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.core.implicits._
import com.twilio.guardrail.extract.{ DataRedaction, EmptyValueIsNull }
import com.twilio.guardrail.generators.syntax.RichString
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.protocol.terms.protocol._
import scala.collection.JavaConverters._
import scala.meta._

object CirceProtocolGenerator {
  def suffixClsName(prefix: String, clsName: String) = Pat.Var(Term.Name(s"${prefix}${clsName}"))

  def lookupTypeName(tpeName: String, concreteTypes: List[PropMeta[ScalaLanguage]])(f: Type => Type): Option[Type] =
    concreteTypes
      .find(_.clsName == tpeName)
      .map(_.tpe)
      .map(f)

  object EnumProtocolTermInterp extends (EnumProtocolTerm[ScalaLanguage, ?] ~> Target) {
    def apply[T](term: EnumProtocolTerm[ScalaLanguage, T]): Target[T] = term match {
      case ExtractEnum(swagger) =>
        val enumEntries: Option[List[String]] = swagger match {
          case x: StringSchema =>
            Option[java.util.List[String]](x.getEnum()).map(_.asScala.toList)
          case x =>
            Option[java.util.List[_]](x.getEnum()).map(_.asScala.toList.map(_.toString()))
        }
        Target.pure(Either.fromOption(enumEntries, "Model has no enumerations"))

      case RenderMembers(clsName, elems) =>
        Target.pure(Some(q"""
          object members {
            ..${elems
          .map({
            case (value, termName, defaultTerm) =>
              q"""case object ${termName} extends ${Type.Name(clsName)}(${Lit.String(value)})"""
          })
          .to[List]}
          }
        """))

      case EncodeEnum(clsName) =>
        Target.pure(Some(q"""
          implicit val ${suffixClsName("encode", clsName)}: Encoder[${Type.Name(clsName)}] =
            Encoder[String].contramap(_.value)
        """))

      case DecodeEnum(clsName) =>
        Target.pure(Some(q"""
          implicit val ${suffixClsName("decode", clsName)}: Decoder[${Type.Name(clsName)}] =
            Decoder[String].emap(value => parse(value).toRight(${Term
          .Interpolate(Term.Name("s"), List(Lit.String(""), Lit.String(s" not a member of ${clsName}")), List(Term.Name("value")))}))
        """))

      case RenderClass(clsName, tpe, _) =>
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
            definitions = members.to[List] ++
                  terms ++
                  List(Some(values), encoder, decoder).flatten ++
                  implicits ++
                  List(
                    q"def parse(value: String): Option[${Type.Name(clsName)}] = values.find(_.value == value)",
                    q"implicit val order: cats.Order[${Type.Name(clsName)}] = cats.Order.by[${Type.Name(clsName)}, Int](values.indexOf)"
                  )
          )
        )
      case BuildAccessor(clsName, termName) =>
        Target.pure(q"${Term.Name(clsName)}.${Term.Name(termName)}")
    }
  }

  object ModelProtocolTermInterp extends (ModelProtocolTerm[ScalaLanguage, ?] ~> Target) {
    def apply[T](term: ModelProtocolTerm[ScalaLanguage, T]): Target[T] = term match {
      case ExtractProperties(swagger) =>
        swagger
          .refine[Target[List[(String, Tracker[Schema[_]])]]]({ case o: ObjectSchema => o })(
            m => Target.pure(m.downField("properties", _.getProperties).indexedCosequence.value)
          )
          .orRefine({ case c: ComposedSchema => c })({ comp =>
            val extractedProps =
              comp.downField("allOf", _.getAllOf()).indexedDistribute.flatMap(_.downField("properties", _.getProperties).indexedCosequence.value)
            Target.pure(extractedProps)
          })
          .orRefine({ case x: Schema[_] if Option(x.get$ref()).isDefined => x })(
            comp => Target.raiseError(s"Attempted to extractProperties for a ${comp.get.getClass()}, unsure what to do here (${comp.showHistory})")
          )
          .getOrElse(Target.pure(List.empty))
          .map(_.toList)

      case TransformProperty(clsName, name, property, meta, needCamelSnakeConversion, concreteTypes, isRequired, isCustomType, defaultValue) =>
        Target.log.function(s"transformProperty")(for {
          _ <- Target.log.debug(s"Args: (${clsName}, ${name}, ...)")

          argName = if (needCamelSnakeConversion) name.toCamelCase else name
          rawType = RawParameterType(Option(property.getType), Option(property.getFormat))

          readOnlyKey = Option(name).filter(_ => Option(property.getReadOnly).contains(true))
          emptyToNull = (property match {
            case d: DateSchema      => EmptyValueIsNull(d)
            case dt: DateTimeSchema => EmptyValueIsNull(dt)
            case s: StringSchema    => EmptyValueIsNull(s)
            case _                  => None
          }).getOrElse(EmptyIsEmpty)
          dataRedaction = DataRedaction(property).getOrElse(DataVisible)

          (tpe, classDep) = meta match {
            case SwaggerUtil.Resolved(declType, classDep, _, Some(rawType), rawFormat) if SwaggerUtil.isFile(rawType, rawFormat) && !isCustomType =>
              // assume that binary data are represented as a string. allow users to override.
              (t"String", classDep)
            case SwaggerUtil.Resolved(declType, classDep, _, _, _) =>
              (declType, classDep)
            case SwaggerUtil.Deferred(tpeName) =>
              val tpe = concreteTypes.find(_.clsName == tpeName).map(_.tpe).getOrElse {
                println(s"Unable to find definition for ${tpeName}, just inlining")
                Type.Name(tpeName)
              }
              (tpe, Option.empty)
            case SwaggerUtil.DeferredArray(tpeName, containerTpe) =>
              val concreteType = lookupTypeName(tpeName, concreteTypes)(identity)
              val innerType    = concreteType.getOrElse(Type.Name(tpeName))
              (t"${containerTpe.getOrElse(t"Vector")}[$innerType]", Option.empty)
            case SwaggerUtil.DeferredMap(tpeName, customTpe) =>
              val concreteType = lookupTypeName(tpeName, concreteTypes)(identity)
              val innerType    = concreteType.getOrElse(Type.Name(tpeName))
              (t"${customTpe.getOrElse(t"Map")}[String, $innerType]", Option.empty)
          }

          (finalDeclType, finalDefaultValue) = Option(isRequired)
            .filterNot(_ == false)
            .fold[(Type, Option[Term])](
              (t"Option[${tpe}]", Some(defaultValue.fold[Term](q"None")(t => q"Option($t)")))
            )(Function.const((tpe, defaultValue)) _)
          term = param"${Term.Name(argName)}: ${finalDeclType}".copy(default = finalDefaultValue)
          dep  = classDep.filterNot(_.value == clsName) // Filter out our own class name
        } yield ProtocolParameter[ScalaLanguage](term, name, dep, rawType, readOnlyKey, emptyToNull, dataRedaction, finalDefaultValue))

      case RenderDTOClass(clsName, selfParams, parents) =>
        val discriminators     = parents.flatMap(_.discriminators)
        val discriminatorNames = discriminators.map(_.propertyName).toSet
        val parentOpt = if (parents.exists(s => s.discriminators.nonEmpty)) {
          parents.headOption
        } else {
          None
        }
        val params = (parents.reverse.flatMap(_.params) ++ selfParams).filterNot(
          param => discriminatorNames.contains(param.term.name.value)
        )
        val terms = params.map(_.term)

        val toStringMethod = if (params.exists(_.dataRedaction != DataVisible)) {
          def mkToStringTerm(param: ProtocolParameter[ScalaLanguage]): Term = param match {
            case param if param.dataRedaction == DataVisible => q"${Term.Name(param.term.name.value)}.toString()"
            case _                                           => Lit.String("[redacted]")
          }

          val toStringTerms = params.map(p => List(mkToStringTerm(p))).intercalate(List(Lit.String(",")))

          List[Defn.Def](
            q"override def toString: String = ${toStringTerms.foldLeft[Term](Lit.String(s"${clsName}("))(
              (accum, term) => q"$accum + $term"
            )} + ${Lit.String(")")}"
          )
        } else {
          List.empty[Defn.Def]
        }

        val code = parentOpt
          .fold(q"""case class ${Type.Name(clsName)}(..${terms}) { ..$toStringMethod }""")(
            parent =>
              q"""case class ${Type.Name(clsName)}(..${terms}) extends ${template"..${init"${Type.Name(parent.clsName)}(...$Nil)" :: parent.interfaces
                    .map(a => init"${Type.Name(a)}(...$Nil)")} { ..$toStringMethod }"}"""
          )

        Target.pure(code)

      case EncodeModel(clsName, needCamelSnakeConversion, selfParams, parents) =>
        val discriminators     = parents.flatMap(_.discriminators)
        val discriminatorNames = discriminators.map(_.propertyName).toSet
        val params = (parents.reverse.flatMap(_.params) ++ selfParams).filterNot(
          param => discriminatorNames.contains(param.name)
        )
        val readOnlyKeys: List[String] = params.flatMap(_.readOnlyKey).toList
        val paramCount                 = params.length
        val typeName                   = Type.Name(clsName)
        val encVal = if (paramCount == 0) {
          Option.empty[Term]
        } else
          /* Temporarily removing forProductN due to https://github.com/circe/circe/issues/561
        if (paramCount == 1) {
          val (names, fields): (List[Lit], List[Term.Name]) = params
            .map(param => (Lit.String(param.name), Term.Name(param.term.name.value)))
            .to[List]
            .unzip
          val List(name)  = names
          val List(field) = fields
          Option(
            q"""
              Encoder.forProduct1(${name})((o: ${Type.Name(clsName)}) => o.${field})
            """
          )
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
          Option(
            q"""
              Encoder.${Term.Name(s"forProduct${paramCount}")}(..${names})(${unapply})
            """
          )
        } else */ {
            val pairs: List[Term.Tuple] = params
              .map(param => q"""(${Lit.String(param.name)}, a.${Term.Name(param.term.name.value)}.asJson)""")
              .to[List]
            Option(
              q"""
              new Encoder.AsObject[${Type.Name(clsName)}] {
                final def encodeObject(a: ${Type
                .Name(clsName)}): JsonObject = JsonObject.fromIterable(Vector(..${pairs}))
              }
            """
            )
          }
        Target.pure(encVal.map(encVal => q"""
            implicit val ${suffixClsName("encode", clsName)}: Encoder.AsObject[${Type.Name(clsName)}] = {
              val readOnlyKeys = Set[String](..${readOnlyKeys.map(Lit.String(_))})
              $encVal.mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
            }
          """))

      case DecodeModel(clsName, needCamelSnakeConversion, selfParams, parents) =>
        val discriminators     = parents.flatMap(_.discriminators)
        val discriminatorNames = discriminators.map(_.propertyName).toSet
        val params = (parents.reverse.flatMap(_.params) ++ selfParams).filterNot(
          param => discriminatorNames.contains(param.name)
        )
        val needsEmptyToNull: Boolean = params.exists(_.emptyToNull == EmptyIsNull)
        val paramCount                = params.length
        for {
          decVal <- if (paramCount == 0) {
            Target.pure(Option.empty[Term])
          } else
            /* Temporarily removing forProductN due to https://github.com/circe/circe/issues/561
          if (paramCount <= 22 && !needsEmptyToNull) {
            val names: List[Lit] = params.map(_.name).map(Lit.String(_)).to[List]
            Target.pure(
              Option(
                q"""
                  Decoder.${Term.Name(s"forProduct${paramCount}")}(..${names})(${Term
                  .Name(clsName)}.apply _)
                """
              )
            )
          } else */ {
              params.zipWithIndex
                .traverse({
                  case (param, idx) =>
                    for {
                      rawTpe <- Target.fromOption(param.term.decltpe, "Missing type")
                      tpe <- rawTpe match {
                        case tpe: Type => Target.pure(tpe)
                        case x         => Target.raiseError(s"Unsure how to map ${x.structure}, please report this bug!")
                      }
                    } yield {
                      val term = Term.Name(s"v$idx")
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
                    }
                })
                .map({ pairs =>
                  val (terms, enumerators) = pairs.unzip
                  Option(
                    q"""
                    new Decoder[${Type.Name(clsName)}] {
                      final def apply(c: HCursor): Decoder.Result[${Type.Name(clsName)}] =
                        for {
                          ..${enumerators}
                        } yield ${Term.Name(clsName)}(..${terms})
                    }
                  """
                  )
                })
            }
        } yield {
          decVal.map(decVal => q"""
              implicit val ${suffixClsName("decode", clsName)}: Decoder[${Type.Name(clsName)}] = $decVal
            """)
        }

      case RenderDTOStaticDefns(clsName, deps, encoder, decoder) =>
        val extraImports: List[Import] = deps.map { term =>
          q"import ${term}._"
        }
        Target.pure(
          StaticDefns[ScalaLanguage](
            className = clsName,
            extraImports = extraImports,
            definitions = List(encoder, decoder).flatten
          )
        )
    }
  }

  object ArrayProtocolTermInterp extends (ArrayProtocolTerm[ScalaLanguage, ?] ~> Target) {
    def apply[T](term: ArrayProtocolTerm[ScalaLanguage, T]): Target[T] = term match {
      case ExtractArrayType(arr, concreteTypes) =>
        for {
          result <- arr match {
            case SwaggerUtil.Resolved(tpe, dep, default, _, _) => Target.pure(tpe)
            case SwaggerUtil.Deferred(tpeName) =>
              Target.fromOption(lookupTypeName(tpeName, concreteTypes)(identity), s"Unresolved reference ${tpeName}")
            case SwaggerUtil.DeferredArray(tpeName, containerTpe) =>
              Target.fromOption(
                lookupTypeName(tpeName, concreteTypes)(tpe => t"${containerTpe.getOrElse(t"Vector")}[${tpe}]"),
                s"Unresolved reference ${tpeName}"
              )
            case SwaggerUtil.DeferredMap(tpeName, customTpe) =>
              Target.fromOption(
                lookupTypeName(tpeName, concreteTypes)(tpe => t"Vector[${customTpe.getOrElse(t"Map")}[String, ${tpe}]]"),
                s"Unresolved reference ${tpeName}"
              )
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
            q"import cats.syntax.either._",
            q"import io.circe._",
            q"import io.circe.syntax._",
            q"import io.circe.generic.semiauto._",
            q"import cats.implicits._"
          )
        )

      case PackageObjectImports() =>
        Target.pure(
          List(
            q"import java.time._"
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
        def allParents: Tracker[Schema[_]] => Target[List[(String, Tracker[Schema[_]], List[Tracker[Schema[_]]])]] =
          _.refine[Target[List[(String, Tracker[Schema[_]], List[Tracker[Schema[_]]])]]]({ case x: ComposedSchema => x })(
            _.downField("allOf", _.getAllOf()).indexedDistribute.filter(_.downField("$ref", _.get$ref()).unwrapTracker.nonEmpty) match {
              case head :: tail =>
                definitions
                  .collectFirst({
                    case (clsName, e) if head.downField("$ref", _.get$ref()).exists(_.get.endsWith(s"/$clsName")) =>
                      val thisParent = (clsName, e, tail)
                      allParents(e).map(otherParents => thisParent :: otherParents)
                  })
                  .getOrElse(Target.raiseError(s"Reference ${head.downField("$ref", _.get$ref()).get} not found among definitions"))
              case _ => Target.pure(List.empty)
            }
          ).getOrElse(Target.pure(List.empty))
        allParents(swagger)

      case RenderADTStaticDefns(clsName, discriminator, encoder, decoder) =>
        Target.pure(
          StaticDefns[ScalaLanguage](
            className = clsName,
            extraImports = List.empty[Import],
            definitions = List[Option[Defn]](
              Some(q"val discriminator: String = ${Lit.String(discriminator.propertyName)}"),
              encoder,
              decoder
            ).flatten
          )
        )

      case DecodeADT(clsName, discriminator, children) =>
        val (childrenCases, childrenDiscriminators) = children
          .map({ child =>
            val discriminatorValue = discriminator.mapping
              .collectFirst({ case (value, elem) if elem.name == child => value })
              .getOrElse(child)
            (
              p"case ${Lit.String(discriminatorValue)} => c.as[${Type.Name(child)}]",
              discriminatorValue
            )
          })
          .unzip
        val code =
          q"""implicit val decoder: Decoder[${Type.Name(clsName)}] = Decoder.instance({ c =>
                 val discriminatorCursor = c.downField(discriminator)
                 discriminatorCursor.as[String].flatMap {
                   ..case $childrenCases;
                   case tpe =>
                     Left(DecodingFailure("Unknown value " ++ tpe ++ ${Lit
            .String(s" (valid: ${childrenDiscriminators.mkString(", ")})")}, discriminatorCursor.history))
                 }
            })"""
        Target.pure(Some(code))

      case EncodeADT(clsName, discriminator, children) =>
        val childrenCases = children.map({ child =>
          val discriminatorValue = discriminator.mapping
            .collectFirst({ case (value, elem) if elem.name == child => value })
            .getOrElse(child)
          p"case e:${Type.Name(child)} => e.asJsonObject.add(discriminator, ${Lit.String(discriminatorValue)}.asJson).asJson"
        })
        val code =
          q"""implicit val encoder: Encoder[${Type.Name(clsName)}] = Encoder.instance {
              ..case $childrenCases
          }"""
        Target.pure(Some(code))

      case RenderSealedTrait(className, params, discriminator, parents, _) =>
        for {
          testTerms <- (
            params
              .map(_.term)
              .filter(_.name.value != discriminator.propertyName)
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
