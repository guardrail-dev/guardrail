package dev.guardrail.generators.Scala

import _root_.io.swagger.v3.oas.models.media.{ Discriminator => _, _ }
import cats.Monad
import cats.data.{ NonEmptyList, NonEmptyVector }
import cats.syntax.all._
import dev.guardrail.{
  Discriminator,
  ProtocolParameter,
  StaticDefns,
  SuperClass,
  SupportDefinition,
  SwaggerUtil,
  Target,
  UserError
}
import dev.guardrail.core
import dev.guardrail.core.{DataVisible, EmptyIsEmpty, EmptyIsNull, ResolvedType, Tracker}
import dev.guardrail.core.implicits._
import dev.guardrail.core.extract.{ DataRedaction, EmptyValueIsNull }
import dev.guardrail.generators.Scala.model.CirceModelGenerator
import dev.guardrail.generators.{ RawParameterName, RawParameterType, ScalaGenerator }
import dev.guardrail.languages.ScalaLanguage
import dev.guardrail.protocol.terms.protocol._
import dev.guardrail.terms.{ CollectionsLibTerms, RenderedEnum, RenderedIntEnum, RenderedLongEnum, RenderedStringEnum }
import scala.meta._

object CirceProtocolGenerator {
  def suffixClsName(prefix: String, clsName: String): Pat.Var = Pat.Var(Term.Name(s"${prefix}${clsName}"))

  def lookupTypeName(tpeName: String, concreteTypes: List[PropMeta[ScalaLanguage]])(f: Type => Type): Option[Type] =
    concreteTypes
      .find(_.clsName == tpeName)
      .map(_.tpe)
      .map(f)

  def EnumProtocolTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): EnumProtocolTerms[ScalaLanguage, Target] = new EnumProtocolTermInterp
  class EnumProtocolTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]) extends EnumProtocolTerms[ScalaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances

    def renderMembers(clsName: String, elems: RenderedEnum[ScalaLanguage]) = {
      val fields = elems match {
        case RenderedStringEnum(elems) =>
          elems.map({
            case (value, termName, _) =>
              (termName, Lit.String(value))
          })
        case RenderedIntEnum(elems) =>
          elems.map({
            case (value, termName, _) =>
              (termName, Lit.Int(value))
          })
        case RenderedLongEnum(elems) =>
          elems.map({
            case (value, termName, _) =>
              (termName, Lit.Long(value))
          })
      }

      Target.pure(Some(q"""
              object members {
                ..${fields.map { case (termName, lit) => q"""case object ${termName} extends ${Type.Name(clsName)}(${lit})""" }}
              }
            """))
    }

    def encodeEnum(clsName: String, tpe: Type): Target[Option[Defn]] =
      Target.pure(Some(q"""
            implicit val ${suffixClsName("encode", clsName)}: _root_.io.circe.Encoder[${Type.Name(clsName)}] =
              _root_.io.circe.Encoder[${tpe}].contramap(_.value)
          """))

    def decodeEnum(clsName: String, tpe: Type): Target[Option[Defn]] =
      Target.pure(Some(q"""
        implicit val ${suffixClsName("decode", clsName)}: _root_.io.circe.Decoder[${Type.Name(clsName)}] =
          _root_.io.circe.Decoder[${tpe}].emap(value => from(value).toRight(${Term
        .Interpolate(Term.Name("s"), List(Lit.String(""), Lit.String(s" not a member of ${clsName}")), List(Term.Name("value")))}))
      """))

    def renderClass(clsName: String, tpe: scala.meta.Type, elems: RenderedEnum[ScalaLanguage]) =
      Target.pure(q"""
        sealed abstract class ${Type.Name(clsName)}(val value: ${tpe}) extends _root_.scala.Product with _root_.scala.Serializable {
          override def toString: String = value.toString
        }
      """)

    def renderStaticDefns(
        clsName: String,
        tpe: scala.meta.Type,
        members: Option[scala.meta.Defn.Object],
        accessors: List[scala.meta.Term.Name],
        encoder: Option[scala.meta.Defn],
        decoder: Option[scala.meta.Defn]
    ): Target[StaticDefns[ScalaLanguage]] = {
      val longType = Type.Name(clsName)
      val terms: List[Defn.Val] = accessors
        .map({ pascalValue =>
          q"val ${Pat.Var(pascalValue)}: ${longType} = members.${pascalValue}"
        })
        .toList
      val values: Defn.Val = q"val values = _root_.scala.Vector(..$accessors)"
      val implicits: List[Defn.Val] = List(
        q"implicit val ${Pat.Var(Term.Name(s"show${clsName}"))}: Show[${longType}] = Show[${tpe}].contramap[${longType}](_.value)"
      )
      Target.pure(
        StaticDefns[ScalaLanguage](
          className = clsName,
          extraImports = List.empty[Import],
          definitions = members.toList ++
                terms ++
                List(Some(values), encoder, decoder).flatten ++
                implicits ++
                List(
                  q"def from(value: ${tpe}): _root_.scala.Option[${longType}] = values.find(_.value == value)",
                  q"implicit val order: cats.Order[${longType}] = cats.Order.by[${longType}, Int](values.indexOf)"
                )
        )
      )
    }

    def buildAccessor(clsName: String, termName: String) =
      Target.pure(q"${Term.Name(clsName)}.${Term.Name(termName)}")
  }

  def ModelProtocolTermInterp(
      circeVersion: CirceModelGenerator
  )(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): ModelProtocolTerms[ScalaLanguage, Target] =
    new ModelProtocolTermInterp(circeVersion)
  class ModelProtocolTermInterp(circeVersion: CirceModelGenerator)(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target])
      extends ModelProtocolTerms[ScalaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances
    def extractProperties(swagger: Tracker[Schema[_]]) =
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
          comp => Target.raiseUserError(s"Attempted to extractProperties for a ${comp.unwrapTracker.getClass()}, unsure what to do here (${comp.showHistory})")
        )
        .getOrElse(Target.pure(List.empty))
        .map(_.toList)

    def transformProperty(
        clsName: String,
        dtoPackage: List[String],
        supportPackage: List[String],
        concreteTypes: List[PropMeta[ScalaLanguage]]
    )(
        name: String,
        fieldName: String,
        property: Tracker[Schema[_]],
        meta: ResolvedType[ScalaLanguage],
        requirement: PropertyRequirement,
        isCustomType: Boolean,
        defaultValue: Option[scala.meta.Term]
    ): Target[ProtocolParameter[ScalaLanguage]] =
      Target.log.function(s"transformProperty") {
        for {
          _ <- Target.log.debug(s"Args: (${clsName}, ${name}, ...)")

          rawType = RawParameterType(property.downField("type", _.getType()).unwrapTracker, property.downField("format", _.getFormat()).unwrapTracker)

          readOnlyKey = Option(name).filter(_ => property.downField("readOnly", _.getReadOnly()).unwrapTracker.contains(true))
          emptyToNull = property
            .refine({ case d: DateSchema => d })(d => EmptyValueIsNull(d))
            .orRefine({ case dt: DateTimeSchema => dt })(dt => EmptyValueIsNull(dt))
            .orRefine({ case s: StringSchema => s })(s => EmptyValueIsNull(s))
            .toOption
            .flatten
            .getOrElse(EmptyIsEmpty)

          dataRedaction = DataRedaction(property).getOrElse(DataVisible)

          (tpe, classDep) = meta match {
            case core.Resolved(declType, classDep, _, Some(rawType), rawFormat) if SwaggerUtil.isFile(rawType, rawFormat) && !isCustomType =>
              // assume that binary data are represented as a string. allow users to override.
              (t"String", classDep)
            case core.Resolved(declType, classDep, _, _, _) =>
              (declType, classDep)
            case core.Deferred(tpeName) =>
              val tpe = concreteTypes.find(_.clsName == tpeName).map(_.tpe).getOrElse {
                println(s"Unable to find definition for ${tpeName}, just inlining")
                Type.Name(tpeName)
              }
              (tpe, Option.empty)
            case core.DeferredArray(tpeName, containerTpe) =>
              val concreteType = lookupTypeName(tpeName, concreteTypes)(identity)
              val innerType    = concreteType.getOrElse(Type.Name(tpeName))
              (t"${containerTpe.getOrElse(t"_root_.scala.Vector")}[$innerType]", Option.empty)
            case core.DeferredMap(tpeName, customTpe) =>
              val concreteType = lookupTypeName(tpeName, concreteTypes)(identity)
              val innerType    = concreteType.getOrElse(Type.Name(tpeName))
              (t"${customTpe.getOrElse(t"_root_.scala.Predef.Map")}[_root_.scala.Predef.String, $innerType]", Option.empty)
          }
          presence     <- ScalaGenerator.ScalaInterp.selectTerm(NonEmptyList.ofInitLast(supportPackage, "Presence"))
          presenceType <- ScalaGenerator.ScalaInterp.selectType(NonEmptyList.ofInitLast(supportPackage, "Presence"))
          (finalDeclType, finalDefaultValue) = requirement match {
            case PropertyRequirement.Required => tpe -> defaultValue
            case PropertyRequirement.Optional | PropertyRequirement.Configured(PropertyRequirement.Optional, PropertyRequirement.Optional) =>
              t"$presenceType[$tpe]" -> defaultValue.map(t => q"$presence.Present($t)").orElse(Some(q"$presence.Absent"))
            case _: PropertyRequirement.OptionalRequirement | _: PropertyRequirement.Configured =>
              t"Option[$tpe]" -> defaultValue.map(t => q"Option($t)").orElse(Some(q"None"))
            case PropertyRequirement.OptionalNullable =>
              t"$presenceType[Option[$tpe]]" -> defaultValue.map(t => q"$presence.Present($t)")
          }
          term = param"${Term.Name(fieldName)}: ${finalDeclType}".copy(default = finalDefaultValue)
          dep  = classDep.filterNot(_.value == clsName) // Filter out our own class name
        } yield ProtocolParameter[ScalaLanguage](
          term,
          tpe,
          RawParameterName(name),
          dep,
          rawType,
          readOnlyKey,
          emptyToNull,
          dataRedaction,
          requirement,
          finalDefaultValue
        )
      }

    def renderDTOClass(
        clsName: String,
        supportPackage: List[String],
        selfParams: List[ProtocolParameter[ScalaLanguage]],
        parents: List[SuperClass[ScalaLanguage]] = Nil
    ) = {
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
            q"""case class ${Type.Name(clsName)}(..${terms}) extends ..${init"${Type.Name(parent.clsName)}(...$Nil)" :: parent.interfaces.map(
                  a => init"${Type.Name(a)}(...$Nil)"
                )} { ..$toStringMethod }"""
        )

      Target.pure(code)
    }

    def encodeModel(
        clsName: String,
        dtoPackage: List[String],
        selfParams: List[ProtocolParameter[ScalaLanguage]],
        parents: List[SuperClass[ScalaLanguage]] = Nil
    ) = {
      val discriminators     = parents.flatMap(_.discriminators)
      val discriminatorNames = discriminators.map(_.propertyName).toSet
      val params = (parents.reverse.flatMap(_.params) ++ selfParams).filterNot(
        param => discriminatorNames.contains(param.name.value)
      )
      val readOnlyKeys: List[String] = params.flatMap(_.readOnlyKey).toList
      val paramCount                 = params.length
      val typeName                   = Type.Name(clsName)
      val encVal                     =
        /* Temporarily removing forProductN due to https://github.com/circe/circe/issues/561
        if (paramCount == 1) {
          val (names, fields): (List[Lit], List[Term.Name]) = params
            .map(param => (Lit.String(param.name), Term.Name(param.term.name.value)))
            .toList
            .unzip
          val List(name)  = names
          val List(field) = fields
          Option(
            q"""
              _root_.io.circe.Encoder.forProduct1(${name})((o: ${Type.Name(clsName)}) => o.${field})
            """
          )
        } else if (paramCount >= 2 && paramCount <= 22) {
          val (names, fields): (List[Lit], List[Term.Name]) = params
            .map(param => (Lit.String(param.name), Term.Name(param.term.name.value)))
            .toList
            .unzip
          val tupleFields = fields
            .map({ field =>
              Term.Select(Term.Name("o"), field)
            })
            .toList

          val unapply: Term.Function = Term.Function(
            List(param"o: ${Type.Name(clsName)}"),
            Term.Tuple(tupleFields)
          )
          Option(
            q"""
              _root_.io.circe.Encoder.${Term.Name(s"forProduct${paramCount}")}(..${names})(${unapply})
            """
          )
        } else */ {
          def encodeRequired(param: ProtocolParameter[ScalaLanguage]) =
            q"""(${Lit.String(param.name.value)}, a.${Term.Name(param.term.name.value)}.asJson)"""

          def encodeOptional(param: ProtocolParameter[ScalaLanguage]) = {
            val name = Lit.String(param.name.value)
            q"a.${Term.Name(param.term.name.value)}.fold(ifAbsent = None, ifPresent = value => Some($name -> value.asJson))"
          }

          val allFields: List[Either[Term.Apply, Term.Tuple]] = params.map { param =>
            val name = Lit.String(param.name.value)
            param.propertyRequirement match {
              case PropertyRequirement.Required | PropertyRequirement.RequiredNullable | PropertyRequirement.OptionalLegacy =>
                Right(encodeRequired(param))
              case PropertyRequirement.Optional | PropertyRequirement.OptionalNullable =>
                Left(encodeOptional(param))
              case PropertyRequirement.Configured(PropertyRequirement.Optional, PropertyRequirement.Optional) =>
                Left(encodeOptional(param))
              case PropertyRequirement.Configured(PropertyRequirement.RequiredNullable | PropertyRequirement.OptionalLegacy, _) =>
                Right(encodeRequired(param))
              case PropertyRequirement.Configured(PropertyRequirement.Optional, _) =>
                Left(q"""a.${Term.Name(param.term.name.value)}.map(value => (${Lit.String(param.name.value)}, value.asJson))""")
            }
          }

          val pairs: List[Term.Tuple] = allFields.collect {
            case Right(pair) => pair
          }
          val optional = allFields.collect {
            case Left(field) => field
          }
          val simpleCase = q"_root_.scala.Vector(..${pairs})"
          val arg = optional.foldLeft[Term](simpleCase) { (acc, field) =>
            q"$acc ++ $field"
          }
          Option(
            q"""
                ${circeVersion.encoderObjectCompanion}.instance[${Type.Name(clsName)}](a => _root_.io.circe.JsonObject.fromIterable($arg))
              """
          )
        }
      Target.pure(encVal.map(encVal => q"""
            implicit val ${suffixClsName("encode", clsName)}: ${circeVersion.encoderObject}[${Type.Name(clsName)}] = {
              val readOnlyKeys = _root_.scala.Predef.Set[_root_.scala.Predef.String](..${readOnlyKeys.map(Lit.String(_))})
              $encVal.mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
            }
          """))
    }

    def decodeModel(
        clsName: String,
        dtoPackage: List[String],
        supportPackage: List[String],
        selfParams: List[ProtocolParameter[ScalaLanguage]],
        parents: List[SuperClass[ScalaLanguage]] = Nil
    ) = {
      val discriminators     = parents.flatMap(_.discriminators)
      val discriminatorNames = discriminators.map(_.propertyName).toSet
      val params = (parents.reverse.flatMap(_.params) ++ selfParams).filterNot(
        param => discriminatorNames.contains(param.name.value)
      )
      val needsEmptyToNull: Boolean = params.exists(_.emptyToNull == EmptyIsNull)
      val paramCount                = params.length
      for {
        presence <- ScalaGenerator.ScalaInterp.selectTerm(NonEmptyList.ofInitLast(supportPackage, "Presence"))
        decVal <- if (paramCount == 0) {
          Target.pure(
            Option[Term](
              q"""
                   new _root_.io.circe.Decoder[${Type.Name(clsName)}] {
                      final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[${Type.Name(clsName)}] =
                        _root_.scala.Right(${Term.Name(clsName)}())
                    }
                  """
            )
          )
        } else
          /* Temporarily removing forProductN due to https://github.com/circe/circe/issues/561
          if (paramCount <= 22 && !needsEmptyToNull) {
            val names: List[Lit] = params.map(_.name).map(Lit.String(_)).toList
            Target.pure(
              Option(
                q"""
                  _root_.io.circe.Decoder.${Term.Name(s"forProduct${paramCount}")}(..${names})(${Term
                  .Name(clsName)}.apply _)
                """
              )
            )
          } else */ {
            params.zipWithIndex
              .traverse({
                case (param, idx) =>
                  for {
                    rawTpe <- Target.fromOption(param.term.decltpe, UserError("Missing type"))
                    tpe <- rawTpe match {
                      case tpe: Type => Target.pure(tpe)
                      case x         => Target.raiseUserError(s"Unsure how to map ${x.structure}, please report this bug!")
                    }
                  } yield {
                    val term = Term.Name(s"v$idx")
                    val name = Lit.String(param.name.value)

                    val emptyToNull: Term => Term = if (param.emptyToNull == EmptyIsNull) { t =>
                      q"$t.withFocus(j => j.asString.fold(j)(s => if(s.isEmpty) _root_.io.circe.Json.Null else j))"
                    } else identity _

                    val decodeField: Type => NonEmptyVector[Term => Term] = { tpe =>
                      NonEmptyVector.of[Term => Term](
                        t => q"$t.downField($name)",
                        emptyToNull,
                        t => q"$t.as[${tpe}]"
                      )
                    }

                    val decodeOptionalField: Type => (Term => Term, Term) => NonEmptyVector[Term => Term] = {
                      tpe => (present, absent) =>
                        NonEmptyVector.of[Term => Term](
                          t => q"""
                          ((c: _root_.io.circe.HCursor) =>
                            c
                              .value
                              .asObject
                              .filter(!_.contains($name))
                              .fold(${emptyToNull(q"c.downField($name)")}.as[${tpe}].map(x => ${present(q"x")})) { _ =>
                                _root_.scala.Right($absent)
                              }
                          )($t)
                        """
                        )
                    }

                    def decodeOptionalRequirement(
                        param: ProtocolParameter[ScalaLanguage]
                    ): PropertyRequirement.OptionalRequirement => NonEmptyVector[Term => Term] = {
                      case PropertyRequirement.OptionalLegacy =>
                        decodeField(tpe)
                      case PropertyRequirement.RequiredNullable =>
                        decodeField(t"_root_.io.circe.Json") :+ (
                                t => q"$t.flatMap(_.as[${tpe}])"
                            )
                      case PropertyRequirement.Optional => // matched only where there is inconsistency between encoder and decoder
                        decodeOptionalField(param.baseType)(x => q"Option($x)", q"None")
                    }

                    val parseTermAccessors: NonEmptyVector[Term => Term] = param.propertyRequirement match {
                      case PropertyRequirement.Required =>
                        decodeField(tpe)
                      case PropertyRequirement.OptionalNullable =>
                        decodeOptionalField(t"Option[${param.baseType}]")(x => q"$presence.present($x)", q"$presence.absent")
                      case PropertyRequirement.Optional | PropertyRequirement.Configured(PropertyRequirement.Optional, PropertyRequirement.Optional) =>
                        decodeOptionalField(param.baseType)(x => q"$presence.present($x)", q"$presence.absent")
                      case requirement: PropertyRequirement.OptionalRequirement =>
                        decodeOptionalRequirement(param)(requirement)
                      case PropertyRequirement.Configured(_, decoderRequirement) =>
                        decodeOptionalRequirement(param)(decoderRequirement)
                    }

                    val parseTerm = parseTermAccessors.foldLeft[Term](q"c")((acc, next) => next(acc))
                    val enum      = enumerator"""${Pat.Var(term)} <- $parseTerm"""
                    (term, enum)
                  }
              })
              .map({ pairs =>
                val (terms, enumerators) = pairs.unzip
                Option(
                  q"""
                    new _root_.io.circe.Decoder[${Type.Name(clsName)}] {
                      final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[${Type.Name(clsName)}] =
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
              implicit val ${suffixClsName("decode", clsName)}: _root_.io.circe.Decoder[${Type.Name(clsName)}] = $decVal
            """)
      }
    }

    def renderDTOStaticDefns(
        clsName: String,
        deps: List[scala.meta.Term.Name],
        encoder: Option[scala.meta.Defn.Val],
        decoder: Option[scala.meta.Defn.Val]
    ) = {
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

  def ArrayProtocolTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): ArrayProtocolTerms[ScalaLanguage, Target] = new ArrayProtocolTermInterp
  class ArrayProtocolTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]) extends ArrayProtocolTerms[ScalaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances
    def extractArrayType(arr: core.ResolvedType[ScalaLanguage], concreteTypes: List[PropMeta[ScalaLanguage]]) =
      for {
        result <- arr match {
          case core.Resolved(tpe, dep, default, _, _) => Target.pure(tpe)
          case core.Deferred(tpeName) =>
            Target.fromOption(lookupTypeName(tpeName, concreteTypes)(identity), UserError(s"Unresolved reference ${tpeName}"))
          case core.DeferredArray(tpeName, containerTpe) =>
            Target.fromOption(
              lookupTypeName(tpeName, concreteTypes)(tpe => t"${containerTpe.getOrElse(t"_root_.scala.Vector")}[${tpe}]"),
              UserError(s"Unresolved reference ${tpeName}")
            )
          case core.DeferredMap(tpeName, customTpe) =>
            Target.fromOption(
              lookupTypeName(tpeName, concreteTypes)(
                tpe => t"_root_.scala.Vector[${customTpe.getOrElse(t"_root_.scala.Predef.Map")}[_root_.scala.Predef.String, ${tpe}]]"
              ),
              UserError(s"Unresolved reference ${tpeName}")
            )
        }
      } yield result
  }

  def ProtocolSupportTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): ProtocolSupportTerms[ScalaLanguage, Target] =
    new ProtocolSupportTermInterp
  class ProtocolSupportTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]) extends ProtocolSupportTerms[ScalaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances
    def extractConcreteTypes(definitions: Either[String, List[PropMeta[ScalaLanguage]]]) =
      definitions.fold[Target[List[PropMeta[ScalaLanguage]]]](Target.raiseUserError _, Target.pure _)

    def protocolImports() =
      Target.pure(
        List(
          q"import cats.syntax.either._",
          q"import io.circe._",
          q"import io.circe.syntax._",
          q"import cats.implicits._"
        )
      )

    def staticProtocolImports(pkgName: List[String]): Target[List[Import]] = {
      val implicitsRef: Term.Ref = (pkgName.map(Term.Name.apply _) ++ List(q"Implicits")).foldLeft[Term.Ref](q"_root_")(Term.Select.apply _)
      Target.pure(
        List(
          q"import cats.implicits._",
          q"import cats.data.EitherT"
        ) :+ q"import $implicitsRef._"
      )
    }

    def packageObjectImports() =
      Target.pure(List.empty)

    def generateSupportDefinitions() = {
      val presenceTrait =
        q"""sealed trait Presence[+T] extends _root_.scala.Product with _root_.scala.Serializable {
                def fold[R](ifAbsent: => R,
                            ifPresent: T => R): R
                def map[R](f: T => R): Presence[R] = fold(Presence.absent, a => Presence.present(f(a)))

                def toOption: Option[T] = fold[Option[T]](None, Some(_))
              }
             """
      val presenceObject =
        q"""
              object Presence {
                def absent[R]: Presence[R] = Absent
                def present[R](value: R): Presence[R] = Present(value)
                case object Absent extends Presence[Nothing] {
                  def fold[R](ifAbsent: => R,
                           ifValue: Nothing => R): R = ifAbsent
                }
                final case class Present[+T](value: T) extends Presence[T] {
                  def fold[R](ifAbsent: => R,
                           ifPresent: T => R): R = ifPresent(value)
                }

                def fromOption[T](value: Option[T]): Presence[T] =
                  value.fold[Presence[T]](Absent)(Present(_))

                implicit object PresenceFunctor extends cats.Functor[Presence] {
                  def map[A, B](fa: Presence[A])(f: A => B): Presence[B] = fa.fold[Presence[B]](Presence.absent, a => Presence.present(f(a)))
                }
              }
             """
      val presenceDefinition = SupportDefinition[ScalaLanguage](q"Presence", Nil, List(presenceTrait, presenceObject), insideDefinitions = false)
      Target.pure(List(presenceDefinition))
    }

    def packageObjectContents() =
      Target.pure(
        List(
          q"implicit val guardrailDecodeInstant: _root_.io.circe.Decoder[java.time.Instant] = _root_.io.circe.Decoder[java.time.Instant].or(_root_.io.circe.Decoder[_root_.scala.Long].map(java.time.Instant.ofEpochMilli))",
          q"implicit val guardrailDecodeLocalDate: _root_.io.circe.Decoder[java.time.LocalDate] = _root_.io.circe.Decoder[java.time.LocalDate].or(_root_.io.circe.Decoder[java.time.Instant].map(_.atZone(java.time.ZoneOffset.UTC).toLocalDate))",
          q"implicit val guardrailDecodeLocalDateTime: _root_.io.circe.Decoder[java.time.LocalDateTime] = _root_.io.circe.Decoder[java.time.LocalDateTime]",
          q"implicit val guardrailDecodeLocalTime: _root_.io.circe.Decoder[java.time.LocalTime] = _root_.io.circe.Decoder[java.time.LocalTime]",
          q"implicit val guardrailDecodeOffsetDateTime: _root_.io.circe.Decoder[java.time.OffsetDateTime] = _root_.io.circe.Decoder[java.time.OffsetDateTime].or(_root_.io.circe.Decoder[java.time.Instant].map(_.atZone(java.time.ZoneOffset.UTC).toOffsetDateTime))",
          q"implicit val guardrailDecodeZonedDateTime: _root_.io.circe.Decoder[java.time.ZonedDateTime] = _root_.io.circe.Decoder[java.time.ZonedDateTime]",
          q"implicit val guardrailDecodeBase64String: _root_.io.circe.Decoder[Base64String] = _root_.io.circe.Decoder[_root_.scala.Predef.String].emapTry(v => scala.util.Try(java.util.Base64.getDecoder.decode(v))).map(new Base64String(_))",
          q"implicit val guardrailEncodeInstant: _root_.io.circe.Encoder[java.time.Instant] = _root_.io.circe.Encoder[java.time.Instant]",
          q"implicit val guardrailEncodeLocalDate: _root_.io.circe.Encoder[java.time.LocalDate] = _root_.io.circe.Encoder[java.time.LocalDate]",
          q"implicit val guardrailEncodeLocalDateTime: _root_.io.circe.Encoder[java.time.LocalDateTime] = _root_.io.circe.Encoder[java.time.LocalDateTime]",
          q"implicit val guardrailEncodeLocalTime: _root_.io.circe.Encoder[java.time.LocalTime] = _root_.io.circe.Encoder[java.time.LocalTime]",
          q"implicit val guardrailEncodeOffsetDateTime: _root_.io.circe.Encoder[java.time.OffsetDateTime] = _root_.io.circe.Encoder[java.time.OffsetDateTime]",
          q"implicit val guardrailEncodeZonedDateTime: _root_.io.circe.Encoder[java.time.ZonedDateTime] = _root_.io.circe.Encoder[java.time.ZonedDateTime]",
          q"implicit val guardrailEncodeBase64String: _root_.io.circe.Encoder[Base64String] = _root_.io.circe.Encoder[_root_.scala.Predef.String].contramap[Base64String](v => new _root_.scala.Predef.String(java.util.Base64.getEncoder.encode(v.data)))"
        )
      )

    def implicitsObject() = Target.pure(None)
  }

  def PolyProtocolTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): PolyProtocolTerms[ScalaLanguage, Target] =
    new PolyProtocolTermInterp
  class PolyProtocolTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]) extends PolyProtocolTerms[ScalaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances
    def extractSuperClass(
        swagger: Tracker[ComposedSchema],
        definitions: List[(String, Tracker[Schema[_]])]
    ) = {
      def allParents: Tracker[Schema[_]] => Target[List[(String, Tracker[Schema[_]], List[Tracker[Schema[_]]])]] =
        _.refine[Target[List[(String, Tracker[Schema[_]], List[Tracker[Schema[_]]])]]]({ case x: ComposedSchema => x })(
          _.downField("allOf", _.getAllOf()).indexedDistribute.filter(_.downField("$ref", _.get$ref()).unwrapTracker.nonEmpty) match {
            case head :: tail =>
              definitions
                .collectFirst({
                  case (clsName, e) if head.downField("$ref", _.get$ref()).exists(_.unwrapTracker.endsWith(s"/$clsName")) =>
                    val thisParent = (clsName, e, tail)
                    allParents(e).map(otherParents => thisParent :: otherParents)
                })
                .getOrElse(
                  Target.raiseUserError(s"Reference ${head.downField("$ref", _.get$ref()).unwrapTracker} not found among definitions (${head.showHistory})")
                )
            case _ => Target.pure(List.empty)
          }
        ).getOrElse(Target.pure(List.empty))
      allParents(swagger)
    }

    def renderADTStaticDefns(
        clsName: String,
        discriminator: Discriminator[ScalaLanguage],
        encoder: Option[scala.meta.Defn.Val],
        decoder: Option[scala.meta.Defn.Val]
    ) =
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

    def decodeADT(clsName: String, discriminator: Discriminator[ScalaLanguage], children: List[String] = Nil) = {
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
        q"""implicit val decoder: _root_.io.circe.Decoder[${Type.Name(clsName)}] = _root_.io.circe.Decoder.instance({ c =>
                 val discriminatorCursor = c.downField(discriminator)
                 discriminatorCursor.as[String].flatMap {
                   ..case $childrenCases;
                   case tpe =>
                     _root_.scala.Left(DecodingFailure("Unknown value " ++ tpe ++ ${Lit
          .String(s" (valid: ${childrenDiscriminators.mkString(", ")})")}, discriminatorCursor.history))
                 }
            })"""
      Target.pure(Some(code))
    }

    def encodeADT(clsName: String, discriminator: Discriminator[ScalaLanguage], children: List[String] = Nil) = {
      val childrenCases = children.map({ child =>
        val discriminatorValue = discriminator.mapping
          .collectFirst({ case (value, elem) if elem.name == child => value })
          .getOrElse(child)
        p"case e:${Type.Name(child)} => e.asJsonObject.add(discriminator, ${Lit.String(discriminatorValue)}.asJson).asJson"
      })
      val code =
        q"""implicit val encoder: _root_.io.circe.Encoder[${Type.Name(clsName)}] = _root_.io.circe.Encoder.instance {
              ..case $childrenCases
          }"""
      Target.pure(Some(code))
    }

    def renderSealedTrait(
        className: String,
        params: List[ProtocolParameter[ScalaLanguage]],
        discriminator: Discriminator[ScalaLanguage],
        parents: List[SuperClass[ScalaLanguage]] = Nil,
        children: List[String] = Nil
    ) =
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
                  UserError(t.decltpe.fold("Nothing to map")(x => s"Unsure how to map ${x.structure}, please report this bug!"))
                )
              } yield q"""def ${Term.Name(t.name.value)}: ${tpe}"""
            }
        )
      } yield {
        parents.headOption
          .fold(q"""trait ${Type.Name(className)} {..${testTerms}}""")(
            parent => q"""trait ${Type.Name(className)} extends ${init"${Type.Name(parent.clsName)}(...$Nil)"} { ..${testTerms} } """
          )
      }
  }
}
