package com.twilio.guardrail
package generators

import _root_.io.swagger.models.{ ArrayModel, ComposedModel, Model, ModelImpl, RefModel }
import _root_.io.swagger.models.properties._
import cats.implicits._
import cats.~>
import com.twilio.guardrail.extract.{ Default, ScalaEmptyIsNull, ScalaType }
import com.twilio.guardrail.terms
import java.util.Locale

import com.twilio.guardrail.protocol.terms.protocol._

import scala.collection.JavaConverters._
import scala.meta._

object CirceProtocolGenerator {
  import ProtocolGenerator._

  def suffixClsName(prefix: String, clsName: String) = Pat.Var(Term.Name(s"${prefix}${clsName}"))

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
        val tpeName = Option(swagger.getType()).getOrElse("string")
        Target.getGeneratorSettings.map { implicit gs =>
          Either.right(SwaggerUtil.typeName(tpeName, Option(swagger.getFormat()), ScalaType(swagger)))
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

  object ModelProtocolTermInterp extends (ModelProtocolTerm ~> Target) {
    def apply[T](term: ModelProtocolTerm[T]): Target[T] = term match {
      case ExtractProperties(swagger) =>
        Target.pure(
          (swagger match {
            case m: ModelImpl        => Option(m.getProperties)
            case comp: ComposedModel => comp.getAllOf().asScala.toList.get(1).flatMap(prop => Option(prop.getProperties))
            case _                   => None
          }).map(_.asScala.toList).toList.flatten
        )

      case TransformProperty(clsName, name, property, needCamelSnakeConversion, concreteTypes) =>
        def toCamelCase(s: String): String =
          "[_\\.]([a-z])".r.replaceAllIn(s, m => m.group(1).toUpperCase(Locale.US))

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

      case RenderDTOClass(clsName, selfTerms, parents) =>
        val discriminator = parents.find(_.discriminator.isDefined).flatMap(_.discriminator) //collectFirst { case SuperClass(_, _, _, Some(discriminator)) => discriminator }
        val parentNameOpt = parents.headOption.map(_.clsName)
        val terms = (parents.reverse.flatMap(_.params.map(_.term)) ++ selfTerms).filterNot(
          param => discriminator.contains(param.name.value)
        )
        val code = parentNameOpt
          .fold(q"""case class ${Type.Name(clsName)}(..${terms})""")(
            parentName => q"""case class ${Type.Name(clsName)}(..${terms}) extends ${template"${init"${Type.Name(parentName)}(...$Nil)"}"}"""
          )

        Target.pure(code)

      case EncodeModel(clsName, needCamelSnakeConversion, selfParams, parents) =>
        val discriminator = parents.find(_.discriminator.isDefined).flatMap(_.discriminator)
        val params = (parents.reverse.flatMap(_.params) ++ selfParams).filterNot(
          param => discriminator.contains(param.name)
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
        val discriminator = parents.find(_.discriminator.isDefined).flatMap(_.discriminator)
        val params = (parents.reverse.flatMap(_.params) ++ selfParams).filterNot(
          param => discriminator.contains(param.name)
        )
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
        Target.pure(q"""object ${Term.Name(clsName)} {
            ..${extraImports :+
          encoder :+
          decoder}
          }
          """)
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
          case SwaggerUtil.Resolved(tpe, dep, default) => Target.pure(tpe)
          case SwaggerUtil.Deferred(tpeName) =>
            Target.fromOption(lookupTypeName(tpeName, concreteTypes)(identity), s"Unresolved reference ${tpeName}")
          case SwaggerUtil.DeferredArray(tpeName) =>
            Target.fromOption(lookupTypeName(tpeName, concreteTypes)(tpe => t"IndexedSeq[${tpe}]"), s"Unresolved reference ${tpeName}")
          case SwaggerUtil.DeferredMap(tpeName) =>
            Target.fromOption(lookupTypeName(tpeName, concreteTypes)(tpe => t"IndexedSeq[Map[String, ${tpe}]]"), s"Unresolved reference ${tpeName}")
        }
    }
  }

  object ProtocolSupportTermInterp extends (ProtocolSupportTerm ~> Target) {
    def apply[T](term: ProtocolSupportTerm[T]): Target[T] = term match {
      case ExtractConcreteTypes(definitions) =>
        Target.getGeneratorSettings.flatMap { implicit gs =>
          for {
            entries <- definitions.traverse {
              case (clsName, impl: ModelImpl) if (Option(impl.getProperties()).isDefined || Option(impl.getEnum()).isDefined) =>
                Target.pure((clsName, SwaggerUtil.Resolved(Type.Name(clsName), None, None): SwaggerUtil.ResolvedType))
              case (clsName, comp: ComposedModel) =>
                val parentSimpleRef: Option[String]        = comp.getInterfaces.asScala.headOption.map(_.getSimpleRef)
                val parentTerm                             = parentSimpleRef.map(n => Term.Name(n))
                val resolvedType: SwaggerUtil.ResolvedType = SwaggerUtil.Resolved(Type.Name(clsName), parentTerm, None)
                Target.pure((clsName, resolvedType))
              case (clsName, definition) =>
                SwaggerUtil
                  .modelMetaType(definition)
                  .map(x => (clsName, x))
            }
            result <- SwaggerUtil.ResolvedType.resolve_(entries)
          } yield
            result.map {
              case (clsName, SwaggerUtil.Resolved(tpe, _, _)) =>
                PropMeta(clsName, tpe)
            }
        }

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
            q"import io.circe.java8.{ time => j8time }"
          )
        )

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

  object PolyProtocolTermInterp extends (PolyProtocolTerm ~> Target) {
    override def apply[A](fa: PolyProtocolTerm[A]): Target[A] = fa match {
      case RenderADTCompanion(clsName, discriminator, encoder, decoder) =>
        val code = q"""object ${Term.Name(clsName)} {
             val discriminator:String = ${Lit.String(discriminator)}
            ..${List(encoder, decoder)}
          }
          """
        Target.pure(code)

      case DecodeADT(clsName, children) =>
        val childrenCases = children.map(
          child => p"case ${Lit.String(child)} => c.as[${Type.Name(child)}]"
        )
        val code =
          q"""implicit val decoder: Decoder[${Type.Name(clsName)}] = Decoder.instance(c =>
                 c.downField(discriminator).as[String].flatMap {
                   ..case $childrenCases
                 }
            )"""
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
        val parentNameOpt = parents.headOption.map(_.clsName)
        val testTerms = terms
          .filter(_.name.value != discriminator)
          .map { t =>
            val tpe: Type = t.decltpe
              .flatMap({
                case tpe: Type => Some(tpe)
                case x =>
                  println(s"Unsure how to map ${x.structure}, please report this bug!")
                  None
              })
              .get

            q"""def ${Term.Name(t.name.value)} : ${tpe}"""
          }

        Target.pure(
          parentNameOpt.fold(q"""trait ${Type.Name(className)} {..${testTerms}}""")(
            parentName => q"""trait ${Type.Name(className)} extends ${template"${init"${Type.Name(parentName)}(...$Nil)"}{..${testTerms}}"} """
          )
        )

    }
  }
}
