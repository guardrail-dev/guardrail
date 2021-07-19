package dev.guardrail.generators.Scala

import cats.data.NonEmptyList
import cats.syntax.all._
import dev.guardrail.{ Discriminator, ProtocolParameter, RuntimeFailure, Target }
import dev.guardrail.generators.Scala.model.CirceModelGenerator
import dev.guardrail.generators.ScalaGenerator
import dev.guardrail.generators.helpers.JacksonHelpers
import dev.guardrail.languages.ScalaLanguage
import dev.guardrail.protocol.terms.protocol.PropertyRequirement.{ Optional, RequiredNullable }
import dev.guardrail.protocol.terms.protocol._
import dev.guardrail.terms.CollectionsLibTerms
import scala.meta._

object JacksonProtocolGenerator {
  def EnumProtocolTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): EnumProtocolTerms[ScalaLanguage, Target] = {
    val baseInterp = new CirceProtocolGenerator.EnumProtocolTermInterp
    baseInterp.copy(
      newRenderClass = (className, tpe, elems) =>
        for {
          renderedClass <- baseInterp.renderClass(className, tpe, elems)
        } yield renderedClass.copy(
          mods = List(
              mod"@com.fasterxml.jackson.databind.annotation.JsonSerialize(using=classOf[${Type.Select(Term.Name(className), Type.Name(className + "Serializer"))}])",
              mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using=classOf[${Type.Select(Term.Name(className), Type.Name(className + "Deserializer"))}])"
            ) ++ renderedClass.mods
        ),
      newEncodeEnum = { (className, tpe) =>
        val writeMethod = tpe match {
          case t"Int"    => q"writeNumber"
          case t"Long"   => q"writeNumber"
          case t"String" => q"writeString"
        }
        Target.pure(
          Some(
            q"""
         class ${Type.Name(className + "Serializer")} extends com.fasterxml.jackson.databind.JsonSerializer[${Type.Name(className)}] {
           override def serialize(value: ${Type
              .Name(className)}, gen: com.fasterxml.jackson.core.JsonGenerator, serializers: com.fasterxml.jackson.databind.SerializerProvider): Unit = gen.${writeMethod}(value.value)
         }
       """
          )
        )
      },
      newDecodeEnum = { (className, tpe) =>
        val getter = tpe match {
          case t"String" => q"getText"
          case t"Int"    => q"getIntValue"
          case t"Long"   => q"getLongValue"
        }
        Target.pure(
          Some(
            q"""
         class ${Type.Name(className + "Deserializer")} extends com.fasterxml.jackson.databind.JsonDeserializer[${Type.Name(className)}] {
           override def deserialize(p: com.fasterxml.jackson.core.JsonParser, ctxt: com.fasterxml.jackson.databind.DeserializationContext): ${Type.Name(
              className
            )} =
            ${Term.Name(className)}.from(p.${getter})
              .getOrElse({ throw new com.fasterxml.jackson.databind.JsonMappingException(p, s"Invalid value '$${p.${getter}}' for " + ${Lit
              .String(className)}) })
         }
       """
          )
        )
      },
      newRenderStaticDefns = (className, elems, members, accessors, encoder, decoder) =>
        for {
          renderedStaticDefns <- baseInterp.renderStaticDefns(className, elems, members, accessors, encoder, decoder)
          classType = Type.Name(className)
        } yield renderedStaticDefns.copy(
          definitions = renderedStaticDefns.definitions ++ List(
                  q"implicit val ${Pat.Var(Term.Name(s"encode${className}"))}: GuardrailEncoder[$classType] = GuardrailEncoder.instance",
                  q"implicit val ${Pat.Var(Term.Name(s"decode${className}"))}: GuardrailDecoder[$classType] = GuardrailDecoder.instance(new com.fasterxml.jackson.core.`type`.TypeReference[$classType] {})",
                  q"implicit val ${Pat.Var(Term.Name(s"validate${className}"))}: GuardrailValidator[$classType] = GuardrailValidator.noop"
                )
        )
    )
  }

  def ModelProtocolTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): ModelProtocolTerms[ScalaLanguage, Target] = {
    def paramAnnotations(
        param: ProtocolParameter[ScalaLanguage],
        presenceSerType: Type,
        presenceDeserType: Type,
        optionNonNullDeserType: Type,
        optionNonMissingDeserType: Type
    ): List[Mod] = {
      val jsonProperty = List(mod"@com.fasterxml.jackson.annotation.JsonProperty(${Lit.String(param.name.value)})")
      val containerDeserializers = param.term.decltpe match {
        case Some(t"Vector[$inner]")        => List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(contentAs = classOf[$inner])")
        case Some(t"List[$inner]")          => List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(contentAs = classOf[$inner])")
        case Some(t"IndexedSeq[$inner]")    => List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(contentAs = classOf[$inner])")
        case Some(t"Seq[$inner]")           => List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(contentAs = classOf[$inner])")
        case Some(t"Iterable[$inner]")      => List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(contentAs = classOf[$inner])")
        case Some(t"Map[$keyType, $inner]") => List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(contentAs = classOf[$inner])")
        case _                              => List.empty
      }
      val presenceHandling = param.term.decltpe match {
        case Some(t"Presence[_]") | Some(Type.Apply(Type.Select(_, Type.Name("Presence")), _)) =>
          List(
            mod"@com.fasterxml.jackson.annotation.JsonInclude(value = com.fasterxml.jackson.annotation.JsonInclude.Include.NON_EMPTY)",
            mod"@com.fasterxml.jackson.databind.annotation.JsonSerialize(using = classOf[$presenceSerType])",
            mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = classOf[$presenceDeserType])"
          )
        case _ => List.empty
      }
      val optionHandling = param.term.decltpe match {
        case Some(t"Option[$inner]") =>
          val encodeRequirement = param.propertyRequirement match {
            case PropertyRequirement.Configured(encoder, _) => encoder
            case other                                      => other
          }
          val decodeRequirement = param.propertyRequirement match {
            case PropertyRequirement.Configured(_, decoder) => decoder
            case other                                      => other
          }

          val serializers = encodeRequirement match {
            case Optional =>
              // When serializing, either include the non-null value or drop the property entirely
              List(mod"@com.fasterxml.jackson.annotation.JsonInclude(value = com.fasterxml.jackson.annotation.JsonInclude.Include.NON_EMPTY)")
            case _ =>
              // RequiredNullable: Always include the property, null if appropriate
              // OptionalLegacy: Legacy behavior is to serialize non-required, unset-nullability as null if not present
              List(mod"@com.fasterxml.jackson.annotation.JsonInclude(value = com.fasterxml.jackson.annotation.JsonInclude.Include.ALWAYS)")
          }

          val deserializers = decodeRequirement match {
            case Optional =>
              // When deserializing either a non-null value must be present, or no value at all
              List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = classOf[$optionNonNullDeserType], contentAs = classOf[$inner])")
            case RequiredNullable =>
              // When deserializing, the property must be present, but can be null
              List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = classOf[$optionNonMissingDeserType], contentAs = classOf[$inner])")
            case _ =>
              // OptionalLegacy: When deserializing the property can be present (null or non-null) or not present
              List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(contentAs = classOf[$inner])")
          }

          serializers ++ deserializers

        case _ => List.empty
      }
      val notNull = List(mod"@constraints.NotNull")
      jsonProperty ++ containerDeserializers ++ presenceHandling ++ optionHandling ++ notNull
    }
    def fixDefaultValue(param: ProtocolParameter[ScalaLanguage]): Option[Term] =
      param.propertyRequirement match {
        case Optional | RequiredNullable | PropertyRequirement.Configured(_, Optional | RequiredNullable) =>
          // We can't have a default value with Jackson, because it will allow us to deserialize
          // things that are not valid (nulls in the Optional case, missing property in the
          // RequiredNullable case). This is unfortunate because it makes the API of the case class
          // less ergonomic, but perhaps we can add an apply() method to the companion object to fix that up.
          None
        case _ => param.term.default
      }

    val jsonIgnoreProperties = mod"""@com.fasterxml.jackson.annotation.JsonIgnoreProperties(ignoreUnknown = true)"""
    val baseInterp           = new CirceProtocolGenerator.ModelProtocolTermInterp(CirceModelGenerator.V012)
    import baseInterp.MonadF
    baseInterp.copy(
      newRenderDTOClass = (className, supportPackage, terms, parents) =>
        for {
          renderedClass <- baseInterp.renderDTOClass(className, supportPackage, terms, parents)
          discriminatorParams = parents.flatMap(
            parent => parent.discriminators.flatMap(discrim => parent.params.find(_.name.value == discrim.propertyName).map((discrim, _)))
          )
          discriminators <- discriminatorParams.traverse({
            case (discriminator, param) =>
              for {
                discrimTpe <- Target.fromOption(param.term.decltpe, RuntimeFailure(s"Property ${param.name.value} has no type"))
                discrimValue <- JacksonHelpers
                  .discriminatorExpression[ScalaLanguage](
                    param.name.value,
                    discriminatorValue(discriminator, className),
                    param.rawType.tpe,
                    param.rawType.format
                  )(
                    v => Target.pure[Term](q"""BigInt(${Lit.String(v)})"""),
                    v => Target.pure[Term](q"""BigDecimal(${Lit.String(v)})"""),
                    v =>
                      param.term.decltpe.fold(
                        Target.raiseUserError[Term](s"No declared type for property '${param.name.value}' on class $className")
                      )({
                        case tpe @ (_: Type.Name | _: Type.Select) =>
                          ScalaGenerator.ScalaInterp.formatEnumName(v).map(ev => Term.Select(Term.Name(tpe.toString), Term.Name(ev)))
                        case tpe => Target.raiseError(RuntimeFailure(s"Assumed property ${param.name.value} was an enum, but can't handle $tpe"))
                      })
                  )(ScalaGenerator.ScalaInterp)
              } yield (param.name.value, param.term.name.value, param.term.decltpe, discrimValue)
          })
          presenceSerType        <- ScalaGenerator.ScalaInterp.selectType(NonEmptyList.ofInitLast(supportPackage :+ "Presence", "PresenceSerializer"))
          presenceDeserType      <- ScalaGenerator.ScalaInterp.selectType(NonEmptyList.ofInitLast(supportPackage :+ "Presence", "PresenceDeserializer"))
          optionNonNullDeserType <- ScalaGenerator.ScalaInterp.selectType(NonEmptyList.ofInitLast(supportPackage :+ "Presence", "OptionNonNullDeserializer"))
          optionNonMissingDeserType <- ScalaGenerator.ScalaInterp.selectType(
            NonEmptyList.ofInitLast(supportPackage :+ "Presence", "OptionNonMissingDeserializer")
          )
          allTerms = terms ++ parents.flatMap(_.params)
        } yield renderedClass.copy(
          mods = jsonIgnoreProperties +: renderedClass.mods,
          ctor = renderedClass.ctor.copy(
            paramss = renderedClass.ctor.paramss.map(
              _.map(
                param =>
                  allTerms
                    .find(_.term.name.value == param.name.value)
                    .fold(param)({ term =>
                      param.copy(
                        mods = paramAnnotations(term, presenceSerType, presenceDeserType, optionNonNullDeserType, optionNonMissingDeserType) ++ param.mods,
                        default = fixDefaultValue(term)
                      )
                    })
              )
            )
          ),
          templ = renderedClass.templ.copy(
            stats =
              discriminators.map({
                case (propertyName, fieldName, tpe, value) =>
                  q"""
                  @com.fasterxml.jackson.annotation.JsonProperty(${Lit.String(propertyName)})
                  val ${Pat.Var(Term.Name(fieldName))}: $tpe = $value
                """
              }) ++ renderedClass.templ.stats
          )
        ),
      newEncodeModel = (_, _, _, _) => Target.pure(None),
      newDecodeModel = (_, _, _, _, _) => Target.pure(None),
      newRenderDTOStaticDefns = (className, deps, encoder, decoder) =>
        for {
          renderedDTOStaticDefns <- baseInterp.renderDTOStaticDefns(className, deps, encoder, decoder)
          classType = Type.Name(className)
        } yield renderedDTOStaticDefns.copy(
          definitions = renderedDTOStaticDefns.definitions ++ List(
                  q"implicit val ${Pat.Var(Term.Name(s"encode${className}"))}: GuardrailEncoder[$classType] = GuardrailEncoder.instance",
                  q"implicit val ${Pat.Var(Term.Name(s"decode${className}"))}: GuardrailDecoder[$classType] = GuardrailDecoder.instance(new com.fasterxml.jackson.core.`type`.TypeReference[$classType] {})",
                  q"implicit val ${Pat.Var(Term.Name(s"validate${className}"))}: GuardrailValidator[$classType] = GuardrailValidator.instance"
                )
        )
    )
  }

  def ArrayProtocolTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): ArrayProtocolTerms[ScalaLanguage, Target] =
    new CirceProtocolGenerator.ArrayProtocolTermInterp

  def ProtocolSupportTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): ProtocolSupportTerms[ScalaLanguage, Target] = {
    val baseInterp = new CirceProtocolGenerator.ProtocolSupportTermInterp
    baseInterp.copy(
      newProtocolImports = () =>
        Target.pure(
          List(
            q"import cats.implicits._"
          )
        ),
      newPackageObjectImports = () => Target.pure(List.empty),
      newPackageObjectContents = () => Target.pure(List.empty),
      newImplicitsObject = () =>
        Target.pure(
          Some(
            (
              q"JacksonImplicits",
              q"""
            object JacksonImplicits {
              object constraints {
                type NotNull = javax.validation.constraints.NotNull @scala.annotation.meta.field @scala.annotation.meta.param
              }

              trait GuardrailEncoder[A] {
                def encode(a: A)(implicit mapper: com.fasterxml.jackson.databind.ObjectMapper): com.fasterxml.jackson.databind.JsonNode =
                  mapper.valueToTree(a)
              }
              object GuardrailEncoder {
                def instance[A]: GuardrailEncoder[A] = new GuardrailEncoder[A] {}

                implicit def guardrailEncodeOption[B: GuardrailEncoder]: GuardrailEncoder[Option[B]] = instance
                implicit def guardrailEncodeVector[B: GuardrailEncoder]: GuardrailEncoder[Vector[B]] = instance
                implicit def guardrailEncodeMap[B: GuardrailEncoder]: GuardrailEncoder[Map[String, B]] = instance
                implicit val guardrailEncodeBoolean: GuardrailEncoder[Boolean] = instance
                implicit val guardrailEncodeInt: GuardrailEncoder[Int] = instance
                implicit val guardrailEncodeLong: GuardrailEncoder[Long] = instance
                implicit val guardrailEncodeBigInt: GuardrailEncoder[BigInt] = instance
                implicit val guardrailEncodeFloat: GuardrailEncoder[Float] = instance
                implicit val guardrailEncodeDouble: GuardrailEncoder[Double] = instance
                implicit val guardrailEncodeBigDecimal: GuardrailEncoder[BigDecimal] = instance
                implicit val guardrailEncodeString: GuardrailEncoder[String] = instance
                implicit val guardrailEncodeBase64String: GuardrailEncoder[Base64String] = instance
                implicit val guardrailEncodeInstant: GuardrailEncoder[java.time.Instant] = instance
                implicit val guardrailEncodeLocalDate: GuardrailEncoder[java.time.LocalDate] = instance
                implicit val guardrailEncodeLocalDateTime: GuardrailEncoder[java.time.LocalDateTime] = instance
                implicit val guardrailEncodeLocalTime: GuardrailEncoder[java.time.LocalTime] = instance
                implicit val guardrailEncodeOffsetDateTime: GuardrailEncoder[java.time.OffsetDateTime] = instance
                implicit val guardrailEncodeZonedDateTime: GuardrailEncoder[java.time.ZonedDateTime] = instance
              }

              trait GuardrailDecoder[A] {
                def tpe: Either[com.fasterxml.jackson.core.`type`.TypeReference[A], Class[A]]
                def decode(jsonNode: com.fasterxml.jackson.databind.JsonNode)(implicit mapper: com.fasterxml.jackson.databind.ObjectMapper, validator: javax.validation.Validator, guardrailValidator: GuardrailValidator[A]): scala.util.Try[A] =
                  scala.util.Try(this.tpe.fold(mapper.convertValue(jsonNode, _), mapper.convertValue(jsonNode, _))).flatMap(guardrailValidator.validate)
              }
              object GuardrailDecoder {
                def instance[A](typeRef: com.fasterxml.jackson.core.`type`.TypeReference[A]): GuardrailDecoder[A] = new GuardrailDecoder[A] {
                  override val tpe: Either[com.fasterxml.jackson.core.`type`.TypeReference[A], Class[A]] = Left(typeRef)
                }
                def instance[A](cls: Class[A]): GuardrailDecoder[A] = new GuardrailDecoder[A] {
                  override val tpe: Either[com.fasterxml.jackson.core.`type`.TypeReference[A], Class[A]] = Right(cls)
                }

                implicit def guardrailDecodeOption[B: GuardrailDecoder: GuardrailValidator]: GuardrailDecoder[Option[B]] = new GuardrailDecoder[Option[B]] {
                  override val tpe: Either[com.fasterxml.jackson.core.`type`.TypeReference[Option[B]], Class[Option[B]]] = Left(new com.fasterxml.jackson.core.`type`.TypeReference[Option[B]] {})
                  override def decode(jsonNode: com.fasterxml.jackson.databind.JsonNode)(implicit mapper: com.fasterxml.jackson.databind.ObjectMapper, validator: javax.validation.Validator, guardrailValidator: GuardrailValidator[Option[B]]): scala.util.Try[Option[B]] = {
                    if (jsonNode.isNull) {
                      scala.util.Success(Option.empty[B])
                    } else {
                      implicitly[GuardrailDecoder[B]].decode(jsonNode).map(Option.apply)
                    }
                  }
                }
                implicit def guardrailDecodeVector[B: GuardrailDecoder: GuardrailValidator]: GuardrailDecoder[Vector[B]] = new GuardrailDecoder[Vector[B]] {
                  override val tpe: Either[com.fasterxml.jackson.core.`type`.TypeReference[Vector[B]], Class[Vector[B]]] = Left(new com.fasterxml.jackson.core.`type`.TypeReference[Vector[B]] {})
                  override def decode(jsonNode: com.fasterxml.jackson.databind.JsonNode)(implicit mapper: com.fasterxml.jackson.databind.ObjectMapper, validator: javax.validation.Validator, guardrailValidator: GuardrailValidator[Vector[B]]): scala.util.Try[Vector[B]] = {
                    jsonNode match {
                      case arr: com.fasterxml.jackson.databind.node.ArrayNode =>
                        import cats.implicits._
                        import _root_.scala.collection.JavaConverters._
                        arr.iterator().asScala.toVector.traverse(implicitly[GuardrailDecoder[B]].decode)
                      case _ =>
                        scala.util.Failure(new com.fasterxml.jackson.databind.JsonMappingException(null, s"Can't decode to vector; node of type $${jsonNode.getClass.getSimpleName} is not an array"))
                    }
                  }
                }
                implicit def guardrailDecodeMap[B: GuardrailDecoder: GuardrailValidator]: GuardrailDecoder[Map[String, B]] = new GuardrailDecoder[Map[String, B]] {
                  override val tpe: Either[com.fasterxml.jackson.core.`type`.TypeReference[Map[String, B]], Class[Map[String, B]]] = Left(new com.fasterxml.jackson.core.`type`.TypeReference[Map[String, B]] {})
                  override def decode(jsonNode: com.fasterxml.jackson.databind.JsonNode)(implicit mapper: com.fasterxml.jackson.databind.ObjectMapper, validator: javax.validation.Validator, guardrailValidator: GuardrailValidator[Map[String, B]]): scala.util.Try[Map[String, B]] = {
                    jsonNode match {
                      case obj: com.fasterxml.jackson.databind.node.ObjectNode =>
                        import cats.implicits._
                        import _root_.scala.collection.JavaConverters._
                        obj.fields().asScala.toVector.traverse(entry => implicitly[GuardrailDecoder[B]].decode(entry.getValue).map((entry.getKey, _))).map(_.toMap)
                      case _ =>
                        scala.util.Failure(new com.fasterxml.jackson.databind.JsonMappingException(null, s"Can't decode to map; node of type $${jsonNode.getClass.getSimpleName} is not an object"))
                    }
                  }
                }
                implicit val guardrailDecodeBoolean: GuardrailDecoder[Boolean] = instance(classOf[Boolean])
                implicit val guardrailDecodeInt: GuardrailDecoder[Int] = instance(classOf[Int])
                implicit val guardrailDecodeLong: GuardrailDecoder[Long] = instance(classOf[Long])
                implicit val guardrailDecodeBigInt: GuardrailDecoder[BigInt] = instance(classOf[BigInt])
                implicit val guardrailDecodeFloat: GuardrailDecoder[Float] = instance(classOf[Float])
                implicit val guardrailDecodeDouble: GuardrailDecoder[Double] = instance(classOf[Double])
                implicit val guardrailDecodeBigDecimal: GuardrailDecoder[BigDecimal] = instance(classOf[BigDecimal])
                implicit val guardrailDecodeString: GuardrailDecoder[String] = instance(classOf[String])
                implicit val guardrailDecodeBase64String: GuardrailDecoder[Base64String] = instance(classOf[Base64String])
                implicit val guardrailDecodeInstant: GuardrailDecoder[java.time.Instant] = instance(classOf[java.time.Instant])
                implicit val guardrailDecodeLocalDate: GuardrailDecoder[java.time.LocalDate] = instance(classOf[java.time.LocalDate])
                implicit val guardrailDecodeLocalDateTime: GuardrailDecoder[java.time.LocalDateTime] = instance(classOf[java.time.LocalDateTime])
                implicit val guardrailDecodeLocalTime: GuardrailDecoder[java.time.LocalTime] = instance(classOf[java.time.LocalTime])
                implicit val guardrailDecodeOffsetDateTime: GuardrailDecoder[java.time.OffsetDateTime] = instance(classOf[java.time.OffsetDateTime])
                implicit val guardrailDecodeZonedDateTime: GuardrailDecoder[java.time.ZonedDateTime] = instance(classOf[java.time.ZonedDateTime])
              }

              trait GuardrailValidator[A] {
                def validate(a: A)(implicit validator: javax.validation.Validator): scala.util.Try[A]
              }
              object GuardrailValidator {
                def instance[A]: GuardrailValidator[A] = new GuardrailValidator[A] {
                  override def validate(a: A)(implicit validator: javax.validation.Validator): scala.util.Try[A] = {
                    import _root_.scala.collection.JavaConverters._
                    scala.util.Try(validator.validate(a)).flatMap({
                      case violations if violations.isEmpty =>
                        scala.util.Success(a)
                      case violations =>
                        scala.util.Failure(new javax.validation.ValidationException(s"Validation of $${a.getClass.getSimpleName} failed: $${violations.asScala.map(viol => s"$${viol.getPropertyPath}: $${viol.getMessage}").mkString("; ")}"))
                    })
                  }
                }
                def noop[A]: GuardrailValidator[A] = new GuardrailValidator[A] {
                  override def validate(a: A)(implicit validator: javax.validation.Validator): scala.util.Try[A] = scala.util.Success(a)
                }

                implicit def guardrailValidateOption[A: GuardrailValidator]: GuardrailValidator[Option[A]] = new GuardrailValidator[Option[A]] {
                  override def validate(a: Option[A])(implicit validator: javax.validation.Validator): scala.util.Try[Option[A]] =
                    a.traverse(implicitly[GuardrailValidator[A]].validate)
                }
                implicit def guardrailValidateVector[A: GuardrailValidator]: GuardrailValidator[Vector[A]] = new GuardrailValidator[Vector[A]] {
                  override def validate(a: Vector[A])(implicit validator: javax.validation.Validator): scala.util.Try[Vector[A]] =
                    a.traverse(implicitly[GuardrailValidator[A]].validate)
                }
                implicit def guardrailValidateMap[A: GuardrailValidator]: GuardrailValidator[Map[String, A]] = new GuardrailValidator[Map[String, A]] {
                  override def validate(a: Map[String, A])(implicit validator: javax.validation.Validator): scala.util.Try[Map[String, A]] =
                    a.toVector.traverse({ case (k, v) => implicitly[GuardrailValidator[A]].validate(v).map((k, _)) }).map(_.toMap)
                }
                implicit val guardrailValidateBoolean: GuardrailValidator[Boolean] = noop
                implicit val guardrailValidateInt: GuardrailValidator[Int] = noop
                implicit val guardrailValidateLong: GuardrailValidator[Long] = noop
                implicit val guardrailValidateBigInt: GuardrailValidator[BigInt] = noop
                implicit val guardrailValidateFloat: GuardrailValidator[Float] = noop
                implicit val guardrailValidateDouble: GuardrailValidator[Double] = noop
                implicit val guardrailValidateBigDecimal: GuardrailValidator[BigDecimal] = noop
                implicit val guardrailValidateString: GuardrailValidator[String] = instance
                implicit val guardrailValidateBase64String: GuardrailValidator[Base64String] = instance
                implicit val guardrailValidateInstant: GuardrailValidator[java.time.Instant] = noop
                implicit val guardrailValidateLocalDate: GuardrailValidator[java.time.LocalDate] = noop
                implicit val guardrailValidateLocalDateTime: GuardrailValidator[java.time.LocalDateTime] = noop
                implicit val guardrailValidateLocalTime: GuardrailValidator[java.time.LocalTime] = noop
                implicit val guardrailValidateOffsetDateTime: GuardrailValidator[java.time.OffsetDateTime] = noop
                implicit val guardrailValidateZonedDateTime: GuardrailValidator[java.time.ZonedDateTime] = noop
              }
            }
         """
            )
          )
        ),
      newGenerateSupportDefinitions = () =>
        for {
          generatedSupportDefinitions <- baseInterp.generateSupportDefinitions()
        } yield {
          val (presence, others) = generatedSupportDefinitions.partition(_.className.value == "Presence")
          presence.headOption
            .map(
              defn =>
                defn.copy(
                  definition = defn.definition.map({
                    case q"object Presence { ..$stmts }" =>
                      q"""
                  object Presence {
                    import com.fasterxml.jackson.annotation.JsonInclude
                    import com.fasterxml.jackson.core.{JsonGenerator, JsonParser, JsonToken}
                    import com.fasterxml.jackson.databind._
                    import com.fasterxml.jackson.databind.`type`.ReferenceType
                    import com.fasterxml.jackson.databind.deser.ContextualDeserializer
                    import com.fasterxml.jackson.databind.deser.std.StdDeserializer
                    import com.fasterxml.jackson.databind.jsontype.TypeSerializer
                    import com.fasterxml.jackson.databind.ser.ContextualSerializer
                    import com.fasterxml.jackson.databind.ser.std.ReferenceTypeSerializer
                    import com.fasterxml.jackson.databind.util.{AccessPattern, NameTransformer}

                    private class PresenceReferenceSerializer(
                        refType: ReferenceType,
                        ts: TypeSerializer,
                        ser: JsonSerializer[AnyRef]
                    ) extends ReferenceTypeSerializer[Presence[_]](refType, true, ts, ser) {
                      override def withResolved(prop: BeanProperty, vts: TypeSerializer, valueSer: JsonSerializer[_], unwrapper: NameTransformer): ReferenceTypeSerializer[Presence[_]] =
                        new ResolvedPresenceSerializer(this, prop, vts, valueSer, unwrapper, _suppressableValue, _suppressNulls)
                      override def withContentInclusion(suppressableValue: AnyRef, suppressNulls: Boolean): ReferenceTypeSerializer[Presence[_]] =
                        new ResolvedPresenceSerializer(this, _property, _valueTypeSerializer, _valueSerializer, _unwrapper, suppressableValue, suppressNulls)
                      override def _isValuePresent(value: Presence[_]): Boolean = value.toOption.isDefined
                      override def _getReferenced(value: Presence[_]): AnyRef = value.toOption.get.asInstanceOf[AnyRef]
                      override def _getReferencedIfPresent(value: Presence[_]): AnyRef = value.toOption.orNull[Any].asInstanceOf[AnyRef]
                    }

                    private class ResolvedPresenceSerializer(
                        base: ReferenceTypeSerializer[_],
                        property: BeanProperty,
                        vts: TypeSerializer,
                        valueSer: JsonSerializer[_],
                        unwrapper: NameTransformer,
                        suppressableValue: AnyRef,
                        suppressNulls: Boolean
                    ) extends ReferenceTypeSerializer[Presence[_]](base, property, vts, valueSer, unwrapper, suppressableValue, suppressNulls) {
                      override def withResolved(prop: BeanProperty, vts: TypeSerializer, valueSer: JsonSerializer[_], unwrapper: NameTransformer): ReferenceTypeSerializer[Presence[_]] =
                        new ResolvedPresenceSerializer(this, prop, vts, valueSer, unwrapper, _suppressableValue, _suppressNulls)
                      override def withContentInclusion(suppressableValue: AnyRef, suppressNulls: Boolean): ReferenceTypeSerializer[Presence[_]] =
                        new ResolvedPresenceSerializer(this, _property, _valueTypeSerializer, _valueSerializer, _unwrapper, suppressableValue, suppressNulls)
                      override def _isValuePresent(value: Presence[_]): Boolean = value.toOption.isDefined
                      override def _getReferenced(value: Presence[_]): AnyRef = value.toOption.get.asInstanceOf[AnyRef]
                      override def _getReferencedIfPresent(value: Presence[_]): AnyRef = value.toOption.orNull[Any].asInstanceOf[AnyRef]
                    }

                    class PresenceSerializer extends JsonSerializer[Presence[_]] with ContextualSerializer {
                      override def serialize(value: Presence[_], gen: JsonGenerator, serializers: SerializerProvider): Unit =
                        throw new AssertionError("Contextual serializer must be used")

                      override def createContextual(prov: SerializerProvider, property: BeanProperty): JsonSerializer[_] = {
                        Option(property.getType.containedType(0)).fold({
                          throw new AssertionError("Presence type has no contained type")
                        })({
                          case tpe: ReferenceType => new PresenceReferenceSerializer(tpe, prov.findTypeSerializer(tpe), prov.findValueSerializer(tpe))
                          case tpe => new PresenceReferenceSerializer(ReferenceType.upgradeFrom(property.getType, tpe), prov.findTypeSerializer(tpe), prov.findValueSerializer(tpe))
                        })
                      }
                    }

                    private class PresenceOptionContextualDeserializer(innerType: JavaType) extends StdDeserializer[Presence[Option[Any]]](innerType) {
                      override def deserialize(p: JsonParser, ctxt: DeserializationContext): Presence[Option[Any]] =
                        Presence.present(Option(p.readValueAs(innerType.getRawClass)))

                      override def getNullValue(ctxt: DeserializationContext): Presence[Option[Any]] =
                        ctxt.getParser.getCurrentToken match {
                          case JsonToken.VALUE_NULL => Presence.present(None)
                          case _ => Presence.absent
                        }

                      override def getNullAccessPattern: AccessPattern = AccessPattern.DYNAMIC
                    }

                    private class PresenceContextualDeserializer(innerType: JavaType) extends StdDeserializer[Presence[_]](innerType) {
                      override def deserialize(p: JsonParser, ctxt: DeserializationContext): Presence[_] =
                        Presence.present(p.readValueAs(innerType.getRawClass))

                      override def getNullValue(ctxt: DeserializationContext): Presence[Option[_]] =
                        ctxt.getParser.getCurrentToken match {
                          case JsonToken.VALUE_NULL => throw new JsonMappingException(ctxt.getParser, s"null is not valid for '$${ctxt.getParser.getCurrentName}'")
                          case _ => Presence.absent
                        }

                      override def getNullAccessPattern: AccessPattern = AccessPattern.DYNAMIC
                    }

                    class PresenceDeserializer extends JsonDeserializer[Presence[_]] with ContextualDeserializer {
                      override def deserialize(p: JsonParser, ctxt: DeserializationContext): Presence[_] =
                        throw new AssertionError("Contextual deserializer must be used")

                      override def createContextual(ctxt: DeserializationContext, property: BeanProperty): JsonDeserializer[_] =
                        Option(ctxt.getContextualType.containedType(0)).flatMap({
                          case tpe if classOf[Option[_]].isAssignableFrom(tpe.getRawClass) =>
                            Option(tpe.containedType(0)).map(new PresenceOptionContextualDeserializer(_))
                          case tpe => Some(new PresenceContextualDeserializer(tpe))
                        }).getOrElse({
                          throw new JsonMappingException(ctxt.getParser, "Presence type has no contained type")
                        })
                    }

                    private class OptionContextualDeserializer(innerType: JavaType, nullValue: JsonToken => Option[_]) extends StdDeserializer[Option[_]](innerType) {
                      override def deserialize(p: JsonParser, ctxt: DeserializationContext): Option[_] =
                        Option(p.readValueAs(innerType.getRawClass))

                      override def getNullValue(ctxt: DeserializationContext): Option[_] = nullValue(ctxt.getParser.getCurrentToken)

                      override def getNullAccessPattern: AccessPattern = AccessPattern.DYNAMIC
                    }

                    class OptionNonNullDeserializer extends JsonDeserializer[Option[_]] with ContextualDeserializer {
                      override def deserialize(p: JsonParser, ctxt: DeserializationContext): Option[_] =
                        throw new AssertionError("Contextual serializer must be used")

                      override def createContextual(ctxt: DeserializationContext, property: BeanProperty): JsonDeserializer[_] =
                        Option(ctxt.getContextualType.containedType(0)).fold({
                          throw new JsonMappingException(ctxt.getParser, "Option type has no contained type")
                        })(
                          tpe => new OptionContextualDeserializer(tpe, {
                            case JsonToken.VALUE_NULL => throw new JsonMappingException(ctxt.getParser, s"null is not valid for '$${ctxt.getParser.getCurrentName}'")
                            case _ => None
                          })
                        )
                    }

                    class OptionNonMissingDeserializer extends JsonDeserializer[Option[_]] with ContextualDeserializer {
                      override def deserialize(p: JsonParser, ctxt: DeserializationContext): Option[_] =
                        throw new AssertionError("Contextual serializer must be used")

                      override def createContextual(ctxt: DeserializationContext, property: BeanProperty): JsonDeserializer[_] =
                        Option(ctxt.getContextualType.containedType(0)).fold({
                          throw new JsonMappingException(ctxt.getParser, "Option type has no contained type")
                        })(
                          tpe => new OptionContextualDeserializer(tpe, {
                            case JsonToken.VALUE_NULL => None
                            case _ => throw new JsonMappingException(ctxt.getParser, s"'$${ctxt.getParser.getCurrentName}' is missing")
                          })
                        )
                    }

                    ..$stmts
                  }
               """
                    case other => other
                  })
                )
            )
            .toList ++ others
        }
    )
  }

  def PolyProtocolTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): PolyProtocolTerms[ScalaLanguage, Target] = {
    val baseInterp = new CirceProtocolGenerator.PolyProtocolTermInterp
    baseInterp.copy(
      newRenderSealedTrait = (className, params, discriminator, parents, children) =>
        for {
          renderedTrait      <- baseInterp.renderSealedTrait(className, params, discriminator, parents, children)
          discriminatorParam <- Target.pure(params.find(_.name.value == discriminator.propertyName))
        } yield {
          val subTypes = children.map(
            child =>
              q"new com.fasterxml.jackson.annotation.JsonSubTypes.Type(name = ${Lit.String(discriminatorValue(discriminator, child))}, value = classOf[${Type.Name(child)}])"
          )

          renderedTrait.copy(
            mods = List(
                mod"""
        @com.fasterxml.jackson.annotation.JsonTypeInfo(
          use = com.fasterxml.jackson.annotation.JsonTypeInfo.Id.NAME,
          include = com.fasterxml.jackson.annotation.JsonTypeInfo.As.PROPERTY,
          property = ${Lit.String(discriminator.propertyName)}
        )
        """,
                mod"""
        @com.fasterxml.jackson.annotation.JsonSubTypes(Array(
          ..$subTypes
        ))
         """
              ) ++ renderedTrait.mods,
            templ = renderedTrait.templ.copy(
              stats = renderedTrait.templ.stats ++ discriminatorParam.map(
                      param => q"def ${Term.Name(param.term.name.value)}: ${param.term.decltpe.getOrElse(t"Any")}"
                    )
            )
          )
        },
      newEncodeADT = (_, _, _) => Target.pure(None),
      newDecodeADT = (_, _, _) => Target.pure(None),
      newRenderADTStaticDefns = (className, discriminator, encoder, decoder) =>
        for {
          renderedADTStaticDefns <- baseInterp.renderADTStaticDefns(className, discriminator, encoder, decoder)
          classType = Type.Name(className)
        } yield renderedADTStaticDefns.copy(
          definitions = renderedADTStaticDefns.definitions ++ List(
                  q"implicit val ${Pat.Var(Term.Name(s"encode${className}"))}: GuardrailEncoder[$classType] = GuardrailEncoder.instance",
                  q"implicit val ${Pat.Var(Term.Name(s"decode${className}"))}: GuardrailDecoder[$classType] = GuardrailDecoder.instance(new com.fasterxml.jackson.core.`type`.TypeReference[$classType] {})",
                  q"implicit val ${Pat.Var(Term.Name(s"validate${className}"))}: GuardrailValidator[$classType] = GuardrailValidator.instance"
                )
        )
    )
  }

  private def discriminatorValue(discriminator: Discriminator[ScalaLanguage], className: String): String =
    discriminator.mapping
      .collectFirst({ case (value, elem) if elem.name == className => value })
      .getOrElse(className)
}
