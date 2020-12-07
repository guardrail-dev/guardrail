package com.twilio.guardrail.generators.Scala

import cats.data.NonEmptyList
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.protocol.terms.{ ApplicationJson, ContentType, Response, Responses }
import com.twilio.guardrail.StrictProtocolElems

import scala.meta._

object Http4sHelper {
  private val ValueFieldName = "value"
  private val ValueTerm      = Term.Name(ValueFieldName)

  def generateResponseDefinitions(
      responseClsName: String,
      responses: Responses[ScalaLanguage],
      protocolElems: List[StrictProtocolElems[ScalaLanguage]]
  ): List[Defn] = {
    val isGeneric                         = isDefinitionGeneric(responses)
    val extraTypes: List[Type]            = if (isGeneric) List(t"F") else Nil
    val extraTypeParams: List[Type.Param] = if (isGeneric) List(tparam"F[_]") else Nil

    val responseSuperType     = Type.Name(responseClsName)
    val responseSuperTerm     = Term.Name(responseClsName)
    val responseSuperTemplate = template"${Init(if (isGeneric) Type.Apply(responseSuperType, extraTypes) else responseSuperType, Name(""), List.empty)}"

    val (terms, foldPair) = responses.value
      .map({
        case Response(statusCodeName, valueType, headers) =>
          val responseTerm = Term.Name(s"${statusCodeName.value}")
          val responseName = Type.Name(s"${statusCodeName.value}")

          val foldHandleName = Term.Name(s"handle${statusCodeName.value}")

          val allParams = valueType.map(tpe => (tpe, q"value")).toList ++ headers.value.map(h => (h.tpe, h.term))
          allParams match {
            case Nil if !isGeneric =>
              val foldParameter = param"${foldHandleName}: => A"
              val foldCase      = p"case ${responseSuperTerm}.${responseTerm} => ${foldHandleName}"
              (q"case object $responseTerm extends $responseSuperTemplate", (foldParameter, foldCase))
            case _ =>
              val responseCaseType = if (isGeneric) t"$responseSuperTerm.$responseName[F]" else t"$responseSuperTerm.$responseName"
              val foldParameter    = param"${foldHandleName}: (..${allParams.map(_._1)}) => A"
              val foldCase         = p"case x: $responseCaseType => ${foldHandleName}(..${allParams.map(t => q"x.${t._2}")})"
              (
                q"case class  $responseName[..$extraTypeParams](..${allParams.map(t => param"${t._2}: ${t._1}")}) extends $responseSuperTemplate",
                (foldParameter, foldCase)
              )
          }
      })
      .unzip

    val (foldParams, foldCases) = foldPair.unzip

    // Union and Coproduct Support
    val typeParams                                                                = if (isGeneric) List(tparam"F[_]") else List()
    val (responseTypeDefns, responseConstructorDefns, unionTypeDefn, toUnionDefn) = makeToUnionDefns(responses.value, typeParams)
    val toCoproductDefn                                                           = makeToCoproductDefn(responses.value, typeParams)

    val companion = q"""
            object ${Term.Name(responseClsName)} {
              ..$terms

              ..$responseTypeDefns
              ..$responseConstructorDefns
              $unionTypeDefn
            }
          """

    val cls = q"""
        sealed abstract class ${responseSuperType}[..$extraTypeParams] {
          def fold[A](..${foldParams}): A = ${Term.Match(Term.This(Name("")), foldCases)}

          import ${Term.Name(responseClsName)}._
          $toUnionDefn
          $toCoproductDefn
        }
      """
    List[Defn](cls, companion)
  }

  def generateDecoder(tpe: Type, consumes: Seq[ContentType]): Term =
    if (consumes.contains(ApplicationJson) || consumes.isEmpty)
      q"jsonOf[F, $tpe]"
    else
      tpe match {
        case t"Option[$_]" => q"""
                  decodeBy(MediaType.text.plain) { msg =>
                    msg.contentLength.filter(_ > 0).fold[DecodeResult[F, $tpe]](DecodeResult.success(None)){ _ =>
                      DecodeResult.success(decodeString(msg)).flatMap { str =>
                        Json.fromString(str).as[$tpe]
                          .fold(failure =>
                            DecodeResult.failure(InvalidMessageBodyFailure(s"Could not decode response: $$str", Some(failure))),
                            DecodeResult.success(_)
                          )
                      }
                    }
                  }
                """
        case _             => q"EntityDecoder[F, $tpe]"
      }

  def generateEncoder(tpe: Type, produces: Seq[ContentType]): Term =
    if (produces.contains(ApplicationJson) || produces.isEmpty)
      q"jsonEncoderOf[F, $tpe]"
    else
      tpe match {
        case t"Option[$_]" => q"""
                  encodeBy(`Content-Type`(MediaType.text.plain, Charset.`UTF-8`)) { a: $tpe =>
                    a.fold[Entity[F]](Entity.empty)(e => EntityEncoder[F, String].toEntity(e.toString))
                  }
                """
        case _             => q"EntityEncoder[F, $tpe]"
      }

  def generateEntityResponseGenerator(term: Term.Ref): Term =
    q"""
       new org.http4s.dsl.impl.EntityResponseGenerator[F,F] {
          def status = $term
          val liftG = cats.arrow.FunctionK.id
       }
     """

  def isDefinitionGeneric(responses: Responses[ScalaLanguage]): Boolean =
    responses.value.exists { response =>
      response.value.exists {
        case (_, tpe, _) =>
          tpe match {
            case t"fs2.Stream[F,Byte]" => true
            case _                     => false
          }
      }
    }

  // toUnion

  /**
    * Define a function (along with supporting types and constructor functions) for projecting a [[Response]] to a
    * discriminated union of extensible records. Union members are labeled by status code, each record contains a value
    * field, and each record contains optional header fields. Empty [[Response]]s have their value set to [[Unit]]. The
    * result will look something like the following:
    *
    * {{{
    * // With type parameters
    * def toUnion: UnionType[F] = fold(value => createOkRecord(value), createBadRequestRecord(()))
    *
    * // Without type parameters
    * def toUnion: UnionType = fold(value => createOkRecord(value), createBadRequestRecord(()))
    * }}}
    *
    * Notice that `toUnion` is defined in terms of `fold`; this may change in future releases, if and when extensible
    * records become the canonical way of representing [[Response]]s.
    *
    * @param responses the list of possible [[Response]]s to be projected to a discriminated union of extensible records
    * @param typeParams type parameters, if any
    * @return a tuple containing extensible record-based type definitions for [[Response]]s, extensible record-based
    *         constructors for [[Response]]s, a discriminated union-based type definition for the full set of
    *         [[Response]]s, and the `toUnion` definition itself
    */
  private def makeToUnionDefns(
      responses: List[Response[ScalaLanguage]],
      typeParams: List[Type.Param]
  ): (List[Defn.Type], List[Defn.Def], Defn.Type, Defn.Def) = {
    val unionType = {
      val typeParamTypes = typeParams.map(typeParam => Type.Name(typeParam.name.value))
      if (typeParamTypes.isEmpty) t"UnionType" else t"UnionType[..$typeParamTypes]"
    }

    // No `unzip4`, hence we have to fold and build our tuple directly.
    val (responseTypeDefns, responseConstructorDefns, unionTypeMembers, foldToUnionCases) = responses
      .foldRight[(List[Defn.Type], List[Defn.Def], List[(Either[Lit.Int, Lit.String], Type)], List[Term])]((List(), List(), List(), List()))(
        (response, results) => {
          val headers          = response.headers.value
          val unionMemberLabel = Lit.Int(response.statusCode)
          val valueTypeO       = response.value.map(_._2)

          // Response Type Definition
          val responseTypeDefn = makeResponseTypeDefn(response, typeParams)

          // Response Constructor Definition
          val responseConstructorDefn = makeResponseConstructorDefn(response, typeParams)

          // Union Type Member
          val unionTypeMember = (Left(unionMemberLabel), makeAppliedResponseType(response, typeParams))

          // Fold to Union Case
          val foldToUnionCase = {
            val headerTerms     = headers.map(_.term)
            val foldParams      = valueTypeO.map(_ => ValueTerm).toList ++ headerTerms
            val foldTerms       = valueTypeO.fold[Term](q"()")(_ => ValueTerm) :: headerTerms
            val constructorTerm = makeResponseConstructorTerm(response)
            if (foldParams.isEmpty && typeParams.isEmpty)
              q"Coproduct[$unionType]($unionMemberLabel ->> $constructorTerm(..$foldTerms))"
            else
              q"(..${foldParams.map(foldParam => param"$foldParam")}) => Coproduct[$unionType]($unionMemberLabel ->> $constructorTerm(..$foldTerms))"
          }

          (responseTypeDefn :: results._1, responseConstructorDefn :: results._2, unionTypeMember :: results._3, foldToUnionCase :: results._4)
        }
      )

    val unionTypeDefn = makeUnionTypeDefn("UnionType", typeParams, unionTypeMembers)
    val toUnionDefn   = q"def toUnion: $unionType = fold(..$foldToUnionCases)"

    (responseTypeDefns, responseConstructorDefns, unionTypeDefn, toUnionDefn)
  }

  // toCoproduct

  /**
    * Define a function for projecting a [[Response]] to a coproduct. Why is this useful? Sometimes you don't need a
    * discriminated union, and filtering out duplicated types in the coproduct can be onerous.
    *
    * For ergonomic reasons, we project the [[Response]] value for any [[Response]] which lacks headers. This makes it
    * easier to use coproduct functions like `select` without having to provide complicated types. [[Response]]s which
    * include headers are still represented as extensible records.
    *
    * {{{
    * // Imagine we have a Response which is either 200 OK containing Unit or 302 Found containing a Location header.
    * val coproduct: Unit :+: FoundRecord :+: CNil = response.toCoproduct
    * val unitO: Option[Unit]                      = coproduct.select[Unit]         // 200 OK
    * val foundO: Option[FoundRecord]              = coproduct.select[FoundRecord]  // 302 Found
    * }}}
    *
    * We also re-use existing types where possible. For example, if there is a unique [[Response]] (with respect to its
    * value and headers), that [[Response]]'s type alias will appear in the coproduct; however, if multiple
    * [[Response]]s share the same value and headers -- i.e., their types are identical -- those [[Response]]s will
    * appear as an unnamed extensible record in the coproduct. This is because there is no canonical type alias for
    * identical types.
    *
    * {{{
    * // Imagine we have a Response which is either a 301 Moved Permanently or 302 Found, both containing a Location
    * // header. The two Response types are identical, hence there is no canonical type alias to use.
    * val coproduct: Record.`"value" -> Unit, "Location" -> String`.T :+: CNil = response.toCoproduct
    * }}}
    *
    * Finally, notice that we don't give a top-level return type to `toCoproduct`. Although [[Response]] values are
    * currently nominally typed, if we ever convert [[Response]] values to an extensible records-based representation,
    * value types with different names may actually be identical (in fact, this can already be done with `x-jvm-type`).
    * It's difficult to detect cases of identical types in the code generator and, therefore, difficult to construct a
    * return type. We may eventually defer this to the compiler. See https://stackoverflow.com/a/54981388 for an example
    * of how to do this.
    *
    * {{{
    * // Consider the case where we have two response types which include a result code and message.
    * type Success = Record.`"code" -> Int, "message" -> String`.T
    * type Failure = Record.`"code" -> Int, "message" -> String`.T
    *
    * type OkRecord         = Record.`"value" -> Success`.T
    * type BadRequestRecord = Record.`"value" -> Failure`.T
    *
    * type Response = Union.`200 -> OkRecord, 400 -> BadRequestRecord`.T
    *
    * val response: Response = ???
    *
    * // The coproduct should ultimately have a single member, since Success and Failure are the same type.
    * val coproduct: Record.`"code" -> Int, "message" -> String`.T :+: CNil = response.toCoproduct
    * }}}
    *
    * @param responses the list of possible [[Response]]s to be projected to a coproduct
    * @param typeParams type parameters, if any
    * @return the `toCoproduct` definition
    */
  private def makeToCoproductDefn(responses: List[Response[ScalaLanguage]], typeParams: List[Type.Param]): Defn = {
    val responseToType = responses
      .foldRight[Map[(String, Set[String]), NonEmptyList[Response[ScalaLanguage]]]](Map.empty)((response, map) => {
        val key = {
          val valueType = response.value.map(_._2).getOrElse(t"Unit")
          val headers   = response.headers.value.map(_.name).toSet
          (valueType.syntax, headers)
        }
        val value = map.get(key).fold(NonEmptyList.one(response))(responses => response :: responses)
        map.updated(key, value)
      })
      .foldRight[Map[Response[ScalaLanguage], Type]](Map.empty)({
        // Case 1: No headers
        case ((_, responses @ NonEmptyList(response, _)), map) if response.headers.value.isEmpty =>
          val valueType = response.value.map(_._2).getOrElse(t"Unit")
          responses.toList.foldRight(map)((response, map) => map.updated(response, valueType))
        // Case 2: Unique response, with headers
        case ((_, NonEmptyList(response, Nil)), map) =>
          val appliedResponseType = makeAppliedResponseType(response, typeParams)
          map.updated(response, appliedResponseType)
        // Case 3: Multiple responses, with headers
        case ((_, responses @ NonEmptyList(response, _)), map) =>
          val responseType = makeResponseType(response)
          responses.toList.foldRight(map)((response, map) => map.updated(response, responseType))
      })

    // We unfortunately lack distinctBy
    val coproductType = responses
      .foldRight[(Type, Set[String])]((t"CNil", Set[String]()))(
        (response, result) =>
          responseToType
            .get(response)
            .fold(result)(
              tpe =>
                result match {
                  case (tail, types) if !types.contains(tpe.toString) => (t"$tpe :+: $tail", types + tpe.toString)
                  case _                                              => result
                }
            )
      )
      ._1

    val coproductCases = responses.map(response => {
      val responseTypeName    = makeResponseTypeName(response)
      val caseTerm            = Term.Name(s"handle${responseTypeName.value}")
      val witness             = makeWitness(Left(Lit.Int(response.statusCode)))
      val appliedResponseType = makeAppliedResponseType(response, typeParams)
      val valueToInject       = if (response.headers.value.isEmpty) q"""pair._2.get("value")""" else q"pair._2"
      q"implicit def $caseTerm = at[($witness, $appliedResponseType)](pair => Coproduct[CoproductType]($valueToInject))"
    })

    q"""
        def toCoproduct = {
          type CoproductType = $coproductType

          case object f extends Poly1 {
            ..$coproductCases
          }

          toUnion.fields.fold(f)
        }
    """
  }

  // Response Types

  /**
    * Define an extensible record-based type for a particular [[Response]]. The type name is given by
    * [[makeResponseTypeName]] and the actual type is given by [[makeResponseType]].
    *
    * @param response the [[Response]] to define an extensible record-based type for
    * @param typeParams type parameters, if any
    * @return the [[Response]]'s extensible record-based type definition
    */
  private def makeResponseTypeDefn(response: Response[ScalaLanguage], typeParams: List[Type.Param]): Defn.Type = {
    val responseTypeName = makeResponseTypeName(response)
    val responseType     = makeResponseType(response)
    if (typeParams.isEmpty) q"type $responseTypeName = $responseType"
    else q"type $responseTypeName[..$typeParams] = $responseType"
  }

  /**
    * Make an extensible record-based type for a particular [[Response]]. Imagine the [[Response]] contains a "Location"
    * header. Then the result will look something like the following:
    *
    * {{{
    * FieldType[Witness.`"value"`.T, Unit] :: FieldType[Witness.`"Location"`.T, String] :: HNil
    * }}}
    *
    * Note that "value" labels the value contained in all [[Response]]s. Header fields are optional.
    *
    * @param response the [[Response]] to make an extensible record-based type for
    * @return the [[Response]]'s extensible record-based type
    */
  // NOTE: Worth reviewing whether 1. headers are sorted, and 2. headers have a regular capitalization. Both of these
  // can impact whether extensible record types which ought to be identical actually are.
  private def makeResponseType(response: Response[ScalaLanguage]): Type = {
    val headers = response.headers.value
    val fields = {
      val valueType    = response.value.map(_._2).getOrElse(t"Unit")
      val valueField   = (Lit.String(ValueFieldName), valueType)
      val headerFields = headers.map(header => (Lit.String(header.name), header.tpe))
      valueField :: headerFields
    }
    fields.foldRight[Type](t"HNil")((field, tail) => {
      val head = makeFieldType(Right(field._1), field._2)
      t"$head :: $tail"
    })
  }

  /**
    * Make the applied type for a particular [[Response]]. If the [[Response]] type requires no parameters, this is
    * equivalent to [[makeResponseTypeName]].
    *
    * @param response the [[Response]] to make an applied type for
    * @param typeParams type parameters, if any
    * @return the applied type for the given [[Response]]
    */
  private def makeAppliedResponseType(response: Response[ScalaLanguage], typeParams: List[Type.Param]): Type = {
    val responseTypeName = makeResponseTypeName(response)
    val typeParamTypes   = typeParams.map(typeParam => Type.Name(typeParam.name.value))
    if (typeParamTypes.isEmpty) responseTypeName else t"$responseTypeName[..$typeParamTypes]"
  }

  private def makeResponseTypeName(response: Response[ScalaLanguage]): Type.Name =
    Type.Name(s"${response.statusCodeName.value}Record")

  // Response Constructors

  /**
    * Define an extensible record-based constructor for a particular [[Response]]. The result will look something like
    * the following:
    *
    * {{{
    * // With type parameters
    * def createOkRecord[F[_]](value: fs2.Stream[F, Byte], remoteUser: String): OkRecord[F] =
    *   ("value" ->> value) :: ("Remote-User" ->> remoteUser) :: HNil
    *
    * // Without type parameters
    * def createOkRecord(value: Unit, remoteUser: String): OkRecord =
    *   ("value" ->> value) :: ("Remote-User" ->> remoteUser) :: HNil
    * }}}
    *
    * @param response the [[Response]] to define an extensible record-based constructor for
    * @param typeParams type parameters, if any
    * @return the [[Response]]'s extensible record-based constructor definition
    */
  private def makeResponseConstructorDefn(response: Response[ScalaLanguage], typeParams: List[Type.Param]): Defn.Def = {
    val constructorTerm = makeResponseConstructorTerm(response)

    val appliedResponseType = makeAppliedResponseType(response, typeParams)

    val (valueFieldNameAndTerm, valueParam) = {
      val valueType  = response.value.map(_._2).getOrElse(t"Unit")
      val valueParam = param"$ValueTerm: $valueType"
      ((Lit.String(ValueFieldName), ValueTerm), valueParam)
    }

    val (headerFieldNamesAndTerms, headerParams) = response.headers.value
      .map(header => {
        val headerParam = param"${header.term}: ${header.tpe}"
        ((Lit.String(header.name), header.term), headerParam)
      })
      .unzip

    val params = valueParam :: headerParams

    val constructorBody = {
      val fieldNamesAndTerms = valueFieldNameAndTerm :: headerFieldNamesAndTerms
      fieldNamesAndTerms.foldRight[Term](q"HNil")((fieldNameAndTerm, tail) => {
        q"(${fieldNameAndTerm._1} ->> ${fieldNameAndTerm._2}) :: $tail"
      })
    }

    if (typeParams.isEmpty) q"def $constructorTerm(..$params): $appliedResponseType = $constructorBody"
    else q"def $constructorTerm[..$typeParams](..$params): $appliedResponseType = $constructorBody"
  }

  private def makeResponseConstructorTerm(response: Response[ScalaLanguage]): Term.Name = {
    val responseTypeName = makeResponseTypeName(response)
    Term.Name(s"create${responseTypeName.value}")
  }

  // Shapeless

  /**
    * Define a discriminated union. The result will look something like the following:
    *
    * {{{
    * // With type parameters
    * type Foo[F[_]] = FieldType[Witness.`"bar"`.T, Bar[F]] :: FieldType[Witness.`123"`.T, Baz] :: HNil
    *
    * // Without type parameters
    * type Foo = FieldType[Witness.`"bar"`.T, Bar] :: FieldType[Witness.`123`.T, Baz] :: HNil
    * }}}
    *
    * @param unionName the name of the discriminated union to define
    * @param typeParams type parameters, if any
    * @param members members of the discriminated union, represented as tuples of labels and the types they label;
    *                any free type variables must appear in `typeParams`, although this is unchecked
    * @return the discriminated union's type definition
    */
  private def makeUnionTypeDefn(unionName: String, typeParams: List[Type.Param], members: List[(Either[Lit.Int, Lit.String], Type)]): Defn.Type = {
    val unionTypeName = Type.Name(unionName)
    val unionType = members.foldRight[Type](t"CNil")((member, tail) => {
      val head = makeFieldType(member._1, member._2)
      t"$head :+: $tail"
    })
    if (typeParams.isEmpty) q"type $unionTypeName = $unionType"
    else q"type $unionTypeName[..$typeParams] = $unionType"
  }

  private def makeFieldType(fieldLit: Either[Lit.Int, Lit.String], fieldType: Type): Type = {
    val witness = makeWitness(fieldLit)
    t"FieldType[$witness, $fieldType]"
  }

  // NOTE: Could we support the full set of literals here and just accept `Lit`?
  private def makeWitness(lit: Either[Lit.Int, Lit.String]): Type = {
    val termName    = lit.fold(lit => Term.Name(lit.syntax), lit => Term.Name(lit.syntax))
    val witnessTerm = q"Witness.$termName"
    Type.Select(witnessTerm, t"T")
  }
}
