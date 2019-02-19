package com.twilio.guardrail
package generators
package Java

import _root_.io.swagger.v3.oas.models.media._
import cats.implicits._
import cats.~>
import cats.data.NonEmptyList
import com.twilio.guardrail.extract.{ Default, ScalaEmptyIsNull, ScalaType }
import com.twilio.guardrail.shims._
import com.twilio.guardrail.terms
import java.util.Locale
import com.twilio.guardrail.languages.{ LA, JavaLanguage }
import com.twilio.guardrail.protocol.terms.protocol._
import scala.collection.JavaConverters._
import scala.meta._

object JacksonProtocolGenerator {
  import ProtocolGenerator._

  object EnumProtocolTermInterp extends (EnumProtocolTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: EnumProtocolTerm[JavaLanguage, T]): Target[T] = term match {
      case ExtractEnum(swagger) =>
        Target.raiseError(s"ExtractEnum($swagger)")
      case RenderMembers(clsName, elems) =>
        Target.raiseError(s"RenderMembers($clsName, $elems)")
      case EncodeEnum(clsName) =>
        Target.raiseError(s"EncodeEnum($clsName)")
      case DecodeEnum(clsName) =>
        Target.raiseError(s"DecodeEnum($clsName)")
      case RenderClass(clsName, tpe) =>
        Target.raiseError(s"RenderClass($clsName, $tpe)")
      case RenderStaticDefns(clsName, members, accessors, encoder, decoder) =>
        Target.raiseError(s"RenderStaticDefns($clsName, $members, $accessors, $encoder, $decoder)")
      case BuildAccessor(clsName, termName) =>
        Target.raiseError(s"BuildAccessor($clsName, $termName)")
    }
  }

  object ModelProtocolTermInterp extends (ModelProtocolTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ModelProtocolTerm[JavaLanguage, T]): Target[T] = term match {
      case ExtractProperties(swagger) =>
        Target.raiseError(s"ExtractProperties($swagger)")
      case TransformProperty(clsName, name, property, meta, needCamelSnakeConversion, concreteTypes, isRequired) =>
        Target.raiseError(s"TransformProperty($clsName, $name, $property, $meta, $needCamelSnakeConversion, $concreteTypes, $isRequired)")
      case RenderDTOClass(clsName, selfTerms, parents) =>
        Target.raiseError(s"RenderDTOClass($clsName, $selfTerms, $parents)")
      case EncodeModel(clsName, needCamelSnakeConversion, selfParams, parents) =>
        Target.raiseError(s"EncodeModel($clsName, $needCamelSnakeConversion, $selfParams, $parents)")
      case DecodeModel(clsName, needCamelSnakeConversion, selfParams, parents) =>
        Target.raiseError(s"DecodeModel($clsName, $needCamelSnakeConversion, $selfParams, $parents)")
      case RenderDTOStaticDefns(clsName, deps, encoder, decoder) =>
        Target.raiseError(s"RenderDTOStaticDefns($clsName, $deps, $encoder, $decoder)")
    }
  }

  object ArrayProtocolTermInterp extends (ArrayProtocolTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ArrayProtocolTerm[JavaLanguage, T]): Target[T] = term match {
      case ExtractArrayType(arr, concreteTypes) =>
        Target.raiseError(s"ExtractArrayType($arr, $concreteTypes)")
    }
  }

  object ProtocolSupportTermInterp extends (ProtocolSupportTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ProtocolSupportTerm[JavaLanguage, T]): Target[T] = term match {
      case ExtractConcreteTypes(definitions) =>
        Target.raiseError(s"ExtractConcreteTypes($definitions)")
      case ProtocolImports() =>
        Target.raiseError(s"ProtocolImports()")
      case PackageObjectImports() =>
        Target.raiseError(s"PackageObjectImports()")
      case PackageObjectContents() =>
        Target.raiseError(s"PackageObjectContents()")
    }
  }

  object PolyProtocolTermInterp extends (PolyProtocolTerm[JavaLanguage, ?] ~> Target) {
    override def apply[A](fa: PolyProtocolTerm[JavaLanguage, A]): Target[A] = fa match {
      case ExtractSuperClass(swagger, definitions) =>
        Target.raiseError(s"ExtractSuperClass($swagger, $definitions)")
      case RenderADTStaticDefns(clsName, discriminator, encoder, decoder) =>
        Target.raiseError(s"RenderADTStaticDefns($clsName, $discriminator, $encoder, $decoder)")
      case DecodeADT(clsName, children) =>
        Target.raiseError(s"DecodeADT($clsName, $children)")
      case EncodeADT(clsName, children) =>
        Target.raiseError(s"EncodeADT($clsName, $children)")
      case RenderSealedTrait(className, terms, discriminator, parents) =>
        Target.raiseError(s"RenderSealedTrait($className, $terms, $discriminator, $parents)")
    }
  }
}
