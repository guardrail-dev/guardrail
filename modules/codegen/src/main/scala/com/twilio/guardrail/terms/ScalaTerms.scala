package com.twilio.guardrail
package terms

import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.SwaggerUtil.LazyResolvedType
import java.nio.file.Path

class ScalaTerms[L <: LA, F[_]](implicit I: InjectK[ScalaTerm[L, ?], F]) {
  def litString(value: String): Free[F, L#Term]        = Free.inject[ScalaTerm[L, ?], F](LitString(value))
  def litFloat(value: Float): Free[F, L#Term]          = Free.inject[ScalaTerm[L, ?], F](LitFloat(value))
  def litDouble(value: Double): Free[F, L#Term]        = Free.inject[ScalaTerm[L, ?], F](LitDouble(value))
  def litInt(value: Int): Free[F, L#Term]              = Free.inject[ScalaTerm[L, ?], F](LitInt(value))
  def litLong(value: Long): Free[F, L#Term]            = Free.inject[ScalaTerm[L, ?], F](LitLong(value))
  def litBoolean(value: Boolean): Free[F, L#Term]      = Free.inject[ScalaTerm[L, ?], F](LitBoolean(value))
  def liftOptionalType(value: L#Type): Free[F, L#Type] = Free.inject[ScalaTerm[L, ?], F](LiftOptionalType(value))
  def liftOptionalTerm(value: L#Term): Free[F, L#Term] = Free.inject[ScalaTerm[L, ?], F](LiftOptionalTerm(value))
  def emptyOptionalTerm(): Free[F, L#Term]             = Free.inject[ScalaTerm[L, ?], F](EmptyOptionalTerm())
  def liftVectorType(value: L#Type): Free[F, L#Type]   = Free.inject[ScalaTerm[L, ?], F](LiftVectorType(value))
  def liftVectorTerm(value: L#Term): Free[F, L#Term]   = Free.inject[ScalaTerm[L, ?], F](LiftVectorTerm(value))
  def liftMapType(value: L#Type): Free[F, L#Type]      = Free.inject[ScalaTerm[L, ?], F](LiftMapType(value))

  def lookupEnumDefaultValue(tpe: L#TypeName, defaultValue: L#Term, values: List[(String, L#TermName, L#TermSelect)]): Free[F, L#TermSelect] =
    Free.inject[ScalaTerm[L, ?], F](LookupEnumDefaultValue(tpe, defaultValue, values))
  def jsonType(): Free[F, L#Type] = Free.inject[ScalaTerm[L, ?], F](JsonType())

  def embedArray(tpe: LazyResolvedType[L]): Free[F, LazyResolvedType[L]] = Free.inject[ScalaTerm[L, ?], F](EmbedArray(tpe))
  def embedMap(tpe: LazyResolvedType[L]): Free[F, LazyResolvedType[L]]   = Free.inject[ScalaTerm[L, ?], F](EmbedMap(tpe))

  def parseType(value: String): Free[F, Option[L#Type]]         = Free.inject[ScalaTerm[L, ?], F](ParseType(value))
  def parseTypeName(value: String): Free[F, Option[L#TypeName]] = Free.inject[ScalaTerm[L, ?], F](ParseTypeName(value))
  def pureTermName(value: String): Free[F, L#TermName]          = Free.inject[ScalaTerm[L, ?], F](PureTermName(value))
  def pureTypeName(value: String): Free[F, L#TypeName]          = Free.inject[ScalaTerm[L, ?], F](PureTypeName(value))

  def pureMethodParameter(name: L#TermName, tpe: L#Type, default: Option[L#Term]): Free[F, L#MethodParameter] =
    Free.inject[ScalaTerm[L, ?], F](PureMethodParameter(name, tpe, default))
  def typeNamesEqual(a: L#TypeName, b: L#TypeName): Free[F, Boolean] = Free.inject[ScalaTerm[L, ?], F](TypeNamesEqual(a, b))
  def typesEqual(a: L#Type, b: L#Type): Free[F, Boolean]             = Free.inject[ScalaTerm[L, ?], F](TypesEqual(a, b))
  def extractTypeName(tpe: L#Type): Free[F, Option[L#TypeName]]      = Free.inject[ScalaTerm[L, ?], F](ExtractTypeName(tpe))

  def dateType(): Free[F, L#Type]                                        = Free.inject[ScalaTerm[L, ?], F](DateType())
  def dateTimeType(): Free[F, L#Type]                                    = Free.inject[ScalaTerm[L, ?], F](DateTimeType())
  def stringType(format: Option[String]): Free[F, L#Type]                = Free.inject[ScalaTerm[L, ?], F](StringType(format))
  def floatType(): Free[F, L#Type]                                       = Free.inject[ScalaTerm[L, ?], F](FloatType())
  def doubleType(): Free[F, L#Type]                                      = Free.inject[ScalaTerm[L, ?], F](DoubleType())
  def numberType(format: Option[String]): Free[F, L#Type]                = Free.inject[ScalaTerm[L, ?], F](NumberType(format))
  def intType(): Free[F, L#Type]                                         = Free.inject[ScalaTerm[L, ?], F](IntType())
  def longType(): Free[F, L#Type]                                        = Free.inject[ScalaTerm[L, ?], F](LongType())
  def integerType(format: Option[String]): Free[F, L#Type]               = Free.inject[ScalaTerm[L, ?], F](IntegerType(format))
  def booleanType(format: Option[String]): Free[F, L#Type]               = Free.inject[ScalaTerm[L, ?], F](BooleanType(format))
  def arrayType(format: Option[String]): Free[F, L#Type]                 = Free.inject[ScalaTerm[L, ?], F](ArrayType(format))
  def fileType(format: Option[String]): Free[F, L#Type]                  = Free.inject[ScalaTerm[L, ?], F](FileType(format))
  def objectType(format: Option[String]): Free[F, L#Type]                = Free.inject[ScalaTerm[L, ?], F](ObjectType(format))
  def fallbackType(tpe: String, format: Option[String]): Free[F, L#Type] = Free.inject[ScalaTerm[L, ?], F](FallbackType(tpe, format))

  def widenTypeName(tpe: L#TypeName): Free[F, L#Type]       = Free.inject[ScalaTerm[L, ?], F](WidenTypeName(tpe))
  def widenTermSelect(value: L#TermSelect): Free[F, L#Term] = Free.inject[ScalaTerm[L, ?], F](WidenTermSelect(value))

  def renderImplicits(pkgName: List[String],
                      frameworkImports: List[L#Import],
                      jsonImports: List[L#Import],
                      customImports: List[L#Import]): Free[F, L#FileContents] =
    Free.inject[ScalaTerm[L, ?], F](RenderImplicits(pkgName, frameworkImports, jsonImports, customImports))
  def renderFrameworkImplicits(pkgName: List[String],
                               frameworkImports: List[L#Import],
                               jsonImports: List[L#Import],
                               frameworkImplicits: L#ObjectDefinition): Free[F, L#FileContents] =
    Free.inject[ScalaTerm[L, ?], F](RenderFrameworkImplicits(pkgName, frameworkImports, jsonImports, frameworkImplicits))

  def writePackageObject(dtoPackagePath: Path,
                         dtoComponents: List[String],
                         customImports: List[L#Import],
                         packageObjectImports: List[L#Import],
                         protocolImports: List[L#Import],
                         packageObjectContents: List[L#ValueDefinition],
                         extraTypes: List[L#Statement]): Free[F, WriteTree] =
    Free.inject[ScalaTerm[L, ?], F](
      WritePackageObject(dtoPackagePath, dtoComponents, customImports, packageObjectImports, protocolImports, packageObjectContents, extraTypes)
    )
}
object ScalaTerms {
  implicit def scalaTerm[L <: LA, F[_]](implicit I: InjectK[ScalaTerm[L, ?], F]): ScalaTerms[L, F] = new ScalaTerms[L, F]
}
