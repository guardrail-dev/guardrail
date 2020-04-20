package com.twilio.guardrail
package terms

import cats.{ InjectK, Monad }
import cats.arrow.FunctionK
import cats.free.Free
import com.twilio.guardrail.SwaggerUtil.LazyResolvedType
import com.twilio.guardrail.generators.RawParameterType
import com.twilio.guardrail.languages.LA
import java.nio.file.Path

import cats.data.NonEmptyList

abstract class LanguageTerms[L <: LA, F[_]] extends FunctionK[LanguageTerm[L, ?], F] {
  def MonadF: Monad[F]
  def vendorPrefixes(): F[List[String]]

  def litString(value: String): F[L#Term]
  def litFloat(value: Float): F[L#Term]
  def litDouble(value: Double): F[L#Term]
  def litInt(value: Int): F[L#Term]
  def litLong(value: Long): F[L#Term]
  def litBoolean(value: Boolean): F[L#Term]
  def liftOptionalType(value: L#Type): F[L#Type]
  def liftOptionalTerm(value: L#Term): F[L#Term]
  def emptyArray(): F[L#Term]
  def emptyMap(): F[L#Term]
  def emptyOptionalTerm(): F[L#Term]
  def liftVectorType(value: L#Type, customTpe: Option[L#Type]): F[L#Type]
  def liftVectorTerm(value: L#Term): F[L#Term]
  def liftMapType(value: L#Type, customTpe: Option[L#Type]): F[L#Type]

  def fullyQualifyPackageName(rawPkgName: List[String]): F[List[String]]

  def lookupEnumDefaultValue(tpe: L#TypeName, defaultValue: L#Term, values: List[(String, L#TermName, L#TermSelect)]): F[L#TermSelect]
  def formatEnumName(enumValue: String): F[String]

  def embedArray(tpe: LazyResolvedType[L], customTpe: Option[L#Type]): F[LazyResolvedType[L]]
  def embedMap(tpe: LazyResolvedType[L], customTpe: Option[L#Type]): F[LazyResolvedType[L]]

  def parseType(value: String): F[Option[L#Type]]
  def parseTypeName(value: String): F[Option[L#TypeName]]
  def pureTermName(value: String): F[L#TermName]
  def pureTypeName(value: String): F[L#TypeName]

  def pureMethodParameter(name: L#TermName, tpe: L#Type, default: Option[L#Term]): F[L#MethodParameter]
  def typeNamesEqual(a: L#TypeName, b: L#TypeName): F[Boolean]
  def typesEqual(a: L#Type, b: L#Type): F[Boolean]
  def extractTypeName(tpe: L#Type): F[Option[L#TypeName]]
  def extractTermName(term: L#TermName): F[String]
  def selectType(typeNames: NonEmptyList[String]): F[L#Type]
  def selectTerm(termNames: NonEmptyList[String]): F[L#Term]
  def alterMethodParameterName(param: L#MethodParameter, name: L#TermName): F[L#MethodParameter]

  def bytesType(): F[L#Type]
  def uuidType(): F[L#Type]
  def dateType(): F[L#Type]
  def dateTimeType(): F[L#Type]
  def stringType(format: Option[String]): F[L#Type]
  def floatType(): F[L#Type]
  def doubleType(): F[L#Type]
  def numberType(format: Option[String]): F[L#Type]
  def intType(): F[L#Type]
  def longType(): F[L#Type]
  def integerType(format: Option[String]): F[L#Type]
  def booleanType(format: Option[String]): F[L#Type]
  def arrayType(format: Option[String]): F[L#Type]
  def fallbackType(tpe: Option[String], format: Option[String]): F[L#Type]

  def widenTypeName(tpe: L#TypeName): F[L#Type]
  def widenTermSelect(value: L#TermSelect): F[L#Term]
  def widenClassDefinition(value: L#ClassDefinition): F[L#Definition]
  def widenObjectDefinition(value: L#ObjectDefinition): F[L#Definition]

  def findCommonDefaultValue(history: String, a: Option[L#Term], b: Option[L#Term]): F[Option[L#Term]]
  def findCommonRawType(history: String, a: RawParameterType, b: RawParameterType): F[RawParameterType]

  def renderImplicits(
      pkgPath: Path,
      pkgName: List[String],
      frameworkImports: List[L#Import],
      jsonImports: List[L#Import],
      customImports: List[L#Import]
  ): F[Option[WriteTree]]
  def renderFrameworkImplicits(
      pkgPath: Path,
      pkgName: List[String],
      frameworkImports: List[L#Import],
      jsonImports: List[L#Import],
      frameworkImplicits: L#ObjectDefinition,
      frameworkImplicitName: L#TermName
  ): F[WriteTree]
  def renderFrameworkDefinitions(
      pkgPath: Path,
      pkgName: List[String],
      frameworkImports: List[L#Import],
      frameworkDefinitions: L#ClassDefinition,
      frameworkDefinitionsName: L#TermName
  ): F[WriteTree]

  def writePackageObject(
      dtoPackagePath: Path,
      dtoComponents: Option[NonEmptyList[String]],
      customImports: List[L#Import],
      packageObjectImports: List[L#Import],
      protocolImports: List[L#Import],
      packageObjectContents: List[L#ValueDefinition],
      extraTypes: List[L#Statement]
  ): F[Option[WriteTree]]
  def writeProtocolDefinition(
      outputPath: Path,
      pkgName: List[String],
      definitions: List[String],
      dtoComponents: List[String],
      imports: List[L#Import],
      elem: StrictProtocolElems[L]
  ): F[(List[WriteTree], List[L#Statement])]
  def writeClient(
      pkgPath: Path,
      pkgName: List[String],
      customImports: List[L#Import],
      frameworkImplicitName: Option[L#TermName],
      dtoComponents: Option[List[String]],
      client: Client[L]
  ): F[List[WriteTree]]
  def writeServer(
      pkgPath: Path,
      pkgName: List[String],
      customImports: List[L#Import],
      frameworkImplicitName: Option[L#TermName],
      dtoComponents: Option[List[String]],
      server: Server[L]
  ): F[List[WriteTree]]

  def wrapToObject(name: L#TermName, imports: List[L#Import], definitions: List[L#Definition]): F[L#ObjectDefinition]

  def copy(
      newMonadF: Monad[F] = MonadF,
      newVendorPrefixes: () => F[List[String]] = vendorPrefixes _,
      newLitString: String => F[L#Term] = litString _,
      newLitFloat: Float => F[L#Term] = litFloat _,
      newLitDouble: Double => F[L#Term] = litDouble _,
      newLitInt: Int => F[L#Term] = litInt _,
      newLitLong: Long => F[L#Term] = litLong _,
      newLitBoolean: Boolean => F[L#Term] = litBoolean _,
      newLiftOptionalType: L#Type => F[L#Type] = liftOptionalType _,
      newLiftOptionalTerm: L#Term => F[L#Term] = liftOptionalTerm _,
      newEmptyArray: () => F[L#Term] = emptyArray _,
      newEmptyMap: () => F[L#Term] = emptyMap _,
      newEmptyOptionalTerm: () => F[L#Term] = emptyOptionalTerm _,
      newLiftVectorType: (L#Type, Option[L#Type]) => F[L#Type] = liftVectorType _,
      newLiftVectorTerm: L#Term => F[L#Term] = liftVectorTerm _,
      newLiftMapType: (L#Type, Option[L#Type]) => F[L#Type] = liftMapType _,
      newFullyQualifyPackageName: List[String] => F[List[String]] = fullyQualifyPackageName _,
      newLookupEnumDefaultValue: (L#TypeName, L#Term, List[(String, L#TermName, L#TermSelect)]) => F[L#TermSelect] = lookupEnumDefaultValue _,
      newFormatEnumName: String => F[String] = formatEnumName _,
      newEmbedArray: (LazyResolvedType[L], Option[L#Type]) => F[LazyResolvedType[L]] = embedArray _,
      newEmbedMap: (LazyResolvedType[L], Option[L#Type]) => F[LazyResolvedType[L]] = embedMap _,
      newParseType: String => F[Option[L#Type]] = parseType _,
      newParseTypeName: String => F[Option[L#TypeName]] = parseTypeName _,
      newPureTermName: String => F[L#TermName] = pureTermName _,
      newPureTypeName: String => F[L#TypeName] = pureTypeName _,
      newPureMethodParameter: (L#TermName, L#Type, Option[L#Term]) => F[L#MethodParameter] = pureMethodParameter _,
      newTypeNamesEqual: (L#TypeName, L#TypeName) => F[Boolean] = typeNamesEqual _,
      newTypesEqual: (L#Type, L#Type) => F[Boolean] = typesEqual _,
      newExtractTypeName: L#Type => F[Option[L#TypeName]] = extractTypeName _,
      newExtractTermName: L#TermName => F[String] = extractTermName _,
      newSelectType: NonEmptyList[String] => F[L#Type] = selectType _,
      newSelectTerm: NonEmptyList[String] => F[L#Term] = selectTerm _,
      newAlterMethodParameterName: (L#MethodParameter, L#TermName) => F[L#MethodParameter] = alterMethodParameterName _,
      newBytesType: () => F[L#Type] = bytesType _,
      newUuidType: () => F[L#Type] = uuidType _,
      newDateType: () => F[L#Type] = dateType _,
      newDateTimeType: () => F[L#Type] = dateTimeType _,
      newStringType: Option[String] => F[L#Type] = stringType _,
      newFloatType: () => F[L#Type] = floatType _,
      newDoubleType: () => F[L#Type] = doubleType _,
      newNumberType: Option[String] => F[L#Type] = numberType _,
      newIntType: () => F[L#Type] = intType _,
      newLongType: () => F[L#Type] = longType _,
      newIntegerType: Option[String] => F[L#Type] = integerType _,
      newBooleanType: Option[String] => F[L#Type] = booleanType _,
      newArrayType: Option[String] => F[L#Type] = arrayType _,
      newFallbackType: (Option[String], Option[String]) => F[L#Type] = fallbackType _,
      newWidenTypeName: L#TypeName => F[L#Type] = widenTypeName _,
      newWidenTermSelect: L#TermSelect => F[L#Term] = widenTermSelect _,
      newWidenClassDefinition: L#ClassDefinition => F[L#Definition] = widenClassDefinition _,
      newWidenObjectDefinition: L#ObjectDefinition => F[L#Definition] = widenObjectDefinition _,
      newFindCommonDefaultValue: (String, Option[L#Term], Option[L#Term]) => F[Option[L#Term]] = findCommonDefaultValue _,
      newFindCommonRawType: (String, RawParameterType, RawParameterType) => F[RawParameterType] = findCommonRawType _,
      newRenderImplicits: (Path, List[String], List[L#Import], List[L#Import], List[L#Import]) => F[Option[WriteTree]] = renderImplicits _,
      newRenderFrameworkImplicits: (Path, List[String], List[L#Import], List[L#Import], L#ObjectDefinition, L#TermName) => F[WriteTree] =
        renderFrameworkImplicits _,
      newRenderFrameworkDefinitions: (Path, List[String], List[L#Import], L#ClassDefinition, L#TermName) => F[WriteTree] = renderFrameworkDefinitions _,
      newWritePackageObject: (
          Path,
          Option[NonEmptyList[String]],
          List[L#Import],
          List[L#Import],
          List[L#Import],
          List[L#ValueDefinition],
          List[L#Statement]
      ) => F[Option[WriteTree]] = writePackageObject _,
      newWriteProtocolDefinition: (
          Path,
          List[String],
          List[String],
          List[String],
          List[L#Import],
          StrictProtocolElems[L]
      ) => F[(List[WriteTree], List[L#Statement])] = writeProtocolDefinition _,
      newWriteClient: (Path, List[String], List[L#Import], Option[L#TermName], Option[List[String]], Client[L]) => F[List[WriteTree]] = writeClient _,
      newWriteServer: (Path, List[String], List[L#Import], Option[L#TermName], Option[List[String]], Server[L]) => F[List[WriteTree]] = writeServer _,
      newWrapToObject: (L#TermName, List[L#Import], List[L#Definition]) => F[L#ObjectDefinition] = wrapToObject _
  ) = new LanguageTerms[L, F] {
    def MonadF                                                   = newMonadF
    def vendorPrefixes()                                         = newVendorPrefixes()
    def litString(value: String)                                 = newLitString(value)
    def litFloat(value: Float)                                   = newLitFloat(value)
    def litDouble(value: Double)                                 = newLitDouble(value)
    def litInt(value: Int)                                       = newLitInt(value)
    def litLong(value: Long)                                     = newLitLong(value)
    def litBoolean(value: Boolean)                               = newLitBoolean(value)
    def liftOptionalType(value: L#Type)                          = newLiftOptionalType(value)
    def liftOptionalTerm(value: L#Term)                          = newLiftOptionalTerm(value)
    def emptyArray()                                             = newEmptyArray()
    def emptyMap()                                               = newEmptyMap()
    def emptyOptionalTerm()                                      = newEmptyOptionalTerm()
    def liftVectorType(value: L#Type, customTpe: Option[L#Type]) = newLiftVectorType(value, customTpe)
    def liftVectorTerm(value: L#Term)                            = newLiftVectorTerm(value)
    def liftMapType(value: L#Type, customTpe: Option[L#Type])    = newLiftMapType(value, customTpe)
    def fullyQualifyPackageName(rawPkgName: List[String])        = newFullyQualifyPackageName(rawPkgName)
    def lookupEnumDefaultValue(tpe: L#TypeName, defaultValue: L#Term, values: List[(String, L#TermName, L#TermSelect)]) =
      newLookupEnumDefaultValue(tpe, defaultValue, values)
    def formatEnumName(enumValue: String)                                             = newFormatEnumName(enumValue)
    def embedArray(tpe: LazyResolvedType[L], customTpe: Option[L#Type])               = newEmbedArray(tpe, customTpe)
    def embedMap(tpe: LazyResolvedType[L], customTpe: Option[L#Type])                 = newEmbedMap(tpe, customTpe)
    def parseType(value: String)                                                      = newParseType(value)
    def parseTypeName(value: String)                                                  = newParseTypeName(value)
    def pureTermName(value: String)                                                   = newPureTermName(value)
    def pureTypeName(value: String)                                                   = newPureTypeName(value)
    def pureMethodParameter(name: L#TermName, tpe: L#Type, default: Option[L#Term])   = newPureMethodParameter(name, tpe, default)
    def typeNamesEqual(a: L#TypeName, b: L#TypeName)                                  = newTypeNamesEqual(a, b)
    def typesEqual(a: L#Type, b: L#Type)                                              = newTypesEqual(a, b)
    def extractTypeName(tpe: L#Type)                                                  = newExtractTypeName(tpe)
    def extractTermName(term: L#TermName)                                             = newExtractTermName(term)
    def selectType(typeNames: NonEmptyList[String])                                   = newSelectType(typeNames)
    def selectTerm(termNames: NonEmptyList[String])                                   = newSelectTerm(termNames)
    def alterMethodParameterName(param: L#MethodParameter, name: L#TermName)          = newAlterMethodParameterName(param, name)
    def bytesType()                                                                   = newBytesType()
    def uuidType()                                                                    = newUuidType()
    def dateType()                                                                    = newDateType()
    def dateTimeType()                                                                = newDateTimeType()
    def stringType(format: Option[String])                                            = newStringType(format)
    def floatType()                                                                   = newFloatType()
    def doubleType()                                                                  = newDoubleType()
    def numberType(format: Option[String])                                            = newNumberType(format)
    def intType()                                                                     = newIntType()
    def longType()                                                                    = newLongType()
    def integerType(format: Option[String])                                           = newIntegerType(format)
    def booleanType(format: Option[String])                                           = newBooleanType(format)
    def arrayType(format: Option[String])                                             = newArrayType(format)
    def fallbackType(tpe: Option[String], format: Option[String])                     = newFallbackType(tpe, format)
    def widenTypeName(tpe: L#TypeName)                                                = newWidenTypeName(tpe)
    def widenTermSelect(value: L#TermSelect)                                          = newWidenTermSelect(value)
    def widenClassDefinition(value: L#ClassDefinition)                                = newWidenClassDefinition(value)
    def widenObjectDefinition(value: L#ObjectDefinition)                              = newWidenObjectDefinition(value)
    def findCommonDefaultValue(history: String, a: Option[L#Term], b: Option[L#Term]) = newFindCommonDefaultValue(history, a, b)
    def findCommonRawType(history: String, a: RawParameterType, b: RawParameterType)  = newFindCommonRawType(history, a, b)
    def renderImplicits(pkgPath: Path, pkgName: List[String], frameworkImports: List[L#Import], jsonImports: List[L#Import], customImports: List[L#Import]) =
      newRenderImplicits(pkgPath, pkgName, frameworkImports, jsonImports, customImports)
    def renderFrameworkImplicits(
        pkgPath: Path,
        pkgName: List[String],
        frameworkImports: List[L#Import],
        jsonImports: List[L#Import],
        frameworkImplicits: L#ObjectDefinition,
        frameworkImplicitName: L#TermName
    ) = newRenderFrameworkImplicits(pkgPath, pkgName, frameworkImports, jsonImports, frameworkImplicits, frameworkImplicitName)
    def renderFrameworkDefinitions(
        pkgPath: Path,
        pkgName: List[String],
        frameworkImports: List[L#Import],
        frameworkDefinitions: L#ClassDefinition,
        frameworkDefinitionsName: L#TermName
    ) = newRenderFrameworkDefinitions(pkgPath, pkgName, frameworkImports, frameworkDefinitions, frameworkDefinitionsName)
    def writePackageObject(
        dtoPackagePath: Path,
        dtoComponents: Option[NonEmptyList[String]],
        customImports: List[L#Import],
        packageObjectImports: List[L#Import],
        protocolImports: List[L#Import],
        packageObjectContents: List[L#ValueDefinition],
        extraTypes: List[L#Statement]
    ) = newWritePackageObject(dtoPackagePath, dtoComponents, customImports, packageObjectImports, protocolImports, packageObjectContents, extraTypes)
    def writeProtocolDefinition(
        outputPath: Path,
        pkgName: List[String],
        definitions: List[String],
        dtoComponents: List[String],
        imports: List[L#Import],
        elem: StrictProtocolElems[L]
    ) = newWriteProtocolDefinition(outputPath, pkgName, definitions, dtoComponents, imports, elem)
    def writeClient(
        pkgPath: Path,
        pkgName: List[String],
        customImports: List[L#Import],
        frameworkImplicitName: Option[L#TermName],
        dtoComponents: Option[List[String]],
        client: Client[L]
    ) = newWriteClient(pkgPath, pkgName, customImports, frameworkImplicitName, dtoComponents, client)
    def writeServer(
        pkgPath: Path,
        pkgName: List[String],
        customImports: List[L#Import],
        frameworkImplicitName: Option[L#TermName],
        dtoComponents: Option[List[String]],
        server: Server[L]
    )                                                                                            = newWriteServer(pkgPath, pkgName, customImports, frameworkImplicitName, dtoComponents, server)
    def wrapToObject(name: L#TermName, imports: List[L#Import], definitions: List[L#Definition]) = newWrapToObject(name, imports, definitions)
  }

  def apply[T](term: LanguageTerm[L, T]): F[T] = term match {
    case VendorPrefixes() => vendorPrefixes()

    case LitString(value)                                  => litString(value)
    case LitFloat(value)                                   => litFloat(value)
    case LitDouble(value)                                  => litDouble(value)
    case LitInt(value)                                     => litInt(value)
    case LitLong(value)                                    => litLong(value)
    case LitBoolean(value)                                 => litBoolean(value)
    case LiftOptionalType(value)                           => liftOptionalType(value)
    case LiftOptionalTerm(value)                           => liftOptionalTerm(value)
    case EmptyOptionalTerm()                               => emptyOptionalTerm()
    case EmptyArray()                                      => emptyArray()
    case EmptyMap()                                        => emptyMap()
    case LiftVectorType(value, customTpe)                  => liftVectorType(value, customTpe)
    case LiftVectorTerm(value)                             => liftVectorTerm(value)
    case LiftMapType(value, customTpe)                     => liftMapType(value, customTpe)
    case FullyQualifyPackageName(rawPkgName)               => fullyQualifyPackageName(rawPkgName)
    case LookupEnumDefaultValue(tpe, defaultValue, values) => lookupEnumDefaultValue(tpe, defaultValue, values)
    case FormatEnumName(enumValue)                         => formatEnumName(enumValue)
    case EmbedArray(tpe, customTpe) =>
      embedArray(tpe, customTpe)
    case EmbedMap(tpe, customTpe) =>
      embedMap(tpe, customTpe)
    case ParseType(value) =>
      parseType(value)
    case ParseTypeName(value) =>
      parseTypeName(value)

    case PureTermName(value) =>
      pureTermName(value)

    case PureTypeName(value) =>
      pureTypeName(value)

    case PureMethodParameter(name, tpe, default) =>
      pureMethodParameter(name, tpe, default)

    case TypeNamesEqual(a, b) =>
      typeNamesEqual(a, b)

    case TypesEqual(a, b) =>
      typesEqual(a, b)

    case ExtractTypeName(tpe) =>
      extractTypeName(tpe)
    case ExtractTermName(term) =>
      extractTermName(term)
    case SelectType(typeNames) =>
      selectType(typeNames)
    case SelectTerm(termNames) =>
      selectTerm(termNames)
    case AlterMethodParameterName(param, name) =>
      alterMethodParameterName(param, name)

    case BytesType()               => bytesType()
    case DateType()                => dateType()
    case DateTimeType()            => dateTimeType()
    case UUIDType()                => uuidType()
    case StringType(format)        => stringType(format)
    case FloatType()               => floatType()
    case DoubleType()              => doubleType()
    case NumberType(format)        => numberType(format)
    case IntType()                 => intType()
    case LongType()                => longType()
    case IntegerType(format)       => integerType(format)
    case BooleanType(format)       => booleanType(format)
    case ArrayType(format)         => arrayType(format)
    case FallbackType(tpe, format) => fallbackType(tpe, format)

    case WidenTypeName(tpe)           => widenTypeName(tpe)
    case WidenTermSelect(value)       => widenTermSelect(value)
    case WidenClassDefinition(value)  => widenClassDefinition(value)
    case WidenObjectDefinition(value) => widenObjectDefinition(value)

    case FindCommonDefaultValue(history, a, b) =>
      findCommonDefaultValue(history, a, b)
    case FindCommonRawType(history, a, b) =>
      findCommonRawType(history, a, b)

    case RenderImplicits(pkgPath, pkgName, frameworkImports, jsonImports, customImports) =>
      renderImplicits(pkgPath, pkgName, frameworkImports, jsonImports, customImports)
    case RenderFrameworkImplicits(pkgPath, pkgName, frameworkImports, jsonImports, frameworkImplicits, frameworkImplicitName) =>
      renderFrameworkImplicits(pkgPath, pkgName, frameworkImports, jsonImports, frameworkImplicits, frameworkImplicitName)
    case RenderFrameworkDefinitions(pkgPath, pkgName, frameworkImports, frameworkDefinitions, frameworkDefinitionsName) =>
      renderFrameworkDefinitions(pkgPath, pkgName, frameworkImports, frameworkDefinitions, frameworkDefinitionsName)
    case WritePackageObject(dtoPackagePath, dtoComponents, customImports, packageObjectImports, protocolImports, packageObjectContents, extraTypes) =>
      writePackageObject(dtoPackagePath, dtoComponents, customImports, packageObjectImports, protocolImports, packageObjectContents, extraTypes)
    case WriteProtocolDefinition(outputPath, pkgName, definitions, dtoComponents, imports, elem) =>
      writeProtocolDefinition(outputPath, pkgName, definitions, dtoComponents, imports, elem)
    case WriteClient(pkgPath, pkgName, customImports, frameworkImplicitName, dtoComponents, client) =>
      writeClient(pkgPath, pkgName, customImports, frameworkImplicitName, dtoComponents, client)
    case WriteServer(pkgPath, pkgName, customImports, frameworkImplicitName, dtoComponents, server) =>
      writeServer(pkgPath, pkgName, customImports, frameworkImplicitName, dtoComponents, server)
    case WrapToObject(name, imports, definitions) =>
      wrapToObject(name, imports, definitions)
  }
}

object LanguageTerms {
  implicit def languageTerm[L <: LA, F[_]](implicit I: InjectK[LanguageTerm[L, ?], F]): LanguageTerms[L, Free[F, ?]] = new LanguageTerms[L, Free[F, ?]] {
    def MonadF                                  = Free.catsFreeMonadForFree
    def vendorPrefixes(): Free[F, List[String]] = Free.inject[LanguageTerm[L, ?], F](VendorPrefixes[L]())

    def litString(value: String): Free[F, L#Term]                                 = Free.inject[LanguageTerm[L, ?], F](LitString(value))
    def litFloat(value: Float): Free[F, L#Term]                                   = Free.inject[LanguageTerm[L, ?], F](LitFloat(value))
    def litDouble(value: Double): Free[F, L#Term]                                 = Free.inject[LanguageTerm[L, ?], F](LitDouble(value))
    def litInt(value: Int): Free[F, L#Term]                                       = Free.inject[LanguageTerm[L, ?], F](LitInt(value))
    def litLong(value: Long): Free[F, L#Term]                                     = Free.inject[LanguageTerm[L, ?], F](LitLong(value))
    def litBoolean(value: Boolean): Free[F, L#Term]                               = Free.inject[LanguageTerm[L, ?], F](LitBoolean(value))
    def liftOptionalType(value: L#Type): Free[F, L#Type]                          = Free.inject[LanguageTerm[L, ?], F](LiftOptionalType(value))
    def liftOptionalTerm(value: L#Term): Free[F, L#Term]                          = Free.inject[LanguageTerm[L, ?], F](LiftOptionalTerm(value))
    def emptyArray(): Free[F, L#Term]                                             = Free.inject[LanguageTerm[L, ?], F](EmptyArray())
    def emptyMap(): Free[F, L#Term]                                               = Free.inject[LanguageTerm[L, ?], F](EmptyMap())
    def emptyOptionalTerm(): Free[F, L#Term]                                      = Free.inject[LanguageTerm[L, ?], F](EmptyOptionalTerm())
    def liftVectorType(value: L#Type, customTpe: Option[L#Type]): Free[F, L#Type] = Free.inject[LanguageTerm[L, ?], F](LiftVectorType(value, customTpe))
    def liftVectorTerm(value: L#Term): Free[F, L#Term]                            = Free.inject[LanguageTerm[L, ?], F](LiftVectorTerm(value))
    def liftMapType(value: L#Type, customTpe: Option[L#Type]): Free[F, L#Type]    = Free.inject[LanguageTerm[L, ?], F](LiftMapType(value, customTpe))

    def fullyQualifyPackageName(rawPkgName: List[String]): Free[F, List[String]] =
      Free.inject[LanguageTerm[L, ?], F](FullyQualifyPackageName(rawPkgName))

    def lookupEnumDefaultValue(tpe: L#TypeName, defaultValue: L#Term, values: List[(String, L#TermName, L#TermSelect)]): Free[F, L#TermSelect] =
      Free.inject[LanguageTerm[L, ?], F](LookupEnumDefaultValue(tpe, defaultValue, values))
    def formatEnumName(enumValue: String): Free[F, String] = Free.inject[LanguageTerm[L, ?], F](FormatEnumName(enumValue))

    def embedArray(tpe: LazyResolvedType[L], customTpe: Option[L#Type]): Free[F, LazyResolvedType[L]] =
      Free.inject[LanguageTerm[L, ?], F](EmbedArray(tpe, customTpe))
    def embedMap(tpe: LazyResolvedType[L], customTpe: Option[L#Type]): Free[F, LazyResolvedType[L]] =
      Free.inject[LanguageTerm[L, ?], F](EmbedMap(tpe, customTpe))

    def parseType(value: String): Free[F, Option[L#Type]]         = Free.inject[LanguageTerm[L, ?], F](ParseType(value))
    def parseTypeName(value: String): Free[F, Option[L#TypeName]] = Free.inject[LanguageTerm[L, ?], F](ParseTypeName(value))
    def pureTermName(value: String): Free[F, L#TermName]          = Free.inject[LanguageTerm[L, ?], F](PureTermName(value))
    def pureTypeName(value: String): Free[F, L#TypeName]          = Free.inject[LanguageTerm[L, ?], F](PureTypeName(value))

    def pureMethodParameter(name: L#TermName, tpe: L#Type, default: Option[L#Term]): Free[F, L#MethodParameter] =
      Free.inject[LanguageTerm[L, ?], F](PureMethodParameter(name, tpe, default))
    def typeNamesEqual(a: L#TypeName, b: L#TypeName): Free[F, Boolean] = Free.inject[LanguageTerm[L, ?], F](TypeNamesEqual(a, b))
    def typesEqual(a: L#Type, b: L#Type): Free[F, Boolean]             = Free.inject[LanguageTerm[L, ?], F](TypesEqual(a, b))
    def extractTypeName(tpe: L#Type): Free[F, Option[L#TypeName]]      = Free.inject[LanguageTerm[L, ?], F](ExtractTypeName(tpe))
    def extractTermName(term: L#TermName): Free[F, String]             = Free.inject[LanguageTerm[L, ?], F](ExtractTermName(term))
    def selectType(typeNames: NonEmptyList[String]): Free[F, L#Type]   = Free.inject[LanguageTerm[L, ?], F](SelectType(typeNames))
    def selectTerm(termNames: NonEmptyList[String]): Free[F, L#Term]   = Free.inject[LanguageTerm[L, ?], F](SelectTerm(termNames))
    def alterMethodParameterName(param: L#MethodParameter, name: L#TermName): Free[F, L#MethodParameter] =
      Free.inject[LanguageTerm[L, ?], F](AlterMethodParameterName(param, name))

    def bytesType(): Free[F, L#Type]                                               = Free.inject[LanguageTerm[L, ?], F](BytesType())
    def uuidType(): Free[F, L#Type]                                                = Free.inject[LanguageTerm[L, ?], F](UUIDType())
    def dateType(): Free[F, L#Type]                                                = Free.inject[LanguageTerm[L, ?], F](DateType())
    def dateTimeType(): Free[F, L#Type]                                            = Free.inject[LanguageTerm[L, ?], F](DateTimeType())
    def stringType(format: Option[String]): Free[F, L#Type]                        = Free.inject[LanguageTerm[L, ?], F](StringType(format))
    def floatType(): Free[F, L#Type]                                               = Free.inject[LanguageTerm[L, ?], F](FloatType())
    def doubleType(): Free[F, L#Type]                                              = Free.inject[LanguageTerm[L, ?], F](DoubleType())
    def numberType(format: Option[String]): Free[F, L#Type]                        = Free.inject[LanguageTerm[L, ?], F](NumberType(format))
    def intType(): Free[F, L#Type]                                                 = Free.inject[LanguageTerm[L, ?], F](IntType())
    def longType(): Free[F, L#Type]                                                = Free.inject[LanguageTerm[L, ?], F](LongType())
    def integerType(format: Option[String]): Free[F, L#Type]                       = Free.inject[LanguageTerm[L, ?], F](IntegerType(format))
    def booleanType(format: Option[String]): Free[F, L#Type]                       = Free.inject[LanguageTerm[L, ?], F](BooleanType(format))
    def arrayType(format: Option[String]): Free[F, L#Type]                         = Free.inject[LanguageTerm[L, ?], F](ArrayType(format))
    def fallbackType(tpe: Option[String], format: Option[String]): Free[F, L#Type] = Free.inject[LanguageTerm[L, ?], F](FallbackType(tpe, format))

    def widenTypeName(tpe: L#TypeName): Free[F, L#Type]                         = Free.inject[LanguageTerm[L, ?], F](WidenTypeName(tpe))
    def widenTermSelect(value: L#TermSelect): Free[F, L#Term]                   = Free.inject[LanguageTerm[L, ?], F](WidenTermSelect(value))
    def widenClassDefinition(value: L#ClassDefinition): Free[F, L#Definition]   = Free.inject[LanguageTerm[L, ?], F](WidenClassDefinition(value))
    def widenObjectDefinition(value: L#ObjectDefinition): Free[F, L#Definition] = Free.inject[LanguageTerm[L, ?], F](WidenObjectDefinition(value))

    def findCommonDefaultValue(history: String, a: Option[L#Term], b: Option[L#Term]): Free[F, Option[L#Term]] =
      Free.inject[LanguageTerm[L, ?], F](FindCommonDefaultValue(history, a, b))
    def findCommonRawType(history: String, a: RawParameterType, b: RawParameterType): Free[F, RawParameterType] =
      Free.inject[LanguageTerm[L, ?], F](FindCommonRawType(history, a, b))

    def renderImplicits(
        pkgPath: Path,
        pkgName: List[String],
        frameworkImports: List[L#Import],
        jsonImports: List[L#Import],
        customImports: List[L#Import]
    ): Free[F, Option[WriteTree]] =
      Free.inject[LanguageTerm[L, ?], F](RenderImplicits(pkgPath, pkgName, frameworkImports, jsonImports, customImports))
    def renderFrameworkImplicits(
        pkgPath: Path,
        pkgName: List[String],
        frameworkImports: List[L#Import],
        jsonImports: List[L#Import],
        frameworkImplicits: L#ObjectDefinition,
        frameworkImplicitName: L#TermName
    ): Free[F, WriteTree] =
      Free.inject[LanguageTerm[L, ?], F](RenderFrameworkImplicits(pkgPath, pkgName, frameworkImports, jsonImports, frameworkImplicits, frameworkImplicitName))
    def renderFrameworkDefinitions(
        pkgPath: Path,
        pkgName: List[String],
        frameworkImports: List[L#Import],
        frameworkDefinitions: L#ClassDefinition,
        frameworkDefinitionsName: L#TermName
    ): Free[F, WriteTree] =
      Free.inject[LanguageTerm[L, ?], F](RenderFrameworkDefinitions(pkgPath, pkgName, frameworkImports, frameworkDefinitions, frameworkDefinitionsName))

    def writePackageObject(
        dtoPackagePath: Path,
        dtoComponents: Option[NonEmptyList[String]],
        customImports: List[L#Import],
        packageObjectImports: List[L#Import],
        protocolImports: List[L#Import],
        packageObjectContents: List[L#ValueDefinition],
        extraTypes: List[L#Statement]
    ): Free[F, Option[WriteTree]] =
      Free.inject[LanguageTerm[L, ?], F](
        WritePackageObject(dtoPackagePath, dtoComponents, customImports, packageObjectImports, protocolImports, packageObjectContents, extraTypes)
      )
    def writeProtocolDefinition(
        outputPath: Path,
        pkgName: List[String],
        definitions: List[String],
        dtoComponents: List[String],
        imports: List[L#Import],
        elem: StrictProtocolElems[L]
    ): Free[F, (List[WriteTree], List[L#Statement])] =
      Free.inject[LanguageTerm[L, ?], F](WriteProtocolDefinition(outputPath, pkgName, definitions, dtoComponents, imports, elem))
    def writeClient(
        pkgPath: Path,
        pkgName: List[String],
        customImports: List[L#Import],
        frameworkImplicitName: Option[L#TermName],
        dtoComponents: Option[List[String]],
        client: Client[L]
    ): Free[F, List[WriteTree]] =
      Free.inject[LanguageTerm[L, ?], F](WriteClient(pkgPath, pkgName, customImports, frameworkImplicitName, dtoComponents, client))
    def writeServer(
        pkgPath: Path,
        pkgName: List[String],
        customImports: List[L#Import],
        frameworkImplicitName: Option[L#TermName],
        dtoComponents: Option[List[String]],
        server: Server[L]
    ): Free[F, List[WriteTree]] =
      Free.inject[LanguageTerm[L, ?], F](WriteServer(pkgPath, pkgName, customImports, frameworkImplicitName, dtoComponents, server))

    def wrapToObject(name: L#TermName, imports: List[L#Import], definitions: List[L#Definition]): Free[F, L#ObjectDefinition] =
      Free.inject[LanguageTerm[L, ?], F](WrapToObject(name, imports, definitions))
  }
}
