package com.twilio.guardrail
package terms

import cats.Monad
import com.twilio.guardrail.SwaggerUtil.LazyResolvedType
import com.twilio.guardrail.generators.RawParameterType
import com.twilio.guardrail.languages.LA
import java.nio.file.Path

import cats.data.NonEmptyList

abstract class LanguageTerms[L <: LA, F[_]] {
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
}
