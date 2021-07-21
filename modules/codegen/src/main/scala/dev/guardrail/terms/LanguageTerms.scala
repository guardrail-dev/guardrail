package dev.guardrail
package terms

import cats.Monad
import cats.data.NonEmptyList
import dev.guardrail.core.Tracker
import dev.guardrail.generators.RawParameterType
import dev.guardrail.languages.LA
import java.nio.file.Path

abstract class LanguageTerms[L <: LA, F[_]] {
  def MonadF: Monad[F]

  def litString(value: String): F[L#Term]
  def litFloat(value: Float): F[L#Term]
  def litDouble(value: Double): F[L#Term]
  def litInt(value: Int): F[L#Term]
  def litLong(value: Long): F[L#Term]
  def litBoolean(value: Boolean): F[L#Term]

  def fullyQualifyPackageName(rawPkgName: NonEmptyList[String]): F[NonEmptyList[String]]

  def lookupEnumDefaultValue(tpe: L#TypeName, defaultValue: L#Term, values: RenderedEnum[L]): F[L#TermSelect]

  def formatPackageName(packageName: List[String]): F[NonEmptyList[String]]
  def formatTypeName(typeName: String, suffix: Option[String] = None): F[String]
  def formatFieldName(fieldName: String): F[String]
  def formatMethodName(methodName: String): F[String]
  def formatMethodArgName(methodArgName: String): F[String]
  def formatEnumName(enumValue: String): F[String]

  def parseType(value: Tracker[String]): F[Option[L#Type]]
  def parseTypeName(value: String): F[Option[L#TypeName]]
  def pureTermName(value: String): F[L#TermName]
  def pureTypeName(value: String): F[L#TypeName]

  def pureMethodParameter(name: L#TermName, tpe: L#Type, default: Option[L#Term]): F[L#MethodParameter]
  def typeNamesEqual(a: L#TypeName, b: L#TypeName): F[Boolean]
  def typesEqual(a: L#Type, b: L#Type): F[Boolean]
  def extractTypeName(tpe: L#Type): F[Option[L#TypeName]]
  def extractTermName(term: L#TermName): F[String]
  def extractTermNameFromParam(param: L#MethodParameter): F[String]
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
  def fallbackType(tpe: Option[String], format: Option[String]): F[L#Type]

  def widenTypeName(tpe: L#TypeName): F[L#Type]
  def widenTermSelect(value: L#TermSelect): F[L#Term]
  def widenClassDefinition(value: L#ClassDefinition): F[L#Definition]
  def widenObjectDefinition(value: L#ObjectDefinition): F[L#Definition]

  def findCommonDefaultValue(history: String, a: Option[L#Term], b: Option[L#Term]): F[Option[L#Term]]
  def findCommonRawType(history: String, a: RawParameterType, b: RawParameterType): F[RawParameterType]

  def renderImplicits(
      pkgPath: Path,
      pkgName: NonEmptyList[String],
      frameworkImports: List[L#Import],
      jsonImports: List[L#Import],
      customImports: List[L#Import]
  ): F[Option[WriteTree]]
  def renderFrameworkImplicits(
      pkgPath: Path,
      pkgName: NonEmptyList[String],
      frameworkImports: List[L#Import],
      frameworkImplicitImportNames: List[L#TermName],
      jsonImports: List[L#Import],
      frameworkImplicits: L#ObjectDefinition,
      frameworkImplicitName: L#TermName
  ): F[WriteTree]
  def renderFrameworkDefinitions(
      pkgPath: Path,
      pkgName: NonEmptyList[String],
      frameworkImports: List[L#Import],
      frameworkDefinitions: List[L#Definition],
      frameworkDefinitionsName: L#TermName
  ): F[WriteTree]

  def writePackageObject(
      dtoPackagePath: Path,
      pkgComponents: NonEmptyList[String],
      dtoComponents: Option[NonEmptyList[String]],
      customImports: List[L#Import],
      packageObjectImports: List[L#Import],
      protocolImports: List[L#Import],
      packageObjectContents: List[L#Statement],
      extraTypes: List[L#Statement]
  ): F[Option[WriteTree]]
  def writeProtocolDefinition(
      outputPath: Path,
      pkgName: NonEmptyList[String],
      definitions: List[String],
      dtoComponents: NonEmptyList[String],
      imports: List[L#Import],
      protoImplicitName: Option[L#TermName],
      elem: StrictProtocolElems[L]
  ): F[(List[WriteTree], List[L#Statement])]
  def writeClient(
      pkgPath: Path,
      pkgName: NonEmptyList[String],
      customImports: List[L#Import],
      frameworkImplicitNames: List[L#TermName],
      dtoComponents: Option[NonEmptyList[String]],
      client: Client[L]
  ): F[List[WriteTree]]
  def writeServer(
      pkgPath: Path,
      pkgName: NonEmptyList[String],
      customImports: List[L#Import],
      frameworkImplicitNames: List[L#TermName],
      dtoComponents: Option[NonEmptyList[String]],
      server: Server[L]
  ): F[List[WriteTree]]

  def wrapToObject(name: L#TermName, imports: List[L#Import], definitions: List[L#Definition]): F[Option[L#ObjectDefinition]]

  def copy(
      newMonadF: Monad[F] = MonadF,
      newLitString: String => F[L#Term] = litString _,
      newLitFloat: Float => F[L#Term] = litFloat _,
      newLitDouble: Double => F[L#Term] = litDouble _,
      newLitInt: Int => F[L#Term] = litInt _,
      newLitLong: Long => F[L#Term] = litLong _,
      newLitBoolean: Boolean => F[L#Term] = litBoolean _,
      newFullyQualifyPackageName: NonEmptyList[String] => F[NonEmptyList[String]] = fullyQualifyPackageName _,
      newLookupEnumDefaultValue: (L#TypeName, L#Term, RenderedEnum[L]) => F[L#TermSelect] = lookupEnumDefaultValue _,
      newFormatPackageName: List[String] => F[NonEmptyList[String]] = formatPackageName _,
      newFormatTypeName: (String, Option[String]) => F[String] = formatTypeName _,
      newFormatFieldName: String => F[String] = formatFieldName _,
      newFormatMethodName: String => F[String] = formatMethodName _,
      newFormatMethodArgName: String => F[String] = formatMethodArgName _,
      newFormatEnumName: String => F[String] = formatEnumName _,
      newParseType: Tracker[String] => F[Option[L#Type]] = parseType _,
      newParseTypeName: String => F[Option[L#TypeName]] = parseTypeName _,
      newPureTermName: String => F[L#TermName] = pureTermName _,
      newPureTypeName: String => F[L#TypeName] = pureTypeName _,
      newPureMethodParameter: (L#TermName, L#Type, Option[L#Term]) => F[L#MethodParameter] = pureMethodParameter _,
      newTypeNamesEqual: (L#TypeName, L#TypeName) => F[Boolean] = typeNamesEqual _,
      newTypesEqual: (L#Type, L#Type) => F[Boolean] = typesEqual _,
      newExtractTypeName: L#Type => F[Option[L#TypeName]] = extractTypeName _,
      newExtractTermName: L#TermName => F[String] = extractTermName _,
      newExtractTermNameFromParam: L#MethodParameter => F[String] = extractTermNameFromParam _,
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
      newFallbackType: (Option[String], Option[String]) => F[L#Type] = fallbackType _,
      newWidenTypeName: L#TypeName => F[L#Type] = widenTypeName _,
      newWidenTermSelect: L#TermSelect => F[L#Term] = widenTermSelect _,
      newWidenClassDefinition: L#ClassDefinition => F[L#Definition] = widenClassDefinition _,
      newWidenObjectDefinition: L#ObjectDefinition => F[L#Definition] = widenObjectDefinition _,
      newFindCommonDefaultValue: (String, Option[L#Term], Option[L#Term]) => F[Option[L#Term]] = findCommonDefaultValue _,
      newFindCommonRawType: (String, RawParameterType, RawParameterType) => F[RawParameterType] = findCommonRawType _,
      newRenderImplicits: (Path, NonEmptyList[String], List[L#Import], List[L#Import], List[L#Import]) => F[Option[WriteTree]] = renderImplicits _,
      newRenderFrameworkImplicits: (
          Path,
          NonEmptyList[String],
          List[L#Import],
          List[L#TermName],
          List[L#Import],
          L#ObjectDefinition,
          L#TermName
      ) => F[WriteTree] = renderFrameworkImplicits _,
      newRenderFrameworkDefinitions: (Path, NonEmptyList[String], List[L#Import], List[L#Definition], L#TermName) => F[WriteTree] =
        renderFrameworkDefinitions _,
      newWritePackageObject: (
          Path,
          NonEmptyList[String],
          Option[NonEmptyList[String]],
          List[L#Import],
          List[L#Import],
          List[L#Import],
          List[L#Statement],
          List[L#Statement]
      ) => F[Option[WriteTree]] = writePackageObject _,
      newWriteProtocolDefinition: (
          Path,
          NonEmptyList[String],
          List[String],
          NonEmptyList[String],
          List[L#Import],
          Option[L#TermName],
          StrictProtocolElems[L]
      ) => F[(List[WriteTree], List[L#Statement])] = writeProtocolDefinition _,
      newWriteClient: (Path, NonEmptyList[String], List[L#Import], List[L#TermName], Option[NonEmptyList[String]], Client[L]) => F[List[WriteTree]] =
        writeClient _,
      newWriteServer: (Path, NonEmptyList[String], List[L#Import], List[L#TermName], Option[NonEmptyList[String]], Server[L]) => F[List[WriteTree]] =
        writeServer _,
      newWrapToObject: (L#TermName, List[L#Import], List[L#Definition]) => F[Option[L#ObjectDefinition]] = wrapToObject _
  ) = new LanguageTerms[L, F] {
    def MonadF                                                    = newMonadF
    def litString(value: String)                                  = newLitString(value)
    def litFloat(value: Float)                                    = newLitFloat(value)
    def litDouble(value: Double)                                  = newLitDouble(value)
    def litInt(value: Int)                                        = newLitInt(value)
    def litLong(value: Long)                                      = newLitLong(value)
    def litBoolean(value: Boolean)                                = newLitBoolean(value)
    def fullyQualifyPackageName(rawPkgName: NonEmptyList[String]) = newFullyQualifyPackageName(rawPkgName)
    def lookupEnumDefaultValue(tpe: L#TypeName, defaultValue: L#Term, values: RenderedEnum[L]) =
      newLookupEnumDefaultValue(tpe, defaultValue, values)
    def formatPackageName(packageName: List[String]): F[NonEmptyList[String]]         = newFormatPackageName(packageName)
    def formatTypeName(typeName: String, suffix: Option[String] = None): F[String]    = newFormatTypeName(typeName, suffix)
    def formatFieldName(fieldName: String): F[String]                                 = newFormatFieldName(fieldName)
    def formatMethodName(methodName: String): F[String]                               = newFormatMethodName(methodName)
    def formatMethodArgName(methodArgName: String): F[String]                         = newFormatMethodArgName(methodArgName)
    def formatEnumName(enumValue: String)                                             = newFormatEnumName(enumValue)
    def parseType(value: Tracker[String])                                             = newParseType(value)
    def parseTypeName(value: String)                                                  = newParseTypeName(value)
    def pureTermName(value: String)                                                   = newPureTermName(value)
    def pureTypeName(value: String)                                                   = newPureTypeName(value)
    def pureMethodParameter(name: L#TermName, tpe: L#Type, default: Option[L#Term])   = newPureMethodParameter(name, tpe, default)
    def typeNamesEqual(a: L#TypeName, b: L#TypeName)                                  = newTypeNamesEqual(a, b)
    def typesEqual(a: L#Type, b: L#Type)                                              = newTypesEqual(a, b)
    def extractTypeName(tpe: L#Type)                                                  = newExtractTypeName(tpe)
    def extractTermName(term: L#TermName)                                             = newExtractTermName(term)
    def extractTermNameFromParam(param: L#MethodParameter)                            = newExtractTermNameFromParam(param)
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
    def fallbackType(tpe: Option[String], format: Option[String])                     = newFallbackType(tpe, format)
    def widenTypeName(tpe: L#TypeName)                                                = newWidenTypeName(tpe)
    def widenTermSelect(value: L#TermSelect)                                          = newWidenTermSelect(value)
    def widenClassDefinition(value: L#ClassDefinition)                                = newWidenClassDefinition(value)
    def widenObjectDefinition(value: L#ObjectDefinition)                              = newWidenObjectDefinition(value)
    def findCommonDefaultValue(history: String, a: Option[L#Term], b: Option[L#Term]) = newFindCommonDefaultValue(history, a, b)
    def findCommonRawType(history: String, a: RawParameterType, b: RawParameterType)  = newFindCommonRawType(history, a, b)
    def renderImplicits(
        pkgPath: Path,
        pkgName: NonEmptyList[String],
        frameworkImports: List[L#Import],
        jsonImports: List[L#Import],
        customImports: List[L#Import]
    ) =
      newRenderImplicits(pkgPath, pkgName, frameworkImports, jsonImports, customImports)
    def renderFrameworkImplicits(
        pkgPath: Path,
        pkgName: NonEmptyList[String],
        frameworkImports: List[L#Import],
        frameworkImplicitImportNames: List[L#TermName],
        jsonImports: List[L#Import],
        frameworkImplicits: L#ObjectDefinition,
        frameworkImplicitName: L#TermName
    ) = newRenderFrameworkImplicits(pkgPath, pkgName, frameworkImports, frameworkImplicitImportNames, jsonImports, frameworkImplicits, frameworkImplicitName)
    def renderFrameworkDefinitions(
        pkgPath: Path,
        pkgName: NonEmptyList[String],
        frameworkImports: List[L#Import],
        frameworkDefinitions: List[L#Definition],
        frameworkDefinitionsName: L#TermName
    ) = newRenderFrameworkDefinitions(pkgPath, pkgName, frameworkImports, frameworkDefinitions, frameworkDefinitionsName)
    def writePackageObject(
        dtoPackagePath: Path,
        pkgComponents: NonEmptyList[String],
        dtoComponents: Option[NonEmptyList[String]],
        customImports: List[L#Import],
        packageObjectImports: List[L#Import],
        protocolImports: List[L#Import],
        packageObjectContents: List[L#Statement],
        extraTypes: List[L#Statement]
    ) =
      newWritePackageObject(
        dtoPackagePath,
        pkgComponents,
        dtoComponents,
        customImports,
        packageObjectImports,
        protocolImports,
        packageObjectContents,
        extraTypes
      )
    def writeProtocolDefinition(
        outputPath: Path,
        pkgName: NonEmptyList[String],
        definitions: List[String],
        dtoComponents: NonEmptyList[String],
        imports: List[L#Import],
        protoImplicitName: Option[L#TermName],
        elem: StrictProtocolElems[L]
    ) = newWriteProtocolDefinition(outputPath, pkgName, definitions, dtoComponents, imports, protoImplicitName, elem)
    def writeClient(
        pkgPath: Path,
        pkgName: NonEmptyList[String],
        customImports: List[L#Import],
        frameworkImplicitNames: List[L#TermName],
        dtoComponents: Option[NonEmptyList[String]],
        client: Client[L]
    ) = newWriteClient(pkgPath, pkgName, customImports, frameworkImplicitNames, dtoComponents, client)
    def writeServer(
        pkgPath: Path,
        pkgName: NonEmptyList[String],
        customImports: List[L#Import],
        frameworkImplicitNames: List[L#TermName],
        dtoComponents: Option[NonEmptyList[String]],
        server: Server[L]
    )                                                                                            = newWriteServer(pkgPath, pkgName, customImports, frameworkImplicitNames, dtoComponents, server)
    def wrapToObject(name: L#TermName, imports: List[L#Import], definitions: List[L#Definition]) = newWrapToObject(name, imports, definitions)
  }
}
