package dev.guardrail.terms

import cats.data.NonEmptyList
import java.nio.file.Path

import dev.guardrail._
import dev.guardrail.core.{ ReifiedRawType, Tracker }
import dev.guardrail.generators.{ Client, Server }
import dev.guardrail.languages.LA
import dev.guardrail.terms.protocol.StrictProtocolElems

abstract class LanguageTerms[L <: LA, F[_]] { self =>
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
  def findCommonRawType(history: String, a: ReifiedRawType, b: ReifiedRawType): F[ReifiedRawType]

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
      litString: String => F[L#Term] = self.litString _,
      litFloat: Float => F[L#Term] = self.litFloat _,
      litDouble: Double => F[L#Term] = self.litDouble _,
      litInt: Int => F[L#Term] = self.litInt _,
      litLong: Long => F[L#Term] = self.litLong _,
      litBoolean: Boolean => F[L#Term] = self.litBoolean _,
      fullyQualifyPackageName: NonEmptyList[String] => F[NonEmptyList[String]] = self.fullyQualifyPackageName _,
      lookupEnumDefaultValue: (L#TypeName, L#Term, RenderedEnum[L]) => F[L#TermSelect] = self.lookupEnumDefaultValue _,
      formatPackageName: List[String] => F[NonEmptyList[String]] = self.formatPackageName _,
      formatTypeName: (String, Option[String]) => F[String] = self.formatTypeName _,
      formatFieldName: String => F[String] = self.formatFieldName _,
      formatMethodName: String => F[String] = self.formatMethodName _,
      formatMethodArgName: String => F[String] = self.formatMethodArgName _,
      formatEnumName: String => F[String] = self.formatEnumName _,
      parseType: Tracker[String] => F[Option[L#Type]] = self.parseType _,
      parseTypeName: String => F[Option[L#TypeName]] = self.parseTypeName _,
      pureTermName: String => F[L#TermName] = self.pureTermName _,
      pureTypeName: String => F[L#TypeName] = self.pureTypeName _,
      pureMethodParameter: (L#TermName, L#Type, Option[L#Term]) => F[L#MethodParameter] = self.pureMethodParameter _,
      typeNamesEqual: (L#TypeName, L#TypeName) => F[Boolean] = self.typeNamesEqual _,
      typesEqual: (L#Type, L#Type) => F[Boolean] = self.typesEqual _,
      extractTypeName: L#Type => F[Option[L#TypeName]] = self.extractTypeName _,
      extractTermName: L#TermName => F[String] = self.extractTermName _,
      extractTermNameFromParam: L#MethodParameter => F[String] = self.extractTermNameFromParam _,
      selectType: NonEmptyList[String] => F[L#Type] = self.selectType _,
      selectTerm: NonEmptyList[String] => F[L#Term] = self.selectTerm _,
      alterMethodParameterName: (L#MethodParameter, L#TermName) => F[L#MethodParameter] = self.alterMethodParameterName _,
      bytesType: () => F[L#Type] = self.bytesType _,
      uuidType: () => F[L#Type] = self.uuidType _,
      dateType: () => F[L#Type] = self.dateType _,
      dateTimeType: () => F[L#Type] = self.dateTimeType _,
      stringType: Option[String] => F[L#Type] = self.stringType _,
      floatType: () => F[L#Type] = self.floatType _,
      doubleType: () => F[L#Type] = self.doubleType _,
      numberType: Option[String] => F[L#Type] = self.numberType _,
      intType: () => F[L#Type] = self.intType _,
      longType: () => F[L#Type] = self.longType _,
      integerType: Option[String] => F[L#Type] = self.integerType _,
      booleanType: Option[String] => F[L#Type] = self.booleanType _,
      fallbackType: (Option[String], Option[String]) => F[L#Type] = self.fallbackType _,
      widenTypeName: L#TypeName => F[L#Type] = self.widenTypeName _,
      widenTermSelect: L#TermSelect => F[L#Term] = self.widenTermSelect _,
      widenClassDefinition: L#ClassDefinition => F[L#Definition] = self.widenClassDefinition _,
      widenObjectDefinition: L#ObjectDefinition => F[L#Definition] = self.widenObjectDefinition _,
      findCommonDefaultValue: (String, Option[L#Term], Option[L#Term]) => F[Option[L#Term]] = self.findCommonDefaultValue _,
      findCommonRawType: (String, ReifiedRawType, ReifiedRawType) => F[ReifiedRawType] = self.findCommonRawType _,
      renderImplicits: (Path, NonEmptyList[String], List[L#Import], List[L#Import], List[L#Import]) => F[Option[WriteTree]] = self.renderImplicits _,
      renderFrameworkImplicits: (
          Path,
          NonEmptyList[String],
          List[L#Import],
          List[L#TermName],
          List[L#Import],
          L#ObjectDefinition,
          L#TermName
      ) => F[WriteTree] = self.renderFrameworkImplicits _,
      renderFrameworkDefinitions: (Path, NonEmptyList[String], List[L#Import], List[L#Definition], L#TermName) => F[WriteTree] =
        self.renderFrameworkDefinitions _,
      writePackageObject: (
          Path,
          NonEmptyList[String],
          Option[NonEmptyList[String]],
          List[L#Import],
          List[L#Import],
          List[L#Import],
          List[L#Statement],
          List[L#Statement]
      ) => F[Option[WriteTree]] = self.writePackageObject _,
      writeProtocolDefinition: (
          Path,
          NonEmptyList[String],
          List[String],
          NonEmptyList[String],
          List[L#Import],
          Option[L#TermName],
          StrictProtocolElems[L]
      ) => F[(List[WriteTree], List[L#Statement])] = self.writeProtocolDefinition _,
      writeClient: (Path, NonEmptyList[String], List[L#Import], List[L#TermName], Option[NonEmptyList[String]], Client[L]) => F[List[WriteTree]] =
        self.writeClient _,
      writeServer: (Path, NonEmptyList[String], List[L#Import], List[L#TermName], Option[NonEmptyList[String]], Server[L]) => F[List[WriteTree]] =
        self.writeServer _,
      wrapToObject: (L#TermName, List[L#Import], List[L#Definition]) => F[Option[L#ObjectDefinition]] = self.wrapToObject _
  ) = {
    val newLitString                  = litString
    val newLitFloat                   = litFloat
    val newLitDouble                  = litDouble
    val newLitInt                     = litInt
    val newLitLong                    = litLong
    val newLitBoolean                 = litBoolean
    val newFullyQualifyPackageName    = fullyQualifyPackageName
    val newLookupEnumDefaultValue     = lookupEnumDefaultValue
    val newFormatPackageName          = formatPackageName
    val newFormatTypeName             = formatTypeName
    val newFormatFieldName            = formatFieldName
    val newFormatMethodName           = formatMethodName
    val newFormatMethodArgName        = formatMethodArgName
    val newFormatEnumName             = formatEnumName
    val newParseType                  = parseType
    val newParseTypeName              = parseTypeName
    val newPureTermName               = pureTermName
    val newPureTypeName               = pureTypeName
    val newPureMethodParameter        = pureMethodParameter
    val newTypeNamesEqual             = typeNamesEqual
    val newTypesEqual                 = typesEqual
    val newExtractTypeName            = extractTypeName
    val newExtractTermName            = extractTermName
    val newExtractTermNameFromParam   = extractTermNameFromParam
    val newSelectType                 = selectType
    val newSelectTerm                 = selectTerm
    val newAlterMethodParameterName   = alterMethodParameterName
    val newBytesType                  = bytesType
    val newUuidType                   = uuidType
    val newDateType                   = dateType
    val newDateTimeType               = dateTimeType
    val newStringType                 = stringType
    val newFloatType                  = floatType
    val newDoubleType                 = doubleType
    val newNumberType                 = numberType
    val newIntType                    = intType
    val newLongType                   = longType
    val newIntegerType                = integerType
    val newBooleanType                = booleanType
    val newFallbackType               = fallbackType
    val newWidenTypeName              = widenTypeName
    val newWidenTermSelect            = widenTermSelect
    val newWidenClassDefinition       = widenClassDefinition
    val newWidenObjectDefinition      = widenObjectDefinition
    val newFindCommonDefaultValue     = findCommonDefaultValue
    val newFindCommonRawType          = findCommonRawType
    val newRenderImplicits            = renderImplicits
    val newRenderFrameworkImplicits   = renderFrameworkImplicits
    val newRenderFrameworkDefinitions = renderFrameworkDefinitions
    val newWritePackageObject         = writePackageObject
    val newWriteProtocolDefinition    = writeProtocolDefinition
    val newWriteClient                = writeClient
    val newWriteServer                = writeServer
    val newWrapToObject               = wrapToObject

    new LanguageTerms[L, F] {
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
      def findCommonRawType(history: String, a: ReifiedRawType, b: ReifiedRawType)      = newFindCommonRawType(history, a, b)
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
      ) = newWriteServer(pkgPath, pkgName, customImports, frameworkImplicitNames, dtoComponents, server)
      def wrapToObject(name: L#TermName, imports: List[L#Import], definitions: List[L#Definition]) = newWrapToObject(name, imports, definitions)
    }
  }
}
