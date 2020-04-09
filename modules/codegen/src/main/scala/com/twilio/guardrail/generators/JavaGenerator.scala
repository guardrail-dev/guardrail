package com.twilio.guardrail.generators

import cats.~>
import cats.Monad
import cats.data.NonEmptyList
import cats.implicits._
import com.twilio.guardrail.SwaggerUtil.LazyResolvedType
import com.github.javaparser.ast._
import com.github.javaparser.ast.Modifier._
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, PrimitiveType, Type, ArrayType => AstArrayType }
import com.github.javaparser.ast.body.{ BodyDeclaration, Parameter, TypeDeclaration }
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt.Statement
import com.twilio.guardrail._
import com.twilio.guardrail.Common.resolveFile
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.generators.syntax.RichString
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.terms._
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.Locale
import com.github.javaparser.StaticJavaParser
import com.twilio.guardrail.languages.JavaLanguage.JavaTypeName
import org.eclipse.jdt.core.{ JavaCore, ToolFactory }
import org.eclipse.jdt.core.formatter.{ CodeFormatter, DefaultCodeFormatterConstants }
import org.eclipse.jface.text.Document
import scala.collection.JavaConverters._
import scala.util.Try

import scala.language.existentials

object JavaGenerator {
  def buildPkgDecl(parts: List[String]): Target[PackageDeclaration] = safeParseName(parts.mkString(".")).map(new PackageDeclaration(_))

  def buildMethodCall(name: String, arg: Option[Node] = None): Target[Node] = arg match {
    case Some(expr: Expression) => Target.pure(new MethodCallExpr(name, expr))
    case None                   => Target.pure(new MethodCallExpr(name))
    case other                  => Target.raiseUserError(s"Need expression to call '${name}' but got a ${other.getClass.getName} instead")
  }

  private val formatter = ToolFactory.createCodeFormatter(
    Map(
      JavaCore.COMPILER_SOURCE                                                     -> JavaCore.VERSION_1_8,
      JavaCore.COMPILER_COMPLIANCE                                                 -> JavaCore.VERSION_1_8,
      JavaCore.COMPILER_CODEGEN_TARGET_PLATFORM                                    -> JavaCore.VERSION_1_8,
      DefaultCodeFormatterConstants.FORMATTER_TAB_CHAR                             -> JavaCore.SPACE,
      DefaultCodeFormatterConstants.FORMATTER_TAB_SIZE                             -> "4",
      DefaultCodeFormatterConstants.FORMATTER_INSERT_SPACE_BEFORE_COLON_IN_CASE    -> DefaultCodeFormatterConstants.FALSE,
      DefaultCodeFormatterConstants.FORMATTER_INSERT_SPACE_BEFORE_COLON_IN_DEFAULT -> DefaultCodeFormatterConstants.FALSE
    ).asJava
  )

  def prettyPrintSource(source: CompilationUnit): Target[Array[Byte]] = {
    val _         = source.getChildNodes.asScala.headOption.fold(source.addOrphanComment _)(_.setComment)(GENERATED_CODE_COMMENT)
    val className = Try[TypeDeclaration[_]](source.getType(0)).fold(_ => "(unknown)", _.getNameAsString)
    val sourceStr = source.toString
    Option(formatter.format(CodeFormatter.K_COMPILATION_UNIT, sourceStr, 0, sourceStr.length, 0, "\n"))
      .fold(
        Target.raiseUserError[Array[Byte]](s"Failed to format class '$className'")
      )({ textEdit =>
        val doc = new Document(sourceStr)
        Try(textEdit.apply(doc)).fold(
          t => Target.raiseUserError[Array[Byte]](s"Failed to format class '$className': $t"),
          _ => Target.pure(doc.get.getBytes(StandardCharsets.UTF_8))
        )
      })
  }

  def writeClientTree(
      pkgPath: Path,
      pkg: List[String],
      pkgDecl: PackageDeclaration,
      imports: List[ImportDeclaration],
      definition: BodyDeclaration[_ <: BodyDeclaration[_]]
  ): Target[WriteTree] =
    definition match {
      case td: TypeDeclaration[_] =>
        val cu = new CompilationUnit()
        cu.setPackageDeclaration(pkgDecl)
        imports.map(cu.addImport)
        cu.addType(td)
        prettyPrintSource(cu).map(bytes => WriteTree(resolveFile(pkgPath)(pkg :+ s"${td.getNameAsString}.java"), bytes))
      case other =>
        Target.raiseUserError(s"Class definition must be a TypeDeclaration but it is a ${other.getClass.getName}")
    }

  def writeServerTree(
      pkgPath: Path,
      pkg: List[String],
      pkgDecl: PackageDeclaration,
      imports: List[ImportDeclaration],
      definition: BodyDeclaration[_ <: BodyDeclaration[_]]
  ): Target[WriteTree] =
    definition match {
      case td: TypeDeclaration[_] =>
        val cu = new CompilationUnit()
        cu.setPackageDeclaration(pkgDecl)
        imports.map(cu.addImport)
        cu.addType(td)
        prettyPrintSource(cu).map(bytes => WriteTree(resolveFile(pkgPath)(pkg :+ s"${td.getNameAsString}.java"), bytes))
      case other =>
        Target.raiseUserError(s"Class definition must be a TypeDeclaration but it is a ${other.getClass.getName}")
    }

  object JavaInterp extends (ScalaTerm[JavaLanguage, ?] ~> Target) {
    type L    = JavaLanguage
    type F[A] = Target[A]
    implicit def MonadF: Monad[F]         = Target.targetInstances
    def vendorPrefixes(): F[List[String]] = Target.pure(List("x-java", "x-jvm"))

    def litString(value: String): F[L#Term]        = Target.pure(new StringLiteralExpr(value))
    def litFloat(value: Float): F[L#Term]          = Target.pure(new DoubleLiteralExpr(value))
    def litDouble(value: Double): F[L#Term]        = Target.pure(new DoubleLiteralExpr(value))
    def litInt(value: Int): F[L#Term]              = Target.pure(new IntegerLiteralExpr(value))
    def litLong(value: Long): F[L#Term]            = Target.pure(new LongLiteralExpr(value))
    def litBoolean(value: Boolean): F[L#Term]      = Target.pure(new BooleanLiteralExpr(value))
    def liftOptionalType(value: L#Type): F[L#Type] = safeParseClassOrInterfaceType(s"java.util.Optional").map(_.setTypeArguments(new NodeList(value)))
    def liftOptionalTerm(value: L#Term): F[L#Term] = buildMethodCall("java.util.Optional.ofNullable", Some(value))
    def emptyArray(): F[L#Term] = for (cls <- safeParseClassOrInterfaceType("java.util.ArrayList")) yield {
      new ObjectCreationExpr(null, cls.setTypeArguments(new NodeList[Type]), new NodeList())
    }
    def emptyMap(): F[L#Term] =
      Target.pure(
        new ObjectCreationExpr(null, StaticJavaParser.parseClassOrInterfaceType("java.util.HashMap").setTypeArguments(new NodeList[Type]), new NodeList())
      )
    def emptyOptionalTerm(): F[L#Term] = buildMethodCall("java.util.Optional.empty")
    def liftVectorType(value: L#Type, customTpe: Option[L#Type]): F[L#Type] =
      customTpe
        .fold[Target[ClassOrInterfaceType]](safeParseClassOrInterfaceType("java.util.List").map(identity))({
          case t: ClassOrInterfaceType =>
            Target.pure(t)
          case x =>
            Target.raiseUserError(s"Unsure how to map $x")
        })
        .map(_.setTypeArguments(new NodeList(value)))
    def liftVectorTerm(value: L#Term): F[L#Term] = buildMethodCall("java.util.Collections.singletonList", Some(value))
    def liftMapType(value: L#Type, customTpe: Option[L#Type]): F[L#Type] =
      customTpe
        .fold[Target[ClassOrInterfaceType]](safeParseClassOrInterfaceType("java.util.Map").map(identity))({
          case t: ClassOrInterfaceType =>
            Target.pure(t)
          case x =>
            Target.raiseUserError(s"Unsure how to map $x")
        })
        .map(_.setTypeArguments(STRING_TYPE, value))

    def fullyQualifyPackageName(rawPkgName: List[String]): F[List[String]] = Target.pure(rawPkgName)

    def lookupEnumDefaultValue(tpe: L#TypeName, defaultValue: L#Term, values: List[(String, L#TermName, L#TermSelect)]): F[L#TermSelect] =
      defaultValue match {
        case s: StringLiteralExpr =>
          values
            .find(_._1 == s.getValue)
            .fold(Target.raiseUserError[Name](s"Enumeration $tpe is not defined for default value ${s.getValue}"))(value => Target.pure(value._3))
        case _ =>
          Target.raiseUserError(s"Enumeration $tpe somehow has a default value that isn't a string")
      }
    def formatEnumName(enumValue: String): F[String] = Target.pure(enumValue.toSnakeCase.toUpperCase(Locale.US))

    def embedArray(tpe: LazyResolvedType[L], containerTpe: Option[L#Type]): F[LazyResolvedType[L]] = tpe match {
      case SwaggerUtil.Deferred(tpe) =>
        Target.pure(SwaggerUtil.DeferredArray[L](tpe, containerTpe))
      case SwaggerUtil.DeferredArray(_, _) =>
        Target.raiseUserError("FIXME: Got an Array of Arrays, currently not supported")
      case SwaggerUtil.DeferredMap(_, _) =>
        Target.raiseUserError("FIXME: Got an Array of Maps, currently not supported")
    }
    def embedMap(tpe: LazyResolvedType[L], containerTpe: Option[L#Type]): F[LazyResolvedType[L]] = tpe match {
      case SwaggerUtil.Deferred(inner) =>
        Target.pure(SwaggerUtil.DeferredMap[L](inner, containerTpe))
      case SwaggerUtil.DeferredMap(_, _) =>
        Target.raiseUserError("FIXME: Got a map of maps, currently not supported")
      case SwaggerUtil.DeferredArray(_, _) =>
        Target.raiseUserError("FIXME: Got a map of arrays, currently not supported")
    }

    def parseType(tpe: String): F[Option[L#Type]] =
      safeParseType(tpe)
        .map(Option.apply)
        .recover({
          case err =>
            println(s"Warning: Unparsable x-java-type: $tpe $err")
            None
        })
    def parseTypeName(tpe: String): F[Option[L#TypeName]] = Option(tpe).map(_.trim).filterNot(_.isEmpty).traverse(safeParseTypeName)
    def pureTermName(tpe: String): F[L#TermName] =
      Option(tpe).map(_.trim).filterNot(_.isEmpty).map(_.escapeIdentifier).map(safeParseName).getOrElse(Target.raiseUserError("A structure's name is empty"))
    def pureTypeName(tpe: String): F[L#TypeName] =
      Option(tpe).map(_.trim).filterNot(_.isEmpty).map(safeParseTypeName).getOrElse(Target.raiseUserError("A structure's name is empty"))

    def pureMethodParameter(nameStr: L#TermName, tpe: L#Type, default: Option[L#Term]): F[L#MethodParameter] =
      safeParseSimpleName(nameStr.asString.escapeIdentifier).map(name => new Parameter(new NodeList(finalModifier), tpe, name))
    def typeNamesEqual(a: L#TypeName, b: L#TypeName): F[Boolean] = Target.pure(a.asString == b.asString)
    def typesEqual(a: L#Type, b: L#Type): F[Boolean]             = Target.pure(a.equals(b))
    def extractTypeName(tpe: L#Type): F[Option[L#TypeName]] = {
      def extractTypeName(tpe: Type): Target[JavaTypeName] = tpe match {
        case a: AstArrayType if a.getComponentType.isPrimitiveType =>
          extractTypeName(new AstArrayType(a.getComponentType.asPrimitiveType().toBoxedType))
        case ci: ClassOrInterfaceType =>
          safeParseTypeName(ci.getNameAsString)
        case p: PrimitiveType =>
          safeParseTypeName(p.toBoxedType.getNameAsString)
        case other =>
          safeParseTypeName(other.asString)
      }
      extractTypeName(tpe).map(Option.apply)
    }
    def extractTermName(term: L#TermName): F[String]           = Target.pure(term.asString)
    def selectType(typeNames: NonEmptyList[String]): F[L#Type] = safeParseType(typeNames.toList.mkString("."))
    def selectTerm(termNames: NonEmptyList[String]): F[L#Term] = safeParseExpression[Expression](termNames.toList.mkString(".")).map(v => v: Node)
    def alterMethodParameterName(param: L#MethodParameter, name: L#TermName): F[L#MethodParameter] =
      safeParseSimpleName(name.asString.escapeIdentifier).map(
        new Parameter(
          param.getTokenRange.orElse(null),
          param.getModifiers,
          param.getAnnotations,
          param.getType,
          param.isVarArgs,
          param.getVarArgsAnnotations,
          _
        )
      )

    def bytesType(): F[L#Type]                                               = Target.raiseUserError("format: bytes not supported for Java")
    def uuidType(): F[L#Type]                                                = safeParseType("java.util.UUID")
    def dateType(): F[L#Type]                                                = safeParseType("java.time.LocalDate")
    def dateTimeType(): F[L#Type]                                            = safeParseType("java.time.OffsetDateTime")
    def stringType(format: Option[String]): F[L#Type]                        = format.fold(Target.pure[Type](STRING_TYPE))(safeParseType)
    def floatType(): F[L#Type]                                               = safeParseType("Float")
    def doubleType(): F[L#Type]                                              = safeParseType("Double")
    def numberType(format: Option[String]): F[L#Type]                        = safeParseType("java.math.BigDecimal")
    def intType(): F[L#Type]                                                 = safeParseType("Integer")
    def longType(): F[L#Type]                                                = safeParseType("Long")
    def integerType(format: Option[String]): F[L#Type]                       = safeParseType("java.math.BigInteger")
    def booleanType(format: Option[String]): F[L#Type]                       = safeParseType("Boolean")
    def arrayType(format: Option[String]): F[L#Type]                         = safeParseClassOrInterfaceType("java.util.List").map(_.setTypeArguments(new NodeList[Type](STRING_TYPE)))
    def fallbackType(tpe: Option[String], format: Option[String]): F[L#Type] = Target.fromOption(tpe, UserError("Missing type")).flatMap(safeParseType)

    def widenTypeName(tpe: L#TypeName): F[L#Type]                         = safeParseType(tpe.asString)
    def widenTermSelect(value: L#TermSelect): F[L#Term]                   = Target.pure(value)
    def widenClassDefinition(value: L#ClassDefinition): F[L#Definition]   = Target.pure(value)
    def widenObjectDefinition(value: L#ObjectDefinition): F[L#Definition] = Target.pure(value)

    def findCommonDefaultValue(history: String, a: Option[L#Term], b: Option[L#Term]): F[Option[L#Term]] = (a, b) match {
      case (Some(va), Some(vb)) =>
        if (va.toString() == vb.toString()) {
          Target.pure(Some(va))
        } else {
          Target.raiseUserError(
            s"There is a mismatch at $history between default values $va and $vb. This parameter is defined at multiple places and those definitions are incompatible with each other. They must have the same name, type and default value. ($history)"
          )
        }
      case (va, vb) =>
        Target.pure(va.orElse(vb))
    }
    def findCommonRawType(history: String, a: RawParameterType, b: RawParameterType): F[RawParameterType] =
      if (a == b) {
        Target.pure(a)
      } else {
        Target.raiseUserError(
          s"There is a mismatch at $history between types $a and $b. Conflicting definitions between types and inherited types are not supported."
        )
      }

    def renderImplicits(
        pkgPath: Path,
        pkgName: List[String],
        frameworkImports: List[L#Import],
        jsonImports: List[L#Import],
        customImports: List[L#Import]
    ): F[Option[WriteTree]] = Target.pure(None)
    def renderFrameworkImplicits(
        pkgPath: Path,
        pkgName: List[String],
        frameworkImports: List[L#Import],
        jsonImports: List[L#Import],
        frameworkImplicits: L#ObjectDefinition,
        frameworkImplicitName: L#TermName
    ): F[WriteTree] = Target.raiseUserError("Java does not support Framework Implicits")
    def renderFrameworkDefinitions(
        pkgPath: Path,
        pkgName: List[String],
        frameworkImports: List[L#Import],
        frameworkDefinitions: L#ClassDefinition,
        frameworkDefinitionsName: L#TermName
    ): F[WriteTree] =
      for {
        pkgDecl <- buildPkgDecl(pkgName)
        cu = {
          val cu = new CompilationUnit()
          cu.setPackageDeclaration(pkgDecl)
          frameworkImports.map(cu.addImport)
          cu.addType(frameworkDefinitions)
          cu
        }
        bytes <- prettyPrintSource(cu)
      } yield WriteTree(resolveFile(pkgPath)(List(s"${frameworkDefinitionsName.asString}.java")), bytes)

    def writePackageObject(
        dtoPackagePath: Path,
        dtoComponents: Option[NonEmptyList[String]],
        customImports: List[L#Import],
        packageObjectImports: List[L#Import],
        protocolImports: List[L#Import],
        packageObjectContents: List[L#ValueDefinition],
        extraTypes: List[L#Statement]
    ): F[Option[WriteTree]] =
      for {
        pkgDecl <- dtoComponents.traverse(xs => buildPkgDecl(xs.toList))
        bytes   <- pkgDecl.traverse(x => prettyPrintSource(new CompilationUnit().setPackageDeclaration(x)))
      } yield bytes.map(WriteTree(resolveFile(dtoPackagePath)(List.empty).resolve("package-info.java"), _))
    def writeProtocolDefinition(
        outputPath: Path,
        pkgName: List[String],
        definitions: List[String],
        dtoComponents: List[String],
        imports: List[L#Import],
        elem: StrictProtocolElems[L]
    ): F[(List[WriteTree], List[L#Statement])] =
      for {
        pkgDecl      <- buildPkgDecl(dtoComponents)
        showerImport <- safeParseRawImport((pkgName :+ "Shower").mkString("."))
        nameAndCompilationUnit = elem match {
          case EnumDefinition(_, _, _, _, cls, staticDefns) =>
            val cu = new CompilationUnit()
            cu.setPackageDeclaration(pkgDecl)
            imports.foreach(cu.addImport)
            staticDefns.extraImports.foreach(cu.addImport)
            cu.addImport(showerImport)
            val clsCopy = cls.clone()
            staticDefns.definitions.foreach(clsCopy.addMember)
            cu.addType(clsCopy)
            Option((cls.getName.getIdentifier, cu))
          case ClassDefinition(_, _, _, cls, staticDefns, _) =>
            val cu = new CompilationUnit()
            cu.setPackageDeclaration(pkgDecl)
            imports.foreach(cu.addImport)
            staticDefns.extraImports.foreach(cu.addImport)
            val clsCopy = cls.clone()
            staticDefns.definitions.foreach(clsCopy.addMember)
            cu.addType(clsCopy)
            Option((cls.getName.getIdentifier, cu))
          case ADT(name, tpe, _, trt, staticDefns) =>
            val cu = new CompilationUnit()
            cu.setPackageDeclaration(pkgDecl)
            imports.foreach(cu.addImport)
            staticDefns.extraImports.foreach(cu.addImport)
            val trtCopy = trt.clone()
            staticDefns.definitions.foreach(trtCopy.addMember)
            cu.addType(trtCopy)
            Option((name, cu))
          case RandomType(_, _) =>
            Option.empty
        }
        nameAndBytes <- nameAndCompilationUnit.fold(Target.pure(Option.empty[(String, Array[Byte])]))({
          case (name, cu) =>
            prettyPrintSource(cu).map(bytes => Option((name, bytes)))
        })
      } yield nameAndBytes.fold((List.empty[WriteTree], List.empty[Statement]))({
        case (name, bytes) =>
          (List(WriteTree(resolveFile(outputPath)(dtoComponents).resolve(s"$name.java"), bytes)), List.empty[Statement])
      })
    def writeClient(
        pkgPath: Path,
        pkgName: List[String],
        customImports: List[L#Import],
        frameworkImplicitName: Option[L#TermName],
        dtoComponents: Option[List[String]],
        _client: Client[L]
    ): F[List[WriteTree]] = {
      val Client(pkg, clientName, imports, staticDefns, client, responseDefinitions) = _client
      for {
        pkgDecl             <- buildPkgDecl(pkgName ++ pkg)
        commonImport        <- safeParseRawImport((pkgName :+ "*").mkString("."))
        dtoComponentsImport <- dtoComponents.traverse(x => safeParseRawImport((x :+ "*").mkString(".")))
        allImports    = imports ++ (customImports :+ commonImport) ++ dtoComponentsImport
        clientClasses = client.map(_.merge).toList
        trees <- (clientClasses ++ responseDefinitions).traverse(writeClientTree(pkgPath, pkg, pkgDecl, allImports, _))
      } yield trees
    }
    def writeServer(
        pkgPath: Path,
        pkgName: List[String],
        customImports: List[L#Import],
        frameworkImplicitName: Option[L#TermName],
        dtoComponents: Option[List[String]],
        server: Server[L]
    ): F[List[WriteTree]] = {
      val Server(pkg, extraImports, handlerDefinition, serverDefinitions) = server
      for {
        pkgDecl             <- buildPkgDecl(pkgName ++ pkg)
        commonImport        <- safeParseRawImport((pkgName :+ "*").mkString("."))
        dtoComponentsImport <- dtoComponents.traverse(x => safeParseRawImport((x :+ "*").mkString(".")))
        allImports = extraImports ++ List(commonImport) ++ dtoComponentsImport ++ customImports
        handlerTree <- writeServerTree(pkgPath, pkg, pkgDecl, allImports, handlerDefinition)
        serverTrees <- serverDefinitions.traverse(writeServerTree(pkgPath, pkg, pkgDecl, allImports, _))
      } yield handlerTree +: serverTrees
    }

    def wrapToObject(name: L#TermName, imports: List[L#Import], definitions: List[L#Definition]): F[L#ObjectDefinition] =
      Target.raiseUserError("Currently not supported for Java")

    def apply[T](term: ScalaTerm[JavaLanguage, T]): Target[T] = term match {
      case VendorPrefixes() => vendorPrefixes()

      case LitString(value)        => litString(value)
      case LitFloat(value)         => litFloat(value)
      case LitDouble(value)        => litDouble(value)
      case LitInt(value)           => litInt(value)
      case LitLong(value)          => litLong(value)
      case LitBoolean(value)       => litBoolean(value)
      case LiftOptionalType(value) => liftOptionalType(value)
      case LiftOptionalTerm(value) => liftOptionalTerm(value)
      case EmptyOptionalTerm()     => emptyOptionalTerm()
      case EmptyArray() =>
        emptyArray()
      case EmptyMap() =>
        emptyMap()
      case LiftVectorType(value, customTpe) =>
        liftVectorType(value, customTpe)
      case LiftVectorTerm(value) => liftVectorTerm(value)
      case LiftMapType(value, customTpe) =>
        liftMapType(value, customTpe)
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
}
