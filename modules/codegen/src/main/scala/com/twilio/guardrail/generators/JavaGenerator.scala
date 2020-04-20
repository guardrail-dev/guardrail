package com.twilio.guardrail.generators

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
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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

  def prettyPrintSource(source: CompilationUnit): Target[Future[Array[Byte]]] = {
    val _         = source.getChildNodes.asScala.headOption.fold(source.addOrphanComment _)(_.setComment)(GENERATED_CODE_COMMENT)
    val className = Try[TypeDeclaration[_]](source.getType(0)).fold(_ => "(unknown)", _.getNameAsString)
    val sourceStr = source.toString
    Option(formatter.format(CodeFormatter.K_COMPILATION_UNIT, sourceStr, 0, sourceStr.length, 0, "\n"))
      .fold(
        Target.raiseUserError[Future[Array[Byte]]](s"Failed to format class '$className'")
      )({ textEdit =>
        val doc = new Document(sourceStr)
        Try(textEdit.apply(doc)).fold(
          t => Target.raiseUserError[Future[Array[Byte]]](s"Failed to format class '$className': $t"),
          _ =>
            Target.pure(Future {
              doc.get.getBytes(StandardCharsets.UTF_8)
            })
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

  object JavaInterp extends LanguageTerms[JavaLanguage, Target] {
    implicit def MonadF: Monad[Target]         = Target.targetInstances
    def vendorPrefixes(): Target[List[String]] = Target.pure(List("x-java", "x-jvm"))

    def litString(value: String): Target[com.github.javaparser.ast.Node]   = Target.pure(new StringLiteralExpr(value))
    def litFloat(value: Float): Target[com.github.javaparser.ast.Node]     = Target.pure(new DoubleLiteralExpr(value))
    def litDouble(value: Double): Target[com.github.javaparser.ast.Node]   = Target.pure(new DoubleLiteralExpr(value))
    def litInt(value: Int): Target[com.github.javaparser.ast.Node]         = Target.pure(new IntegerLiteralExpr(value))
    def litLong(value: Long): Target[com.github.javaparser.ast.Node]       = Target.pure(new LongLiteralExpr(value))
    def litBoolean(value: Boolean): Target[com.github.javaparser.ast.Node] = Target.pure(new BooleanLiteralExpr(value))
    def liftOptionalType(value: com.github.javaparser.ast.`type`.Type): Target[com.github.javaparser.ast.`type`.Type] =
      safeParseClassOrInterfaceType(s"java.util.Optional").map(_.setTypeArguments(new NodeList(value)))
    def liftOptionalTerm(value: com.github.javaparser.ast.Node): Target[com.github.javaparser.ast.Node] =
      buildMethodCall("java.util.Optional.ofNullable", Some(value))
    def emptyArray(): Target[com.github.javaparser.ast.Node] = for (cls <- safeParseClassOrInterfaceType("java.util.ArrayList")) yield {
      new ObjectCreationExpr(null, cls.setTypeArguments(new NodeList[Type]), new NodeList())
    }
    def emptyMap(): Target[com.github.javaparser.ast.Node] =
      Target.pure(
        new ObjectCreationExpr(null, StaticJavaParser.parseClassOrInterfaceType("java.util.HashMap").setTypeArguments(new NodeList[Type]), new NodeList())
      )
    def emptyOptionalTerm(): Target[com.github.javaparser.ast.Node] = buildMethodCall("java.util.Optional.empty")
    def liftVectorType(
        value: com.github.javaparser.ast.`type`.Type,
        customTpe: Option[com.github.javaparser.ast.`type`.Type]
    ): Target[com.github.javaparser.ast.`type`.Type] =
      customTpe
        .fold[Target[ClassOrInterfaceType]](safeParseClassOrInterfaceType("java.util.List").map(identity))({
          case t: ClassOrInterfaceType =>
            Target.pure(t)
          case x =>
            Target.raiseUserError(s"Unsure how to map $x")
        })
        .map(_.setTypeArguments(new NodeList(value)))
    def liftVectorTerm(value: com.github.javaparser.ast.Node): Target[com.github.javaparser.ast.Node] =
      buildMethodCall("java.util.Collections.singletonList", Some(value))
    def liftMapType(
        value: com.github.javaparser.ast.`type`.Type,
        customTpe: Option[com.github.javaparser.ast.`type`.Type]
    ): Target[com.github.javaparser.ast.`type`.Type] =
      customTpe
        .fold[Target[ClassOrInterfaceType]](safeParseClassOrInterfaceType("java.util.Map").map(identity))({
          case t: ClassOrInterfaceType =>
            Target.pure(t)
          case x =>
            Target.raiseUserError(s"Unsure how to map $x")
        })
        .map(_.setTypeArguments(STRING_TYPE, value))

    def fullyQualifyPackageName(rawPkgName: List[String]): Target[List[String]] = Target.pure(rawPkgName)

    def lookupEnumDefaultValue(
        tpe: JavaTypeName,
        defaultValue: com.github.javaparser.ast.Node,
        values: List[(String, com.github.javaparser.ast.expr.Name, com.github.javaparser.ast.expr.Name)]
    ): Target[com.github.javaparser.ast.expr.Name] =
      defaultValue match {
        case s: StringLiteralExpr =>
          values
            .find(_._1 == s.getValue)
            .fold(Target.raiseUserError[Name](s"Enumeration $tpe is not defined for default value ${s.getValue}"))(value => Target.pure(value._3))
        case _ =>
          Target.raiseUserError(s"Enumeration $tpe somehow has a default value that isn't a string")
      }
    def formatEnumName(enumValue: String): Target[String] = Target.pure(enumValue.toSnakeCase.toUpperCase(Locale.US))

    def embedArray(tpe: LazyResolvedType[JavaLanguage], containerTpe: Option[com.github.javaparser.ast.`type`.Type]): Target[LazyResolvedType[JavaLanguage]] =
      tpe match {
        case SwaggerUtil.Deferred(tpe) =>
          Target.pure(SwaggerUtil.DeferredArray[JavaLanguage](tpe, containerTpe))
        case SwaggerUtil.DeferredArray(_, _) =>
          Target.raiseUserError("FIXME: Got an Array of Arrays, currently not supported")
        case SwaggerUtil.DeferredMap(_, _) =>
          Target.raiseUserError("FIXME: Got an Array of Maps, currently not supported")
      }
    def embedMap(tpe: LazyResolvedType[JavaLanguage], containerTpe: Option[com.github.javaparser.ast.`type`.Type]): Target[LazyResolvedType[JavaLanguage]] =
      tpe match {
        case SwaggerUtil.Deferred(inner) =>
          Target.pure(SwaggerUtil.DeferredMap[JavaLanguage](inner, containerTpe))
        case SwaggerUtil.DeferredMap(_, _) =>
          Target.raiseUserError("FIXME: Got a map of maps, currently not supported")
        case SwaggerUtil.DeferredArray(_, _) =>
          Target.raiseUserError("FIXME: Got a map of arrays, currently not supported")
      }

    def parseType(tpe: String): Target[Option[com.github.javaparser.ast.`type`.Type]] =
      safeParseType(tpe)
        .map(Option.apply)
        .recover({
          case err =>
            println(s"Warning: Unparsable x-java-type: $tpe $err")
            None
        })
    def parseTypeName(tpe: String): Target[Option[JavaTypeName]] = Option(tpe).map(_.trim).filterNot(_.isEmpty).traverse(safeParseTypeName)
    def pureTermName(tpe: String): Target[com.github.javaparser.ast.expr.Name] =
      Option(tpe).map(_.trim).filterNot(_.isEmpty).map(_.escapeIdentifier).map(safeParseName).getOrElse(Target.raiseUserError("A structure's name is empty"))
    def pureTypeName(tpe: String): Target[JavaTypeName] =
      Option(tpe).map(_.trim).filterNot(_.isEmpty).map(safeParseTypeName).getOrElse(Target.raiseUserError("A structure's name is empty"))

    def pureMethodParameter(
        nameStr: com.github.javaparser.ast.expr.Name,
        tpe: com.github.javaparser.ast.`type`.Type,
        default: Option[com.github.javaparser.ast.Node]
    ): Target[com.github.javaparser.ast.body.Parameter] =
      safeParseSimpleName(nameStr.asString.escapeIdentifier).map(name => new Parameter(new NodeList(finalModifier), tpe, name))
    def typeNamesEqual(a: JavaTypeName, b: JavaTypeName): Target[Boolean]                                               = Target.pure(a.asString == b.asString)
    def typesEqual(a: com.github.javaparser.ast.`type`.Type, b: com.github.javaparser.ast.`type`.Type): Target[Boolean] = Target.pure(a.equals(b))
    def extractTypeName(tpe: com.github.javaparser.ast.`type`.Type): Target[Option[JavaTypeName]] = {
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
    def extractTermName(term: com.github.javaparser.ast.expr.Name): Target[String]                 = Target.pure(term.asString)
    def selectType(typeNames: NonEmptyList[String]): Target[com.github.javaparser.ast.`type`.Type] = safeParseType(typeNames.toList.mkString("."))
    def selectTerm(termNames: NonEmptyList[String]): Target[com.github.javaparser.ast.Node] =
      safeParseExpression[Expression](termNames.toList.mkString(".")).map(v => v: Node)
    def alterMethodParameterName(
        param: com.github.javaparser.ast.body.Parameter,
        name: com.github.javaparser.ast.expr.Name
    ): Target[com.github.javaparser.ast.body.Parameter] =
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

    def bytesType(): Target[com.github.javaparser.ast.`type`.Type]                         = Target.raiseUserError("format: bytes not supported for Java")
    def uuidType(): Target[com.github.javaparser.ast.`type`.Type]                          = safeParseType("java.util.UUID")
    def dateType(): Target[com.github.javaparser.ast.`type`.Type]                          = safeParseType("java.time.LocalDate")
    def dateTimeType(): Target[com.github.javaparser.ast.`type`.Type]                      = safeParseType("java.time.OffsetDateTime")
    def stringType(format: Option[String]): Target[com.github.javaparser.ast.`type`.Type]  = format.fold(Target.pure[Type](STRING_TYPE))(safeParseType)
    def floatType(): Target[com.github.javaparser.ast.`type`.Type]                         = safeParseType("Float")
    def doubleType(): Target[com.github.javaparser.ast.`type`.Type]                        = safeParseType("Double")
    def numberType(format: Option[String]): Target[com.github.javaparser.ast.`type`.Type]  = safeParseType("java.math.BigDecimal")
    def intType(): Target[com.github.javaparser.ast.`type`.Type]                           = safeParseType("Integer")
    def longType(): Target[com.github.javaparser.ast.`type`.Type]                          = safeParseType("Long")
    def integerType(format: Option[String]): Target[com.github.javaparser.ast.`type`.Type] = safeParseType("java.math.BigInteger")
    def booleanType(format: Option[String]): Target[com.github.javaparser.ast.`type`.Type] = safeParseType("Boolean")
    def arrayType(format: Option[String]): Target[com.github.javaparser.ast.`type`.Type] =
      safeParseClassOrInterfaceType("java.util.List").map(_.setTypeArguments(new NodeList[Type](STRING_TYPE)))
    def fallbackType(tpe: Option[String], format: Option[String]): Target[com.github.javaparser.ast.`type`.Type] =
      Target.fromOption(tpe, UserError("Missing type")).flatMap(safeParseType)

    def widenTypeName(tpe: JavaTypeName): Target[com.github.javaparser.ast.`type`.Type]                     = safeParseType(tpe.asString)
    def widenTermSelect(value: com.github.javaparser.ast.expr.Name): Target[com.github.javaparser.ast.Node] = Target.pure(value)
    def widenClassDefinition(
        value: com.github.javaparser.ast.body.TypeDeclaration[_ <: com.github.javaparser.ast.body.TypeDeclaration[_]]
    ): Target[com.github.javaparser.ast.body.BodyDeclaration[_ <: com.github.javaparser.ast.body.BodyDeclaration[_]]] = Target.pure(value)
    def widenObjectDefinition(value: Nothing): Target[com.github.javaparser.ast.body.BodyDeclaration[_ <: com.github.javaparser.ast.body.BodyDeclaration[_]]] =
      Target.pure(value)

    def findCommonDefaultValue(
        history: String,
        a: Option[com.github.javaparser.ast.Node],
        b: Option[com.github.javaparser.ast.Node]
    ): Target[Option[com.github.javaparser.ast.Node]] = (a, b) match {
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
    def findCommonRawType(history: String, a: RawParameterType, b: RawParameterType): Target[RawParameterType] =
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
        frameworkImports: List[com.github.javaparser.ast.ImportDeclaration],
        jsonImports: List[com.github.javaparser.ast.ImportDeclaration],
        customImports: List[com.github.javaparser.ast.ImportDeclaration]
    ): Target[Option[WriteTree]] = Target.pure(None)
    def renderFrameworkImplicits(
        pkgPath: Path,
        pkgName: List[String],
        frameworkImports: List[com.github.javaparser.ast.ImportDeclaration],
        jsonImports: List[com.github.javaparser.ast.ImportDeclaration],
        frameworkImplicits: Nothing,
        frameworkImplicitName: com.github.javaparser.ast.expr.Name
    ): Target[WriteTree] = Target.raiseUserError("Java does not support Framework Implicits")
    def renderFrameworkDefinitions(
        pkgPath: Path,
        pkgName: List[String],
        frameworkImports: List[com.github.javaparser.ast.ImportDeclaration],
        frameworkDefinitions: com.github.javaparser.ast.body.TypeDeclaration[_ <: com.github.javaparser.ast.body.TypeDeclaration[_]],
        frameworkDefinitionsName: com.github.javaparser.ast.expr.Name
    ): Target[WriteTree] =
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
        customImports: List[com.github.javaparser.ast.ImportDeclaration],
        packageObjectImports: List[com.github.javaparser.ast.ImportDeclaration],
        protocolImports: List[com.github.javaparser.ast.ImportDeclaration],
        packageObjectContents: List[com.github.javaparser.ast.body.VariableDeclarator],
        extraTypes: List[com.github.javaparser.ast.stmt.Statement]
    ): Target[Option[WriteTree]] =
      for {
        pkgDecl <- dtoComponents.traverse(xs => buildPkgDecl(xs.toList))
        bytes   <- pkgDecl.traverse(x => prettyPrintSource(new CompilationUnit().setPackageDeclaration(x)))
      } yield bytes.map(WriteTree(resolveFile(dtoPackagePath)(List.empty).resolve("package-info.java"), _))
    def writeProtocolDefinition(
        outputPath: Path,
        pkgName: List[String],
        definitions: List[String],
        dtoComponents: List[String],
        imports: List[com.github.javaparser.ast.ImportDeclaration],
        elem: StrictProtocolElems[JavaLanguage]
    ): Target[(List[WriteTree], List[com.github.javaparser.ast.stmt.Statement])] =
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
        nameAndBytes <- nameAndCompilationUnit.fold(Target.pure(Option.empty[(String, Future[Array[Byte]])]))({
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
        customImports: List[com.github.javaparser.ast.ImportDeclaration],
        frameworkImplicitName: Option[com.github.javaparser.ast.expr.Name],
        dtoComponents: Option[List[String]],
        _client: Client[JavaLanguage]
    ): Target[List[WriteTree]] = {
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
        customImports: List[com.github.javaparser.ast.ImportDeclaration],
        frameworkImplicitName: Option[com.github.javaparser.ast.expr.Name],
        dtoComponents: Option[List[String]],
        server: Server[JavaLanguage]
    ): Target[List[WriteTree]] = {
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

    def wrapToObject(
        name: com.github.javaparser.ast.expr.Name,
        imports: List[com.github.javaparser.ast.ImportDeclaration],
        definitions: List[com.github.javaparser.ast.body.BodyDeclaration[_ <: com.github.javaparser.ast.body.BodyDeclaration[_]]]
    ): Target[Nothing] =
      Target.raiseUserError("Currently not supported for Java")
  }
}
