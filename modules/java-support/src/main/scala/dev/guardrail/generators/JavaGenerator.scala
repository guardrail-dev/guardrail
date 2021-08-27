package dev.guardrail.generators

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all._
import com.github.javaparser.ast._
import com.github.javaparser.ast.Modifier._
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, PrimitiveType, Type, ArrayType => AstArrayType }
import com.github.javaparser.ast.body.{ BodyDeclaration, ClassOrInterfaceDeclaration, Parameter, TypeDeclaration }
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt.Statement
import dev.guardrail._
import dev.guardrail.core.Tracker
import dev.guardrail.Common.resolveFile
import dev.guardrail.generators.syntax.Java._
import dev.guardrail.generators.syntax.RichString
import dev.guardrail.languages.JavaLanguage
import dev.guardrail.terms._
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.Locale
import dev.guardrail.languages.JavaLanguage.JavaTypeName
import org.eclipse.jdt.core.{ JavaCore, ToolFactory }
import org.eclipse.jdt.core.formatter.{ CodeFormatter, DefaultCodeFormatterConstants }
import org.eclipse.jface.text.Document
import scala.collection.JavaConverters._
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.existentials

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Null"))
object JavaGenerator {
  def buildPkgDecl(parts: NonEmptyList[String]): Target[PackageDeclaration] =
    safeParseName(parts.toList.mkString(".")).map(new PackageDeclaration(_))

  private val FORMATTER_OPTIONS = Map(
    JavaCore.COMPILER_SOURCE                                                     -> JavaCore.VERSION_1_8,
    JavaCore.COMPILER_COMPLIANCE                                                 -> JavaCore.VERSION_1_8,
    JavaCore.COMPILER_CODEGEN_TARGET_PLATFORM                                    -> JavaCore.VERSION_1_8,
    DefaultCodeFormatterConstants.FORMATTER_TAB_CHAR                             -> JavaCore.SPACE,
    DefaultCodeFormatterConstants.FORMATTER_TAB_SIZE                             -> "4",
    DefaultCodeFormatterConstants.FORMATTER_INSERT_SPACE_BEFORE_COLON_IN_CASE    -> DefaultCodeFormatterConstants.FALSE,
    DefaultCodeFormatterConstants.FORMATTER_INSERT_SPACE_BEFORE_COLON_IN_DEFAULT -> DefaultCodeFormatterConstants.FALSE
  ).asJava

  def prettyPrintSource(path: Path, source: CompilationUnit): Target[WriteTree] =
    Target.pure(
      WriteTree(
        path, {
          val _         = source.getChildNodes.asScala.headOption.fold(source.addOrphanComment _)(_.setComment)(GENERATED_CODE_COMMENT)
          val sourceStr = source.toString
          Future {
            val className = Try[TypeDeclaration[_]](source.getType(0)).fold(_ => "(unknown)", _.getNameAsString)
            val formatter = ToolFactory.createCodeFormatter(FORMATTER_OPTIONS)
            val result: Either[Option[Throwable], Array[Byte]] = for {
              textEdit <- Option(formatter.format(CodeFormatter.K_COMPILATION_UNIT, sourceStr, 0, sourceStr.length, 0, "\n")).toRight(None)
              doc = new Document(sourceStr)
              _ <- Try(textEdit.apply(doc)).toEither.leftMap(Some(_))
            } yield doc.get.getBytes(StandardCharsets.UTF_8)
            result
              .fold[Target[Array[Byte]]](
                _.fold[Target[Array[Byte]]](Target.raiseUserError(s"Failed to format class '$className'")) { t =>
                  Target.raiseUserError(s"Failed to format class '$className': $t")
                },
                Target.pure _
              )
          }
        }
      )
    )

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
        imports.foreach(cu.addImport)
        cu.addType(td)
        prettyPrintSource(resolveFile(pkgPath)(pkg :+ s"${td.getNameAsString}.java"), cu)
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
        imports.foreach(cu.addImport)
        cu.addType(td)
        prettyPrintSource(resolveFile(pkgPath)(pkg :+ s"${td.getNameAsString}.java"), cu)
      case other =>
        Target.raiseUserError(s"Class definition must be a TypeDeclaration but it is a ${other.getClass.getName}")
    }

  object JavaInterp extends LanguageTerms[JavaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances

    def litString(value: String): Target[com.github.javaparser.ast.Node]   = Target.pure(new StringLiteralExpr(value))
    def litFloat(value: Float): Target[com.github.javaparser.ast.Node]     = Target.pure(new DoubleLiteralExpr(value))
    def litDouble(value: Double): Target[com.github.javaparser.ast.Node]   = Target.pure(new DoubleLiteralExpr(value))
    def litInt(value: Int): Target[com.github.javaparser.ast.Node]         = Target.pure(new IntegerLiteralExpr(value.toString))
    def litLong(value: Long): Target[com.github.javaparser.ast.Node]       = Target.pure(new LongLiteralExpr(value.toString))
    def litBoolean(value: Boolean): Target[com.github.javaparser.ast.Node] = Target.pure(new BooleanLiteralExpr(value))

    def fullyQualifyPackageName(rawPkgName: NonEmptyList[String]): Target[NonEmptyList[String]] = Target.pure(rawPkgName)

    def lookupEnumDefaultValue(
        tpe: JavaTypeName,
        defaultValue: com.github.javaparser.ast.Node,
        values: RenderedEnum[JavaLanguage]
    ): Target[com.github.javaparser.ast.expr.Name] =
      (defaultValue, values) match {
        case (x: StringLiteralExpr, RenderedStringEnum(values)) =>
          values
            .find(_._1 == x.getValue)
            .fold(Target.raiseUserError[Name](s"Enumeration $tpe is not defined for default value ${x.getValue}"))(value => Target.pure(value._3))
        case (x: IntegerLiteralExpr, RenderedIntEnum(values)) =>
          values
            .find(_._1 == x.getValue.toInt)
            .fold(Target.raiseUserError[Name](s"Enumeration $tpe is not defined for default value ${x.getValue}"))(value => Target.pure(value._3))
        case (x: LongLiteralExpr, RenderedIntEnum(values)) =>
          values
            .find(_._1 == x.getValue.toLong)
            .fold(Target.raiseUserError[Name](s"Enumeration $tpe is not defined for default value ${x.getValue}"))(value => Target.pure(value._3))
        case _ =>
          Target.raiseUserError(s"Enumeration $tpe somehow has a default value that doesn't match its type")
      }

    def formatPackageName(packageName: List[String]): Target[NonEmptyList[String]] =
      Target.fromOption(NonEmptyList.fromList(packageName.map(_.escapeInvalidCharacters.toCamelCase.escapeIdentifier)), UserError("Empty packageName"))
    def formatTypeName(typeName: String, suffix: Option[String] = None): Target[String] =
      Target.pure(typeName.escapeInvalidCharacters.toPascalCase.escapeIdentifier + suffix.fold("")(_.escapeInvalidCharacters.toPascalCase.escapeIdentifier))
    def formatFieldName(fieldName: String): Target[String]         = Target.pure(fieldName.escapeInvalidCharacters.toCamelCase.escapeIdentifier)
    def formatMethodName(methodName: String): Target[String]       = Target.pure(methodName.escapeInvalidCharacters.toCamelCase.escapeIdentifier)
    def formatMethodArgName(methodArgName: String): Target[String] = Target.pure(methodArgName.escapeInvalidCharacters.toCamelCase.escapeIdentifier)
    def formatEnumName(enumValue: String): Target[String]          = Target.pure(enumValue.escapeInvalidCharacters.toSnakeCase.toUpperCase(Locale.US).escapeIdentifier)

    def parseType(tpe: Tracker[String]): Target[Option[com.github.javaparser.ast.`type`.Type]] =
      safeParseType(tpe.unwrapTracker)
        .map(Option.apply)
        .recover({
          case err =>
            println(s"Warning: Unparsable x-java-type: ${tpe.unwrapTracker} $err (${tpe.showHistory})")
            None
        })
    def parseTypeName(tpe: String): Target[Option[JavaTypeName]] = Option(tpe).map(_.trim).filterNot(_.isEmpty).traverse(safeParseTypeName)
    def pureTermName(tpe: String): Target[com.github.javaparser.ast.expr.Name] =
      Option(tpe).map(_.trim).filterNot(_.isEmpty).map(safeParseName).getOrElse(Target.raiseUserError("A structure's name is empty"))
    def pureTypeName(tpe: String): Target[JavaTypeName] =
      Option(tpe).map(_.trim).filterNot(_.isEmpty).map(safeParseTypeName).getOrElse(Target.raiseUserError("A structure's name is empty"))

    def pureMethodParameter(
        nameStr: com.github.javaparser.ast.expr.Name,
        tpe: com.github.javaparser.ast.`type`.Type,
        default: Option[com.github.javaparser.ast.Node]
    ): Target[com.github.javaparser.ast.body.Parameter] =
      safeParseSimpleName(nameStr.asString).map(name => new Parameter(new NodeList(finalModifier), tpe, name))
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
    def extractTermName(term: com.github.javaparser.ast.expr.Name): Target[String] = Target.pure(term.asString)
    def extractTermNameFromParam(param: Parameter): Target[String]                 = Target.pure(param.getNameAsString)
    def selectType(typeNames: NonEmptyList[String]): Target[com.github.javaparser.ast.`type`.Type] =
      safeParseType(typeNames.toList.mkString("."))
    def selectTerm(termNames: NonEmptyList[String]): Target[com.github.javaparser.ast.Node] =
      safeParseExpression[Expression](termNames.toList.mkString(".")).map(v => v: Node)
    def alterMethodParameterName(
        param: com.github.javaparser.ast.body.Parameter,
        name: com.github.javaparser.ast.expr.Name
    ): Target[com.github.javaparser.ast.body.Parameter] =
      safeParseSimpleName(name.asString).map(
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
        pkgName: NonEmptyList[String],
        frameworkImports: List[com.github.javaparser.ast.ImportDeclaration],
        jsonImports: List[com.github.javaparser.ast.ImportDeclaration],
        customImports: List[com.github.javaparser.ast.ImportDeclaration]
    ): Target[Option[WriteTree]] = Target.pure(None)
    def renderFrameworkImplicits(
        pkgPath: Path,
        pkgName: NonEmptyList[String],
        frameworkImports: List[com.github.javaparser.ast.ImportDeclaration],
        frameworkImplicitImportNames: List[com.github.javaparser.ast.expr.Name],
        jsonImports: List[com.github.javaparser.ast.ImportDeclaration],
        frameworkImplicits: Nothing,
        frameworkImplicitName: com.github.javaparser.ast.expr.Name
    ): Target[WriteTree] = Target.raiseUserError("Java does not support Framework Implicits")
    def renderFrameworkDefinitions(
        pkgPath: Path,
        pkgName: NonEmptyList[String],
        frameworkImports: List[com.github.javaparser.ast.ImportDeclaration],
        frameworkDefinitions: List[com.github.javaparser.ast.body.BodyDeclaration[_ <: com.github.javaparser.ast.body.BodyDeclaration[_]]],
        frameworkDefinitionsName: com.github.javaparser.ast.expr.Name
    ): Target[WriteTree] =
      for {
        pkgDecl <- buildPkgDecl(pkgName)
        cu = {
          val cu = new CompilationUnit()
          cu.setPackageDeclaration(pkgDecl)
          frameworkImports.map(cu.addImport)
          frameworkDefinitions.foreach {
            case cls: TypeDeclaration[_] => cu.addType(cls)
            case other                   => Target.raiseError(RuntimeFailure(s"Don't know how to handle $other. This is a bug."))
          }
          cu
        }
        writeTree <- prettyPrintSource(resolveFile(pkgPath)(List(s"${frameworkDefinitionsName.asString}.java")), cu)
      } yield writeTree

    def writePackageObject(
        dtoPackagePath: Path,
        pkgComponents: NonEmptyList[String],
        dtoComponents: Option[NonEmptyList[String]],
        customImports: List[com.github.javaparser.ast.ImportDeclaration],
        packageObjectImports: List[com.github.javaparser.ast.ImportDeclaration],
        protocolImports: List[com.github.javaparser.ast.ImportDeclaration],
        packageObjectContents: List[com.github.javaparser.ast.Node],
        extraTypes: List[com.github.javaparser.ast.Node]
    ): Target[Option[WriteTree]] =
      for {
        pkgDecl <- dtoComponents.traverse(buildPkgDecl)
        writeTree <- pkgDecl.traverse(
          x => prettyPrintSource(resolveFile(dtoPackagePath)(List.empty).resolve("package-info.java"), new CompilationUnit().setPackageDeclaration(x))
        )
      } yield writeTree

    private def staticifyInnerObjects(decl: BodyDeclaration[_ <: BodyDeclaration[_]]): BodyDeclaration[_ <: BodyDeclaration[_]] = {
      def rec(decl: BodyDeclaration[_ <: BodyDeclaration[_]]): Unit =
        decl match {
          case cls: ClassOrInterfaceDeclaration if !cls.isInterface =>
            cls.addModifier(Modifier.Keyword.STATIC)
            cls.getMembers.toList.foreach(rec)
          case _ =>
        }
      rec(decl)
      decl
    }

    def writeProtocolDefinition(
        outputPath: Path,
        pkgName: NonEmptyList[String],
        definitions: List[String],
        dtoComponents: NonEmptyList[String],
        imports: List[com.github.javaparser.ast.ImportDeclaration],
        protoImplicitName: Option[com.github.javaparser.ast.expr.Name],
        elem: StrictProtocolElems[JavaLanguage]
    ): Target[(List[WriteTree], List[com.github.javaparser.ast.Node])] =
      for {
        pkgDecl      <- buildPkgDecl(dtoComponents)
        showerImport <- safeParseRawImport((pkgName :+ "Shower").toList.mkString("."))
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
            staticDefns.definitions.map(staticifyInnerObjects).foreach(clsCopy.addMember)
            cu.addImport(showerImport)
            cu.addType(clsCopy)
            Option((cls.getName.getIdentifier, cu))
          case ADT(name, tpe, _, trt, staticDefns) =>
            val cu = new CompilationUnit()
            cu.setPackageDeclaration(pkgDecl)
            imports.foreach(cu.addImport)
            staticDefns.extraImports.foreach(cu.addImport)
            val trtCopy = trt.clone()
            staticDefns.definitions.map(staticifyInnerObjects).foreach(trtCopy.addMember)
            cu.addImport(showerImport)
            cu.addType(trtCopy)
            Option((name, cu))
          case RandomType(_, _) =>
            Option.empty
        }
        writeTree <- nameAndCompilationUnit.traverse {
          case (name, cu) => prettyPrintSource(resolveFile(outputPath)(dtoComponents.toList).resolve(s"$name.java"), cu)
        }
      } yield (writeTree.toList, List.empty[Statement])
    def writeClient(
        pkgPath: Path,
        pkgName: NonEmptyList[String],
        customImports: List[com.github.javaparser.ast.ImportDeclaration],
        frameworkImplicitNames: List[com.github.javaparser.ast.expr.Name],
        dtoComponents: Option[NonEmptyList[String]],
        _client: Client[JavaLanguage]
    ): Target[List[WriteTree]] = {
      val Client(pkg, clientName, imports, staticDefns, client, responseDefinitions) = _client
      for {
        pkgDecl             <- buildPkgDecl(pkgName ++ pkg)
        commonImport        <- safeParseRawImport((pkgName :+ "*").toList.mkString("."))
        dtoComponentsImport <- dtoComponents.traverse(xs => safeParseRawImport((xs :+ "*").toList.mkString(".")))
        allImports    = imports ++ (customImports :+ commonImport) ++ dtoComponentsImport
        clientClasses = client.map(_.merge).toList
        trees <- (clientClasses ++ responseDefinitions).traverse(writeClientTree(pkgPath, pkg, pkgDecl, allImports, _))
      } yield trees
    }
    def writeServer(
        pkgPath: Path,
        pkgName: NonEmptyList[String],
        customImports: List[com.github.javaparser.ast.ImportDeclaration],
        frameworkImplicitNames: List[com.github.javaparser.ast.expr.Name],
        dtoComponents: Option[NonEmptyList[String]],
        server: Server[JavaLanguage]
    ): Target[List[WriteTree]] = {
      val Server(pkg, extraImports, handlerDefinition, serverDefinitions) = server
      for {
        pkgDecl             <- buildPkgDecl(pkgName ++ pkg)
        commonImport        <- safeParseRawImport((pkgName :+ "*").toList.mkString("."))
        dtoComponentsImport <- dtoComponents.traverse(xs => safeParseRawImport((xs :+ "*").toList.mkString(".")))
        allImports = extraImports ++ List(commonImport) ++ dtoComponentsImport ++ customImports
        handlerTree <- writeServerTree(pkgPath, pkg, pkgDecl, allImports, handlerDefinition)
        serverTrees <- serverDefinitions.traverse(writeServerTree(pkgPath, pkg, pkgDecl, allImports, _))
      } yield handlerTree +: serverTrees
    }

    def wrapToObject(
        name: com.github.javaparser.ast.expr.Name,
        imports: List[com.github.javaparser.ast.ImportDeclaration],
        definitions: List[com.github.javaparser.ast.body.BodyDeclaration[_ <: com.github.javaparser.ast.body.BodyDeclaration[_]]]
    ): Target[Option[Nothing]] =
      Target.pure(Option.empty[Nothing])
  }
}
