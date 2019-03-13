package com.twilio.guardrail.generators

import cats.~>
import cats.instances.list._
import cats.instances.option._
import cats.syntax.traverse._
import com.github.javaparser.ast._
import com.github.javaparser.ast.`type`.{ClassOrInterfaceType, PrimitiveType, Type, VoidType, ArrayType => AstArrayType}
import com.github.javaparser.ast.body.{BodyDeclaration, Parameter, TypeDeclaration}
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt.Statement
import com.twilio.guardrail._
import com.twilio.guardrail.Common.resolveFile
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.terms._
import java.nio.charset.StandardCharsets
import java.util

object JavaGenerator {
  object JavaInterp extends (ScalaTerm[JavaLanguage, ?] ~> Target) {
    def buildPkgDecl(parts: List[String]): Target[PackageDeclaration] = safeParseName(parts.mkString(".")).map(new PackageDeclaration(_))

    def buildMethodCall(name: String, arg: Option[Node] = None): Target[Node] = arg match {
      case Some(expr: Expression) => Target.pure(new MethodCallExpr(name, expr))
      case None => Target.pure(new MethodCallExpr(name))
      case other => Target.raiseError(s"Need expression to call '${name}' but got a ${other.getClass.getName} instead")
    }

    def apply[T](term: ScalaTerm[JavaLanguage, T]): Target[T] = term match {
      case LitString(value)        => Target.pure(new StringLiteralExpr(value))
      case LitFloat(value)         => Target.pure(new DoubleLiteralExpr(value))
      case LitDouble(value)        => Target.pure(new DoubleLiteralExpr(value))
      case LitInt(value)           => Target.pure(new IntegerLiteralExpr(value))
      case LitLong(value)          => Target.pure(new LongLiteralExpr(value))
      case LitBoolean(value)       => Target.pure(new BooleanLiteralExpr(value))
      case LiftOptionalType(value) => safeParseClassOrInterfaceType(s"java.util.Optional").map(_.setTypeArguments(new NodeList(value)))
      case LiftOptionalTerm(value) => buildMethodCall("java.util.Optional.ofNullable", Some(value))
      case EmptyOptionalTerm()     => buildMethodCall("java.util.Optional.empty")
      case LiftVectorType(value)   => safeParseClassOrInterfaceType("java.util.List").map(_.setTypeArguments(new NodeList(value)))
      case LiftVectorTerm(value)   => buildMethodCall("java.util.Collections.singletonList", Some(value))
      case LiftMapType(value)      => safeParseClassOrInterfaceType("java.util.Map").map(_.setTypeArguments(STRING_TYPE, value))
      case LookupEnumDefaultValue(tpe, defaultValue, values) => {
        // FIXME: Is there a better way to do this? There's a gap of coverage here
        defaultValue match {
          case s: StringLiteralExpr =>
            values
              .find(_._1 == s.getValue)
              .fold(Target.raiseError[Name](s"Enumeration ${tpe} is not defined for default value ${s.getValue}"))(value => Target.pure(value._3))
          case _ =>
            Target.raiseError(s"Enumeration ${tpe} somehow has a default value that isn't a string")
        }
      }
      case EmbedArray(tpe) =>
        tpe match {
          case SwaggerUtil.Deferred(tpe) =>
            Target.pure(SwaggerUtil.DeferredArray(tpe))
          case SwaggerUtil.DeferredArray(_) =>
            Target.raiseError("FIXME: Got an Array of Arrays, currently not supported")
          case SwaggerUtil.DeferredMap(_) =>
            Target.raiseError("FIXME: Got an Array of Maps, currently not supported")
        }
      case EmbedMap(tpe) =>
        tpe match {
          case SwaggerUtil.Deferred(inner) => Target.pure(SwaggerUtil.DeferredMap(inner))
          case SwaggerUtil.DeferredMap(_) =>
            Target.raiseError("FIXME: Got a map of maps, currently not supported")
          case SwaggerUtil.DeferredArray(_) =>
            Target.raiseError("FIXME: Got a map of arrays, currently not supported")
        }
      case ParseType(tpe) =>
        safeParseType(tpe)
          .map(Option.apply)
          .recover {
            case err =>
              println(s"Warning: Unparsable x-java-type: ${tpe} ${err}")
              None
          }
      case ParseTypeName(tpe) =>
        Option(tpe).map(_.trim).filterNot(_.isEmpty).traverse(safeParseName)

      case PureTermName(tpe) =>
        Option(tpe).map(_.trim).filterNot(_.isEmpty).map(_.escapeReservedWord).map(safeParseName).getOrElse(Target.raiseError("A structure's name is empty"))

      case PureTypeName(tpe) =>
        Option(tpe).map(_.trim).filterNot(_.isEmpty).map(safeParseName).getOrElse(Target.raiseError("A structure's name is empty"))

      case PureMethodParameter(nameStr, tpe, default) =>
        // FIXME: java methods do not support default param values -- what should we do here?
        safeParseSimpleName(nameStr.asString.escapeReservedWord).map(name => new Parameter(util.EnumSet.of(Modifier.FINAL), tpe, name))

      case TypeNamesEqual(a, b) =>
        Target.pure(a.asString == b.asString)

      case TypesEqual(a, b) =>
        Target.pure(a.equals(b))

      case ExtractTypeName(tpe) =>
        def extractTypeName(tpe: Type): Target[Name] = tpe match {
          case a: AstArrayType if a.getComponentType.isPrimitiveType => extractTypeName(new AstArrayType(a.getComponentType.asPrimitiveType().toBoxedType))
          case a: AstArrayType => safeParseName(a.asString)
          case ci: ClassOrInterfaceType => safeParseName(ci.getNameAsString)
          case p: PrimitiveType => safeParseName(p.toBoxedType.getNameAsString)
          case _: VoidType => safeParseName("Void")
          case _ =>
            println(s"WARN: ExtractTypeName: unhandled type ${tpe.getClass.getName}")
            safeParseName("Void")
        }

        extractTypeName(tpe).map(Option.apply)

      case ExtractTermName(term) =>
        Target.pure(term.asString)

      case AlterMethodParameterName(param, name) =>
        safeParseSimpleName(name.asString.escapeReservedWord).map(new Parameter(
          param.getTokenRange.orElse(null),
          param.getModifiers,
          param.getAnnotations,
          param.getType,
          param.isVarArgs,
          param.getVarArgsAnnotations,
          _
        ))

      case DateType()                => safeParseType("java.time.LocalDate")
      case DateTimeType()            => safeParseType("java.time.OffsetDateTime")
      case StringType(format)        => format.fold(Target.pure[Type](STRING_TYPE))(safeParseType)
      case FloatType()               => safeParseType("Float")
      case DoubleType()              => safeParseType("Double")
      case NumberType(format)        => safeParseType("java.math.BigDecimal")
      case IntType()                 => safeParseType("Integer")
      case LongType()                => safeParseType("Long")
      case IntegerType(format)       => safeParseType("java.math.BigInteger")
      case BooleanType(format)       => safeParseType("Boolean")
      case ArrayType(format)         => safeParseClassOrInterfaceType("java.util.List").map(_.setTypeArguments(new NodeList[Type](STRING_TYPE)))
      case FallbackType(tpe, format) => safeParseType(tpe)

      case WidenTypeName(tpe)     => safeParseType(tpe.asString)
      case WidenTermSelect(value) => Target.pure(value)

      case RenderImplicits(pkgPath, pkgName, frameworkImports, jsonImports, customImports) =>
        Target.pure(None)

      case RenderFrameworkImplicits(pkgPath, pkgName, frameworkImports, jsonImports, frameworkImplicits, frameworkImplicitName) =>
        Target.raiseError("Java does not support Framework Implicits")

      case RenderFrameworkDefinitions(pkgPath, pkgName, frameworkImports, frameworkDefinitions, frameworkDefinitionsName) =>
        for {
          pkgDecl <- buildPkgDecl(pkgName)
        } yield {
          val cu = new CompilationUnit()
          cu.setPackageDeclaration(pkgDecl)
          frameworkImports.map(cu.addImport)
          cu.addType(frameworkDefinitions)
          WriteTree(
            resolveFile(pkgPath)(List(s"${frameworkDefinitionsName.asString}.java")),
            cu.toString(printer).getBytes(StandardCharsets.UTF_8)
          )
        }

      case WritePackageObject(dtoPackagePath, dtoComponents, customImports, packageObjectImports, protocolImports, packageObjectContents, extraTypes) =>
        for {
          pkgDecl <- buildPkgDecl(dtoComponents)
        } yield Some(WriteTree(
          resolveFile(dtoPackagePath)(List.empty).resolve("package-info.java"),
          pkgDecl.toString(printer).getBytes(StandardCharsets.UTF_8)
        ))

      case WriteProtocolDefinition(outputPath, pkgName, definitions, dtoComponents, imports, elem) =>
        for {
          pkgDecl <- buildPkgDecl(definitions)
          showerImport <- safeParseRawImport((pkgName :+ "Shower").mkString("."))
        } yield {
          elem match {
            case EnumDefinition(_, _, _, cls, staticDefns) =>
              val cu = new CompilationUnit()
              cu.setPackageDeclaration(pkgDecl)
              imports.foreach(cu.addImport)
              staticDefns.extraImports.foreach(cu.addImport)
              cu.addImport(showerImport)
              val clsCopy = cls.clone()
              staticDefns.definitions.foreach(clsCopy.addMember)
              cu.addType(clsCopy)
              (
                List(
                  WriteTree(
                    resolveFile(outputPath)(dtoComponents).resolve(s"${cls.getName.getIdentifier}.java"),
                    cu.toString(printer).getBytes(StandardCharsets.UTF_8)
                  )
                ),
                List.empty[Statement]
              )

            case ClassDefinition(_, _, cls, staticDefns, _) =>
              val cu = new CompilationUnit()
              cu.setPackageDeclaration(pkgDecl)
              imports.foreach(cu.addImport)
              staticDefns.extraImports.foreach(cu.addImport)
              val clsCopy = cls.clone()
              staticDefns.definitions.foreach(clsCopy.addMember)
              cu.addType(clsCopy)
              (
                List(
                  WriteTree(
                    resolveFile(outputPath)(dtoComponents).resolve(s"${cls.getName.getIdentifier}.java"),
                    cu.toString(printer).getBytes(StandardCharsets.UTF_8)
                  )
                ),
                List.empty[Statement]
              )

            case ADT(name, tpe, trt, staticDefns) =>
              val cu = new CompilationUnit()
              cu.setPackageDeclaration(pkgDecl)
              imports.foreach(cu.addImport)
              staticDefns.extraImports.foreach(cu.addImport)
              val trtCopy = trt.clone()
              staticDefns.definitions.foreach(trtCopy.addMember)
              cu.addType(trtCopy)
              (
                List(
                  WriteTree(
                    resolveFile(outputPath)(dtoComponents).resolve(s"${name}.java"),
                    cu.toString(printer).getBytes(StandardCharsets.UTF_8)
                  )
                ),
                List.empty[Statement]
              )

            case RandomType(_, _) =>
              (List.empty, List.empty)
          }
        }

      case WriteClient(pkgPath,
                       pkgName,
                       customImports,
                       frameworkImplicitName,
                       dtoComponents,
                       Client(pkg, clientName, imports, staticDefns, client, responseDefinitions)) =>
        for {
          pkgDecl         <- buildPkgDecl(pkgName ++ pkg)
          commonImport <- safeParseRawImport((pkgName :+ "*").mkString("."))
          dtoComponentsImport <- safeParseRawImport((dtoComponents :+ "*").mkString("."))
        } yield {
          val cu = new CompilationUnit()
          cu.setPackageDeclaration(pkgDecl)
          imports.map(cu.addImport)
          customImports.map(cu.addImport)
          cu.addImport(commonImport)
          cu.addImport(dtoComponentsImport)
          val clientCopy = client.head.merge.clone() // FIXME: WriteClient needs to be altered to return `NonEmptyList[WriteTree]` to accommodate Java not being able to put multiple classes in the same file. Scala just jams them all together, but this could be improved over there as well.
          staticDefns.definitions.foreach(clientCopy.addMember)
          responseDefinitions.foreach(clientCopy.addMember)
          cu.addType(clientCopy)
          WriteTree(
            resolveFile(pkgPath)(pkg :+ s"${clientName}.java"),
            cu.toString(printer).getBytes(StandardCharsets.UTF_8)
          )
        }

      case WriteServer(pkgPath, pkgName, customImports, frameworkImplicitName, dtoComponents, Server(pkg, extraImports, handlerDefinition, serverDefinitions)) =>
        def writeClass(pkgDecl: PackageDeclaration, extraImports: List[ImportDeclaration], definition: TypeDeclaration[_]): WriteTree = {
          val cu = new CompilationUnit()
          cu.setPackageDeclaration(pkgDecl)
          extraImports.map(cu.addImport)
          customImports.map(cu.addImport)
          cu.addType(definition)
          WriteTree(
            resolveFile(pkgPath)(pkg :+ s"${definition.getNameAsString}.java"),
            cu.toString(printer).getBytes(StandardCharsets.UTF_8)
          )
        }

        def writeDefinition(pkgDecl: PackageDeclaration, extraImports: List[ImportDeclaration], definition: BodyDeclaration[_]): Target[WriteTree] = {
          definition match {
            case td: TypeDeclaration[_] => Target.pure(writeClass(pkgDecl, extraImports, td))
            case other => Target.raiseError(s"Class definition must be a TypeDeclaration but it is a ${other.getClass.getName}")
          }
        }

        for {
          pkgDecl <- buildPkgDecl(pkgName ++ pkg)

          commonImport <- safeParseRawImport((pkgName :+ "*").mkString("."))
          dtoComponentsImport <- safeParseRawImport((dtoComponents :+ "*").mkString("."))
          allExtraImports = extraImports ++ List(commonImport, dtoComponentsImport)

          handlerTree <- writeDefinition(pkgDecl, allExtraImports, handlerDefinition)
          serverTrees <- serverDefinitions.traverse(writeDefinition(pkgDecl, allExtraImports, _))
        } yield handlerTree +: serverTrees
    }
  }
}
