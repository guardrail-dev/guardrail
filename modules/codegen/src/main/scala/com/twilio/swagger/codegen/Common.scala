package com.twilio.swagger.codegen

import _root_.io.swagger.models.Swagger
import cats.data.NonEmptyList
import cats.free.Free
import cats.instances.all._
import cats.syntax.either._
import cats.syntax.semigroup._
import cats.syntax.traverse._
import cats.~>
import com.twilio.swagger.codegen.terms.{CoreTerm, CoreTerms, ScalaTerms}
import java.nio.file.{Path, Paths}
import scala.io.AnsiColor
import scala.meta._

object Common {
  def writePackage(kind: CodegenTarget, context: Context, swagger: Swagger, outputPath: Path, pkgName: List[String], dtoPackage: List[String], customImports: List[Import]
      )(implicit S: ScalaTerms[CodegenApplication]
      ): Free[CodegenApplication, List[WriteTree]] = {
    import S._

    val resolveFile: Path => List[String] => Path = root => _.foldLeft(root)(_.resolve(_))
    val splitComponents: String => Option[List[String]] = x => Some(x.split('.').toList).filterNot(_.isEmpty)

    val buildPackage: String => Option[Term.Ref] = pkg => splitComponents(pkg).map(dtoPackage ++ _).map(_.map(Term.Name.apply _).reduceLeft(Term.Select.apply _))

    val pkgPath = resolveFile(outputPath)(pkgName)
    val dtoPackagePath = resolveFile(pkgPath.resolve("definitions"))(dtoPackage)

    val definitions: List[String] = pkgName :+ "definitions"
    val dtoComponents: List[String] = definitions ++ dtoPackage
    val buildPkgTerm: List[String] => Term.Ref = _.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)

    for {
      proto <- ProtocolGenerator.fromSwagger[CodegenApplication](swagger)
      ProtocolDefinitions(protocolElems, protocolImports, packageObjectImports, packageObjectContents) = proto
      implicitsImport = q"import ${buildPkgTerm(List("_root_") ++ pkgName ++ List("Implicits"))}._"
      imports = customImports ++ protocolImports ++ List(implicitsImport)

      protoOut = protocolElems.map({
        case EnumDefinition(_, _, cls, obj) =>
          (List(WriteTree(
            resolveFile(outputPath)(dtoComponents).resolve(s"${cls.name.value}.scala")
            , source"""
              package ${buildPkgTerm(definitions)}
                ..${imports}
                $cls
                $obj
              """
          )), List.empty[Stat])

        case ClassDefinition(_, cls, obj) =>
          (List(WriteTree(
            resolveFile(outputPath)(dtoComponents).resolve(s"${cls.name.value}.scala")
            , source"""
              package ${buildPkgTerm(dtoComponents)}
                ..${imports}
                $cls
                $obj
              """
          )), List.empty[Stat])

        case RandomType(_, defns) =>
          (List.empty, defns.toList)

      }).foldLeft((List.empty[WriteTree], List.empty[Stat]))(_ |+| _)
      (protocolDefinitions, extraTypes) = protoOut

      packageObject = WriteTree(dtoPackagePath.resolve("package.scala"),
        source"""package ${Term.Name(dtoComponents.dropRight(1).mkString("."))}
            ..${customImports ++ packageObjectImports ++ protocolImports}

            package object ${Term.Name(dtoComponents.last)} {
               ..${(packageObjectContents ++ extraTypes).to[List]}
             }
          """
        )

      codegen <- kind match {
        case CodegenTarget.Client =>
          for {
            clientMeta <- ClientGenerator.fromSwagger[CodegenApplication](context, swagger)(protocolElems)
            Clients(clients, frameworkImports) = clientMeta
          } yield CodegenDefinitions(clients, List.empty, frameworkImports)

        case CodegenTarget.Server =>
          for {
            serverMeta <- ServerGenerator.fromSwagger[CodegenApplication](context, swagger)(protocolElems)
            Servers(servers, frameworkImports) = serverMeta
          } yield CodegenDefinitions(List.empty, servers, frameworkImports)
      }

      CodegenDefinitions(clients, servers, frameworkImports) = codegen

      files = (
        clients
          .map({ case Client(pkg, clientName, clientSrc) =>
              WriteTree(
                resolveFile(pkgPath)(pkg :+ s"${clientName}.scala")
                , source"""
                  package ${buildPkgTerm(pkgName ++ pkg                             )}
                  import ${buildPkgTerm(List("_root_") ++ pkgName ++ List("Implicits"))}._
                  import ${buildPkgTerm(List("_root_") ++ dtoComponents              )}._
                  ..${customImports}
                  ..${clientSrc}
                  """
              )
            }).to[List]
          ) ++ (
            servers
              .map({ case Server(pkg, extraImports, src) =>
                WriteTree(resolveFile(pkgPath)(pkg.toList :+ "Routes.scala")
                  , source"""
                    package ${buildPkgTerm((pkgName ++ pkg.toList)                    )}
                    ..${extraImports}
                    import ${buildPkgTerm(List("_root_") ++ pkgName ++ List("Implicits"))}._
                    import ${buildPkgTerm(List("_root_") ++ dtoComponents              )}._
                    ..${customImports}
                    ..$src
                    """
                )
              }).to[List]
          )

      implicits <- renderImplicits(pkgName, frameworkImports, protocolImports, customImports)
    } yield (
      protocolDefinitions ++
      List(packageObject) ++
      files ++
      List(WriteTree(pkgPath.resolve("Implicits.scala"), implicits))
    ).toList
  }

  def processArgs[F[_]](args: NonEmptyList[Args])(implicit C: CoreTerms[F]): Free[F, NonEmptyList[ReadSwagger[Target[List[WriteTree]]]]] = {
    import C._
    args.map(arg =>
      for {
        targetInterpreter <- extractGenerator(arg.context)
        writeFile <- processArgSet(targetInterpreter)(arg)
      } yield writeFile
    ).sequenceU
  }

  def runM[F[_]](args: Array[String])(implicit C: CoreTerms[F]): Free[F, NonEmptyList[ReadSwagger[Target[List[WriteTree]]]]] = {
    import C._

    for {
      defaultFramework <- getDefaultFramework
      parsed <- parseArgs(args, defaultFramework)
      args <- validateArgs(parsed)
      writeTrees <- processArgs(args)
    } yield writeTrees
  }
}
