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
import scala.collection.immutable.Seq
import scala.io.AnsiColor
import scala.meta._

object Common {
  def writePackage(kind: CodegenTarget, context: Context, swagger: Swagger, outputPath: Path, pkgName: Seq[String], dtoPackage: Seq[String], customImports: Seq[Import]
      )(implicit S: ScalaTerms[CodegenApplication]
      ): Free[CodegenApplication, List[WriteTree]] = {
    import S._

    val resolveFile: Path => Seq[String] => Path = root => _.foldLeft(root)(_.resolve(_))
    val splitComponents: String => Option[Seq[String]] = x => Some(x.split('.').toList).filterNot(_.isEmpty)

    val buildPackage: String => Option[Term.Ref] = pkg => splitComponents(pkg).map(dtoPackage ++ _).map(_.map(Term.Name.apply _).reduceLeft(Term.Select.apply _))

    val pkgPath = resolveFile(outputPath)(pkgName)
    val dtoPackagePath = resolveFile(pkgPath.resolve("definitions"))(dtoPackage)

    val definitions: Seq[String] = pkgName :+ "definitions"
    val dtoComponents: Seq[String] = definitions ++ dtoPackage
    val buildPkgTerm: Seq[String] => Term.Ref = _.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)

    for {
      proto <- ProtocolGenerator.fromSwagger[CodegenApplication](swagger)
      ProtocolDefinitions(protocolElems, protocolImports, packageObjectImports, packageObjectContents) = proto
      implicitsImport = q"import ${buildPkgTerm(Seq("_root_") ++ pkgName ++ Seq("Implicits"))}._"
      imports = customImports ++ protocolImports ++ Seq(implicitsImport)

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
               ..${(packageObjectContents ++ extraTypes).to[Seq]}
             }
          """
        )

      codegen <- kind match {
        case CodegenTarget.Client =>
          for {
            clientMeta <- ClientGenerator.fromSwagger[CodegenApplication](context, swagger)(protocolElems)
            Clients(clients, frameworkImports) = clientMeta
          } yield CodegenDefinitions(clients, Seq.empty, frameworkImports)

        case CodegenTarget.Server =>
          for {
            serverMeta <- ServerGenerator.fromSwagger[CodegenApplication](context, swagger)
            Servers(servers, frameworkImports) = serverMeta
          } yield CodegenDefinitions(Seq.empty, servers, frameworkImports)
      }

      CodegenDefinitions(clients, servers, frameworkImports) = codegen

      files = (
        clients
          .map({ case Client(pkg, clientName, clientSrc) =>
              WriteTree(
                resolveFile(pkgPath)(pkg :+ s"${clientName}.scala")
                , source"""
                  package ${buildPkgTerm(pkgName ++ pkg                             )}
                  import ${buildPkgTerm(Seq("_root_") ++ pkgName ++ Seq("Implicits"))}._
                  import ${buildPkgTerm(Seq("_root_") ++ dtoComponents              )}._
                  ..${customImports}
                  ..${clientSrc}
                  """
              )
            }).to[Seq]
          ) ++ (
            servers
              .map({ case Server(pkg, extraImports, src) =>
                WriteTree(resolveFile(pkgPath)(pkg.toList :+ "Routes.scala")
                  , source"""
                    package ${buildPkgTerm((pkgName ++ pkg.toList)                    )}
                    ..${extraImports}
                    import ${buildPkgTerm(Seq("_root_") ++ pkgName ++ Seq("Implicits"))}._
                    import ${buildPkgTerm(Seq("_root_") ++ dtoComponents              )}._
                    ..${customImports}
                    ..$src
                    """
                )
              }).to[Seq]
          )

      implicits <- renderImplicits(pkgName, frameworkImports, protocolImports, customImports)
    } yield (
      protocolDefinitions ++
      Seq(packageObject) ++
      files ++
      Seq(WriteTree(pkgPath.resolve("Implicits.scala"), implicits))
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
