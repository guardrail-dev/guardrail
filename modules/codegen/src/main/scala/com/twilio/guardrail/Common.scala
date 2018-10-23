package com.twilio.guardrail

import _root_.io.swagger.models.Swagger
import cats.data.NonEmptyList
import cats.free.Free
import cats.instances.all._
import cats.syntax.either._
import cats.syntax.semigroup._
import cats.syntax.traverse._
import cats.~>
import com.twilio.guardrail.terms.{ CoreTerm, CoreTerms, ScalaTerms, SwaggerTerms }
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.generators.GeneratorSettings
import java.nio.file.{ Path, Paths }

import com.twilio.guardrail.protocol.terms.protocol.PolyProtocolTerms

import scala.collection.JavaConverters._
import scala.io.AnsiColor
import scala.meta._

object Common {
  def writePackage(kind: CodegenTarget,
                   context: Context,
                   swagger: Swagger,
                   outputPath: Path,
                   pkgName: List[String],
                   dtoPackage: List[String],
                   customImports: List[Import])(implicit F: FrameworkTerms[CodegenApplication],
                                                Sc: ScalaTerms[CodegenApplication],
                                                Pol: PolyProtocolTerms[CodegenApplication],
                                                Sw: SwaggerTerms[CodegenApplication]): Free[CodegenApplication, List[WriteTree]] = {
    import F._
    import Sc._
    import Sw._

    val resolveFile: Path => List[String] => Path       = root => _.foldLeft(root)(_.resolve(_))
    val splitComponents: String => Option[List[String]] = x => Some(x.split('.').toList).filterNot(_.isEmpty)

    val buildPackage: String => Option[Term.Ref] = pkg =>
      splitComponents(pkg)
        .map(dtoPackage ++ _)
        .map(_.map(Term.Name.apply _).reduceLeft(Term.Select.apply _))

    val pkgPath        = resolveFile(outputPath)(pkgName)
    val dtoPackagePath = resolveFile(pkgPath.resolve("definitions"))(dtoPackage)

    val definitions: List[String]   = pkgName :+ "definitions"
    val dtoComponents: List[String] = definitions ++ dtoPackage
    val buildPkgTerm: List[String] => Term.Ref =
      _.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)

    for {
      proto <- ProtocolGenerator.fromSwagger[CodegenApplication](swagger)
      ProtocolDefinitions(protocolElems, protocolImports, packageObjectImports, packageObjectContents) = proto
      implicitsImport                                                                                  = q"import ${buildPkgTerm(List("_root_") ++ pkgName ++ List("Implicits"))}._"
      imports                                                                                          = customImports ++ protocolImports ++ List(implicitsImport)

      protoOut = protocolElems
        .map({
          case EnumDefinition(_, _, _, cls, obj) =>
            (List(
               WriteTree(
                 resolveFile(outputPath)(dtoComponents).resolve(s"${cls.name.value}.scala"),
                 source"""
              package ${buildPkgTerm(definitions)}
                ..${imports}
                $cls
                $obj
              """
               )
             ),
             List.empty[Stat])

          case ClassDefinition(_, _, cls, obj, _) =>
            (List(
               WriteTree(
                 resolveFile(outputPath)(dtoComponents).resolve(s"${cls.name.value}.scala"),
                 source"""
              package ${buildPkgTerm(dtoComponents)}
                ..${imports}
                $cls
                $obj
              """
               )
             ),
             List.empty[Stat])

          case ADT(name, tpe, trt, obj) =>
            val polyImports: Import = q"""import cats.syntax.either._"""

            (
              List(
                WriteTree(
                  resolveFile(outputPath)(dtoComponents).resolve(s"$name.scala"),
                  source"""
                    package ${buildPkgTerm(dtoComponents)}

                    ..$imports
                    $polyImports
                    $trt
                    $obj

                  """
                )
              ),
              List.empty[Stat]
            )

          case RandomType(_, _) =>
            (List.empty, List.empty)
        })
        .foldLeft((List.empty[WriteTree], List.empty[Stat]))(_ |+| _)
      (protocolDefinitions, extraTypes) = protoOut

      packageObject = WriteTree(
        dtoPackagePath.resolve("package.scala"),
        source"""package ${dtoComponents.init.tail.foldLeft[Term.Ref](Term.Name(dtoComponents.head)) {
          case (acc, next) => Term.Select(acc, Term.Name(next))
        }}
            ..${customImports ++ packageObjectImports ++ protocolImports}

            package object ${Term.Name(dtoComponents.last)} {
               ..${(packageObjectContents ++ extraTypes).to[List]}
             }
          """
      )

      schemes = Option(swagger.getSchemes)
        .fold(List.empty[String])(_.asScala.to[List].map(_.toValue))
      host     = Option(swagger.getHost)
      basePath = Option(swagger.getBasePath)
      paths = Option(swagger.getPaths)
        .map(_.asScala.toList)
        .getOrElse(List.empty)
      routes           <- extractOperations(paths)
      classNamedRoutes <- routes.traverse(route => getClassName(route.operation).map(_ -> route))
      groupedRoutes = classNamedRoutes
        .groupBy(_._1)
        .mapValues(_.map(_._2))
        .toList
      frameworkImports   <- getFrameworkImports(context.tracing)
      frameworkImplicits <- getFrameworkImplicits()
      frameworkImplicitName = frameworkImplicits.name

      codegen <- kind match {
        case CodegenTarget.Client =>
          for {
            clientMeta <- ClientGenerator
              .fromSwagger[CodegenApplication](context, frameworkImports)(schemes, host, basePath, groupedRoutes)(protocolElems)
            Clients(clients) = clientMeta
          } yield CodegenDefinitions(clients, List.empty)

        case CodegenTarget.Server =>
          for {
            serverMeta <- ServerGenerator
              .fromSwagger[CodegenApplication](context, swagger, frameworkImports)(protocolElems)
            Servers(servers) = serverMeta
          } yield CodegenDefinitions(List.empty, servers)
      }

      CodegenDefinitions(clients, servers) = codegen

      files = (
        clients
          .map({
            case Client(pkg, clientName, clientSrc) =>
              WriteTree(
                resolveFile(pkgPath)(pkg :+ s"${clientName}.scala"),
                source"""
                  package ${buildPkgTerm(pkgName ++ pkg)}
                  import ${buildPkgTerm(List("_root_") ++ pkgName ++ List("Implicits"))}._
                  import ${buildPkgTerm(List("_root_") ++ pkgName ++ List(frameworkImplicitName.value))}._
                  import ${buildPkgTerm(List("_root_") ++ dtoComponents)}._
                  ..${customImports}
                  ..${clientSrc}
                  """
              )
          })
          .to[List]
        ) ++ (
        servers
          .map({
            case Server(pkg, extraImports, src) =>
              WriteTree(
                resolveFile(pkgPath)(pkg.toList :+ "Routes.scala"),
                source"""
                    package ${buildPkgTerm((pkgName ++ pkg.toList))}
                    ..${extraImports}
                    import ${buildPkgTerm(List("_root_") ++ pkgName ++ List("Implicits"))}._
                    import ${buildPkgTerm(List("_root_") ++ pkgName ++ List(frameworkImplicitName.value))}._
                    import ${buildPkgTerm(List("_root_") ++ dtoComponents)}._
                    ..${customImports}
                    ..$src
                    """
              )
          })
          .to[List]
        )

      implicits              <- renderImplicits(pkgName, frameworkImports, protocolImports, customImports)
      frameworkImplicitsFile <- renderFrameworkImplicits(pkgName, frameworkImports, protocolImports, frameworkImplicits)
    } yield
      (
        protocolDefinitions ++
          List(packageObject) ++
          files ++
          List(
            WriteTree(pkgPath.resolve("Implicits.scala"), implicits),
            WriteTree(pkgPath.resolve(s"${frameworkImplicitName.value}.scala"), frameworkImplicitsFile)
          )
      ).toList
  }

  def processArgs[F[_]](
      args: NonEmptyList[Args]
  )(implicit C: CoreTerms[F]): Free[F, NonEmptyList[(GeneratorSettings, ReadSwagger[Target[List[WriteTree]]])]] = {
    import C._
    args.traverse(
      arg =>
        for {
          targetInterpreter <- extractGenerator(arg.context)
          generatorSettings <- extractGeneratorSettings(arg.context)
          writeFile         <- processArgSet(targetInterpreter)(arg)
        } yield (generatorSettings, writeFile)
    )
  }

  def runM[F[_]](args: Array[String])(implicit C: CoreTerms[F]): Free[F, NonEmptyList[(GeneratorSettings, ReadSwagger[Target[List[WriteTree]]])]] = {
    import C._

    for {
      defaultFramework <- getDefaultFramework
      parsed           <- parseArgs(args, defaultFramework)
      args             <- validateArgs(parsed)
      writeTrees       <- processArgs(args)
    } yield writeTrees
  }
}
