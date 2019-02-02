package com.twilio.guardrail

import _root_.io.swagger.v3.oas.models.OpenAPI
import cats.data.NonEmptyList
import cats.free.Free
import cats.implicits._
import cats.~>
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.protocol.terms.protocol.{ ArrayProtocolTerms, EnumProtocolTerms, ModelProtocolTerms, PolyProtocolTerms, ProtocolSupportTerms }
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.protocol.terms.client.ClientTerms
import com.twilio.guardrail.protocol.terms.server.ServerTerms
import com.twilio.guardrail.shims._
import com.twilio.guardrail.terms.{ CoreTerms, ScalaTerms, SwaggerTerms }
import java.nio.file.{ Path, Paths }
import java.util.Locale
import scala.collection.JavaConverters._
import scala.io.AnsiColor
import scala.meta._
import java.net.URI

object Common {
  val resolveFile: Path => List[String] => Path = root => _.foldLeft(root)(_.resolve(_))

  def prepareDefinitions[L <: LA, F[_]](kind: CodegenTarget, context: Context, swagger: OpenAPI)(
      implicit
      C: ClientTerms[L, F],
      R: ArrayProtocolTerms[L, F],
      E: EnumProtocolTerms[L, F],
      F: FrameworkTerms[L, F],
      M: ModelProtocolTerms[L, F],
      Pol: PolyProtocolTerms[L, F],
      S: ProtocolSupportTerms[L, F],
      Sc: ScalaTerms[L, F],
      Se: ServerTerms[L, F],
      Sw: SwaggerTerms[L, F]): Free[F, (ProtocolDefinitions[L], CodegenDefinitions[L])] = {
    import F._
    import Sw._

    for {
      proto <- ProtocolGenerator.fromSwagger[L, F](swagger)
      ProtocolDefinitions(protocolElems, protocolImports, packageObjectImports, packageObjectContents) = proto

      serverUrls = Option(swagger.getServers)
        .map(_.asScala.toList)
        .map(_.flatMap({ x => Option(x.getUrl().stripSuffix("/")).filter(_.nonEmpty) }))
        .flatMap(NonEmptyList.fromList(_))
        .map(_.map({ x =>
          val uri = new URI(x)
          new URI(Option(uri.getScheme).getOrElse("http"), uri.getUserInfo, uri.getHost, uri.getPort, uri.getPath, uri.getQuery, uri.getFragment)
        }))
      basePath = swagger.basePath()

      paths = swagger.getPathsOpt()
      routes           <- extractOperations(paths)
      classNamedRoutes <- routes.traverse(route => getClassName(route.operation).map(_ -> route))
      groupedRoutes = classNamedRoutes
        .groupBy(_._1)
        .mapValues(_.map(_._2))
        .toList
      frameworkImports <- getFrameworkImports(context.tracing)

      codegen <- kind match {
        case CodegenTarget.Client =>
          for {
            clientMeta <- ClientGenerator
              .fromSwagger[L, F](context, frameworkImports)(serverUrls, basePath, groupedRoutes)(protocolElems)
            Clients(clients) = clientMeta
          } yield CodegenDefinitions[L](clients, List.empty)

        case CodegenTarget.Server =>
          for {
            serverMeta <- ServerGenerator
              .fromSwagger[L, F](context, swagger, frameworkImports)(protocolElems)
            Servers(servers) = serverMeta
          } yield CodegenDefinitions[L](List.empty, servers)
        case CodegenTarget.Models => Free.pure[F, CodegenDefinitions[L]](CodegenDefinitions[L](List.empty, List.empty))
      }
    } yield (proto, codegen)
  }

  def writePackage[L <: LA, F[_]](proto: ProtocolDefinitions[L], codegen: CodegenDefinitions[L], context: Context)(
      outputPath: Path,
      pkgName: List[String],
      dtoPackage: List[String],
      customImports: List[L#Import]
  )(implicit Sc: ScalaTerms[L, F], F: FrameworkTerms[L, F]): Free[F, List[WriteTree]] = {
    import F._
    import Sc._

    val pkgPath        = resolveFile(outputPath)(pkgName)
    val dtoPackagePath = resolveFile(pkgPath.resolve("definitions"))(dtoPackage)

    val definitions: List[String]   = pkgName :+ "definitions"
    val dtoComponents: List[String] = definitions ++ dtoPackage

    val ProtocolDefinitions(protocolElems, protocolImports, packageObjectImports, packageObjectContents) = proto
    val CodegenDefinitions(clients, servers)                                                             = codegen

    for {
      protoOut <- protocolElems.traverse(writeProtocolDefinition(outputPath, pkgName, definitions, dtoComponents, customImports ++ protocolImports, _))
      (protocolDefinitions, extraTypes) = protoOut.foldLeft((List.empty[WriteTree], List.empty[L#Statement]))(_ |+| _)
      packageObject <- writePackageObject(
        dtoPackagePath,
        dtoComponents,
        customImports,
        packageObjectImports,
        protocolImports,
        packageObjectContents,
        extraTypes
      )

      frameworkImports    <- getFrameworkImports(context.tracing)
      _frameworkImplicits <- getFrameworkImplicits()
      (frameworkImplicitName, frameworkImplicits) = _frameworkImplicits

      files <- (clients.traverse(writeClient(pkgPath, pkgName, customImports, frameworkImplicitName, dtoComponents, _)),
                servers.traverse(writeServer(pkgPath, pkgName, customImports, frameworkImplicitName, dtoComponents, _))).mapN(_ ++ _)

      implicits              <- renderImplicits(pkgPath, pkgName, frameworkImports, protocolImports, customImports)
      frameworkImplicitsFile <- renderFrameworkImplicits(pkgPath, pkgName, frameworkImports, protocolImports, frameworkImplicits, frameworkImplicitName)
    } yield
      (
        protocolDefinitions ++
          List(packageObject) ++
          files ++
          List(
            implicits,
            frameworkImplicitsFile
          )
      ).toList
  }

  def processArgs[L <: LA, F[_]](
      args: NonEmptyList[Args]
  )(implicit C: CoreTerms[L, F]): Free[F, NonEmptyList[ReadSwagger[Target[List[WriteTree]]]]] = {
    import C._
    args.traverse(
      arg =>
        for {
          targetInterpreter <- extractGenerator(arg.context)
          writeFile         <- processArgSet(targetInterpreter)(arg)
        } yield writeFile
    )
  }

  def runM[L <: LA, F[_]](
      args: Array[String]
  )(implicit C: CoreTerms[L, F]): Free[F, NonEmptyList[ReadSwagger[Target[List[WriteTree]]]]] = {
    import C._

    for {
      defaultFramework <- getDefaultFramework
      parsed           <- parseArgs(args, defaultFramework)
      args             <- validateArgs(parsed)
      writeTrees       <- processArgs(args)
    } yield writeTrees
  }
}
