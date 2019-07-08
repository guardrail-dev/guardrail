package com.twilio.guardrail

import _root_.io.swagger.v3.oas.models.OpenAPI
import cats.data.NonEmptyList
import cats.free.Free
import cats.implicits._
import cats.Id
import com.twilio.guardrail.extract.SecurityOptional
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.protocol.terms.protocol.{ ArrayProtocolTerms, EnumProtocolTerms, ModelProtocolTerms, PolyProtocolTerms, ProtocolSupportTerms }
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.protocol.terms.client.ClientTerms
import com.twilio.guardrail.protocol.terms.server.ServerTerms
import com.twilio.guardrail.shims._
import com.twilio.guardrail.terms.{ CoreTerms, ScalaTerms, SecurityRequirements, SecurityScheme, SwaggerTerms }
import java.nio.file.Path
import scala.collection.JavaConverters._
import java.net.URI

case class SupportDefinition[L <: LA](className: L#TermName, imports: List[L#Import], definition: L#ClassDefinition)

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
      Sw: SwaggerTerms[L, F]
  ): Free[F, (ProtocolDefinitions[L], CodegenDefinitions[L])] = {
    import F._
    import Sc._
    import Sw._

    for {
      proto <- ProtocolGenerator.fromSwagger[L, F](swagger)
      ProtocolDefinitions(protocolElems, protocolImports, packageObjectImports, packageObjectContents) = proto

      serverUrls = Option(swagger.getServers)
        .map(_.asScala.toList)
        .map(_.flatMap({ x =>
          Option(x.getUrl())
            .map({ x =>
              val uri    = new URI(x.iterateWhileM[Id](_.stripSuffix("/"))(_.endsWith("/")))
              val scheme = Option(uri.getScheme).orElse(Option(uri.getHost).filterNot(_.isEmpty).map(_ => "http")).getOrElse(null) // Only force a scheme if we have a host, falling back to null as required by URI
              new URI(scheme, uri.getUserInfo, uri.getHost, uri.getPort, "", uri.getQuery, uri.getFragment)
            })
            .filterNot(_.toString().isEmpty)
        }))
        .flatMap(NonEmptyList.fromList(_))
      basePath = swagger.basePath()

      paths                      = swagger.getPathsOpt()
      globalSecurityRequirements = Option(swagger.getSecurity).flatMap(SecurityRequirements(_, SecurityOptional(swagger), SecurityRequirements.Global))
      requestBodies    <- extractCommonRequestBodies(Option(swagger.getComponents))
      routes           <- extractOperations(paths, requestBodies, globalSecurityRequirements)
      prefixes         <- vendorPrefixes()
      securitySchemes  <- SwaggerUtil.extractSecuritySchemes(swagger, prefixes)
      classNamedRoutes <- routes.traverse(route => getClassName(route.operation, prefixes).map(_ -> route))
      groupedRoutes = classNamedRoutes
        .groupBy(_._1)
        .mapValues(_.map(_._2))
        .toList
      frameworkImports <- getFrameworkImports(context.tracing)

      codegen <- kind match {
        case CodegenTarget.Client =>
          for {
            clientMeta <- ClientGenerator
              .fromSwagger[L, F](context, frameworkImports)(serverUrls, basePath, groupedRoutes)(protocolElems, securitySchemes)
            Clients(clients, supportDefinitions) = clientMeta
            frameworkImplicits <- getFrameworkImplicits()
          } yield CodegenDefinitions[L](clients, List.empty, supportDefinitions, frameworkImplicits)

        case CodegenTarget.Server =>
          for {
            serverMeta <- ServerGenerator
              .fromSwagger[L, F](context, swagger, frameworkImports)(groupedRoutes)(protocolElems, securitySchemes)
            Servers(servers, supportDefinitions) = serverMeta
            frameworkImplicits <- getFrameworkImplicits()
          } yield CodegenDefinitions[L](List.empty, servers, supportDefinitions, frameworkImplicits)
        case CodegenTarget.Models =>
          Free.pure[F, CodegenDefinitions[L]](CodegenDefinitions[L](List.empty, List.empty, List.empty, Option.empty))
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
    val CodegenDefinitions(clients, servers, supportDefinitions, frameworkImplicits)                     = codegen
    val frameworkImplicitName                                                                            = frameworkImplicits.map(_._1)

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

      frameworkImports     <- getFrameworkImports(context.tracing)
      frameworkDefinitions <- getFrameworkDefinitions(context.tracing)

      files <- (clients.flatTraverse(writeClient(pkgPath, pkgName, customImports, frameworkImplicitName, dtoComponents, _)),
                servers.flatTraverse(writeServer(pkgPath, pkgName, customImports, frameworkImplicitName, dtoComponents, _))).mapN(_ ++ _)

      implicits <- renderImplicits(pkgPath, pkgName, frameworkImports, protocolImports, customImports)
      frameworkImplicitsFile <- frameworkImplicits.fold(Free.pure[F, Option[WriteTree]](None))({
        case (name, defn) => renderFrameworkImplicits(pkgPath, pkgName, frameworkImports, protocolImports, defn, name).map(Option.apply)
      })

      frameworkDefinitionsFiles <- frameworkDefinitions.traverse({
        case (name, defn) => renderFrameworkDefinitions(pkgPath, pkgName, frameworkImports, defn, name)
      })
      supportDefinitionsFiles <- supportDefinitions.traverse({
        case SupportDefinition(name, imports, defn) => renderFrameworkDefinitions(pkgPath, pkgName, imports, defn, name)
      })
    } yield
      (
        protocolDefinitions ++
          packageObject.toList ++
          files ++
          implicits.toList ++
          frameworkImplicitsFile.toList ++
          frameworkDefinitionsFiles ++
          supportDefinitionsFiles
      ).toList
  }

  def processArgs[L <: LA, F[_]](
      args: NonEmptyList[Args]
  )(implicit C: CoreTerms[L, F]): Free[F, NonEmptyList[ReadSwagger[Target[List[WriteTree]]]]] = {
    import C._
    args.traverse(
      arg =>
        for {
          defaultFramework  <- getDefaultFramework
          targetInterpreter <- extractGenerator(arg.context, defaultFramework)
          writeFile         <- processArgSet(targetInterpreter)(arg)
        } yield writeFile
    )
  }

  def runM[L <: LA, F[_]](
      args: NonEmptyList[Args]
  )(implicit C: CoreTerms[L, F]): Free[F, NonEmptyList[ReadSwagger[Target[List[WriteTree]]]]] = {
    import C._

    for {
      validated  <- validateArgs(args.toList)
      writeTrees <- processArgs(validated)
    } yield writeTrees
  }
}
