package dev.guardrail

import _root_.io.swagger.v3.oas.models.OpenAPI
import cats.data.NonEmptyList
import cats.syntax.all._
import cats.Id
import dev.guardrail.core.Tracker
import dev.guardrail.extract.SecurityOptional
import dev.guardrail.languages.LA
import dev.guardrail.protocol.terms.protocol.{ ArrayProtocolTerms, EnumProtocolTerms, ModelProtocolTerms, PolyProtocolTerms, ProtocolSupportTerms }
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.protocol.terms.client.ClientTerms
import dev.guardrail.protocol.terms.server.ServerTerms
import dev.guardrail.terms.{ CollectionsLibTerms, CoreTerms, LanguageTerms, SecurityRequirements, SwaggerTerms }
import java.nio.file.Path
import java.net.URI

case class SupportDefinition[L <: LA](className: L#TermName, imports: List[L#Import], definition: List[L#Definition], insideDefinitions: Boolean = true)

object Common {
  val resolveFile: Path => List[String] => Path            = root => _.foldLeft(root)(_.resolve(_))
  val resolveFileNel: Path => NonEmptyList[String] => Path = root => _.foldLeft(root)(_.resolve(_))

  def prepareDefinitions[L <: LA, F[_]](
      kind: CodegenTarget,
      context: Context,
      swagger: Tracker[OpenAPI],
      dtoPackage: List[String],
      supportPackage: NonEmptyList[String]
  )(
      implicit
      C: ClientTerms[L, F],
      R: ArrayProtocolTerms[L, F],
      E: EnumProtocolTerms[L, F],
      Fw: FrameworkTerms[L, F],
      M: ModelProtocolTerms[L, F],
      Pol: PolyProtocolTerms[L, F],
      S: ProtocolSupportTerms[L, F],
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Se: ServerTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): F[(ProtocolDefinitions[L], CodegenDefinitions[L])] = {
    import Fw._
    import Cl._
    import Sw._

    Sw.log.function("prepareDefinitions")(for {
      proto @ ProtocolDefinitions(protocolElems, protocolImports, packageObjectImports, packageObjectContents, _) <- ProtocolGenerator
        .fromSwagger[L, F](swagger, dtoPackage, supportPackage, context.propertyRequirement)

      serverUrls = NonEmptyList.fromList(
        swagger
          .downField("servers", _.getServers)
          .flatExtract(
            server =>
              server
                .downField("url", _.getUrl)
                .unwrapTracker
                .map({ x =>
                  val uri = new URI(x.iterateWhileM[Id](_.stripSuffix("/"))(_.endsWith("/")))
                  @SuppressWarnings(Array("org.wartremover.warts.Null"))
                  val scheme = Option(uri.getScheme).orElse(Option(uri.getHost).filterNot(_.isEmpty).map(_ => "http")).getOrElse(null) // Only force a scheme if we have a host, falling back to null as required by URI
                  new URI(scheme, uri.getUserInfo, uri.getHost, uri.getPort, "", uri.getQuery, uri.getFragment)
                })
                .filterNot(_.toString().isEmpty)
                .toList
          )
      )
      basePath = swagger
        .downField("servers", _.getServers)
        .cotraverse(_.downField("url", _.getUrl))
        .headOption
        .flatMap(_.unwrapTracker)
        .flatMap(url => Option(new URI(url).getPath))
        .filter(_ != "/")

      paths = swagger.downField("paths", _.getPaths)
      globalSecurityRequirements = NonEmptyList
        .fromList(swagger.downField("security", _.getSecurity).unwrapTracker)
        .flatMap(SecurityRequirements(_, SecurityOptional(swagger), SecurityRequirements.Global))
      requestBodies    <- extractCommonRequestBodies(swagger.downField("components", _.getComponents))
      routes           <- extractOperations(paths, requestBodies, globalSecurityRequirements)
      prefixes         <- vendorPrefixes()
      securitySchemes  <- SwaggerUtil.extractSecuritySchemes(swagger.unwrapTracker, prefixes)
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
              .fromSwagger[L, F](context, supportPackage, basePath, frameworkImports)(groupedRoutes)(protocolElems, securitySchemes)
            Servers(servers, supportDefinitions) = serverMeta
            frameworkImplicits <- getFrameworkImplicits()
          } yield CodegenDefinitions[L](List.empty, servers, supportDefinitions, frameworkImplicits)
        case CodegenTarget.Models =>
          CodegenDefinitions[L](List.empty, List.empty, List.empty, Option.empty).pure[F]
      }
    } yield (proto, codegen))
  }

  def writePackage[L <: LA, F[_]](proto: ProtocolDefinitions[L], codegen: CodegenDefinitions[L], context: Context)(
      outputPath: Path,
      pkgName: List[String],
      dtoPackage: List[String],
      customImports: List[L#Import],
      protocolSupport: List[SupportDefinition[L]]
  )(implicit Sc: LanguageTerms[L, F], Fw: FrameworkTerms[L, F], Pt: ProtocolSupportTerms[L, F]): F[List[WriteTree]] = {
    import Fw._
    import Sc._

    for {
      formattedPkgName <- formatPackageName(pkgName)
      pkgPath        = resolveFileNel(outputPath)(formattedPkgName)
      dtoPackagePath = resolveFile(pkgPath.resolve("definitions"))(dtoPackage)
      supportPkgPath = resolveFile(pkgPath.resolve("support"))(Nil)

      definitions: NonEmptyList[String] = formattedPkgName :+ "definitions"

      ProtocolDefinitions(protocolElems, protocolImports, packageObjectImports, packageObjectContents, protoImplicits) = proto
      protoImplicitName                                                                                                = protoImplicits.map(_._1)
      CodegenDefinitions(clients, servers, supportDefinitions, frameworkImplicits)                                     = codegen
      frameworkImplicitName                                                                                            = frameworkImplicits.map(_._1)

      dtoComponents: NonEmptyList[String] = definitions ++ dtoPackage

      // Only presume ...definitions._ import is available if we have
      // protocolElems which are not just all type aliases.
      filteredDtoComponents: Option[NonEmptyList[String]] = Option(dtoComponents).filter(
        _ => protocolElems.exists({ case _: RandomType[_] => false; case _ => true })
      )

      protoOut <- protocolElems.traverse(
        writeProtocolDefinition(outputPath, formattedPkgName, definitions.toList, dtoComponents, customImports ++ protocolImports, protoImplicitName, _)
      )
      (protocolDefinitions, extraTypes) = protoOut.foldLeft((List.empty[WriteTree], List.empty[L#Statement]))(_ |+| _)
      packageObject <- writePackageObject(
        dtoPackagePath,
        formattedPkgName,
        filteredDtoComponents,
        customImports,
        packageObjectImports,
        protocolImports,
        packageObjectContents,
        extraTypes
      )

      frameworkImports     <- getFrameworkImports(context.tracing)
      frameworkDefinitions <- getFrameworkDefinitions(context.tracing)

      frameworkImplicitNames = frameworkImplicitName.toList ++ protoImplicitName.toList

      files <- (
        clients.flatTraverse(writeClient(pkgPath, formattedPkgName, customImports, frameworkImplicitNames, filteredDtoComponents, _)),
        servers.flatTraverse(writeServer(pkgPath, formattedPkgName, customImports, frameworkImplicitNames, filteredDtoComponents, _))
      ).mapN(_ ++ _)

      implicits <- renderImplicits(pkgPath, formattedPkgName, frameworkImports, protocolImports, customImports)
      frameworkImplicitsFile <- frameworkImplicits.fold(Option.empty[WriteTree].pure[F])({
        case (name, defn) =>
          renderFrameworkImplicits(pkgPath, formattedPkgName, frameworkImports, frameworkImplicitNames.filterNot(_ == name), protocolImports, defn, name)
            .map(Option.apply)
      })
      protocolImplicitsFile <- protoImplicits.fold(Option.empty[WriteTree].pure[F])({
        case (name, defn) =>
          renderFrameworkImplicits(pkgPath, formattedPkgName, frameworkImports, frameworkImplicitNames.filterNot(_ == name), protocolImports, defn, name)
            .map(Option.apply)
      })
      frameworkDefinitionsFiles <- frameworkDefinitions.traverse({
        case (name, defn) => renderFrameworkDefinitions(pkgPath, formattedPkgName, frameworkImports, defn, name)
      })

      protocolStaticImports <- Pt.staticProtocolImports(formattedPkgName.toList)

      supportDefinitionsFiles <- (supportDefinitions ++ protocolSupport).traverse({
        case SupportDefinition(name, imports, defn, true)  => renderFrameworkDefinitions(pkgPath, formattedPkgName, imports ++ protocolStaticImports, defn, name)
        case SupportDefinition(name, imports, defn, false) => renderFrameworkDefinitions(supportPkgPath, formattedPkgName :+ "support", imports, defn, name)
      })
    } yield (
      protocolDefinitions ++
          packageObject.toList ++
          files ++
          implicits.toList ++
          frameworkImplicitsFile.toList ++
          protocolImplicitsFile.toList ++
          frameworkDefinitionsFiles ++
          supportDefinitionsFiles
    ).toList
  }

  def processArgs[L <: LA, F[_]](
      args: NonEmptyList[Args]
  )(implicit C: CoreTerms[L, F]): F[NonEmptyList[ReadSwagger[Target[List[WriteTree]]]]] = {
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
  )(implicit C: CoreTerms[L, F]): F[NonEmptyList[ReadSwagger[Target[List[WriteTree]]]]] = {
    import C._

    for {
      validated  <- validateArgs(args.toList)
      writeTrees <- processArgs[L, F](validated)
    } yield writeTrees
  }
}
