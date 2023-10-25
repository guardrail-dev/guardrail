package dev.guardrail

import _root_.io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.oas.models.security.{ SecurityScheme => SwSecurityScheme }
import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all._
import cats.Id
import java.nio.file.Path
import java.net.URI

import dev.guardrail.core.{ SupportDefinition, Tracker }
import dev.guardrail.core.extract.CustomTypeName
import dev.guardrail.generators.{ Clients, Servers }
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.languages.LA
import dev.guardrail.terms.client.ClientTerms
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.protocol.RandomType
import dev.guardrail.terms.server.ServerTerms
import dev.guardrail.terms.{ CollectionsLibTerms, CoreTerms, LanguageTerms, OpenAPITerms, ProtocolTerms, SecurityRequirements, SecurityScheme }

object Common {
  val resolveFile: Path => List[String] => Path            = root => _.foldLeft(root)(_.resolve(_))
  val resolveFileNel: Path => NonEmptyList[String] => Path = root => _.foldLeft(root)(_.resolve(_))

  private[this] def extractSecuritySchemes[L <: LA, F[_]: Monad](
      spec: OpenAPI,
      prefixes: List[String]
  )(implicit Sw: OpenAPITerms[L, F], Sc: LanguageTerms[L, F]): F[Map[String, SecurityScheme[L]]] = {
    import Sw._
    import Sc._

    Tracker(spec)
      .downField("components", _.getComponents)
      .flatDownField("securitySchemes", _.getSecuritySchemes)
      .indexedDistribute
      .value
      .flatTraverse { case (schemeName, scheme) =>
        val typeName = CustomTypeName(scheme, prefixes)
        for {
          tpe <- typeName.fold(Option.empty[L#Type].pure[F])(x => parseType(Tracker.cloneHistory(scheme, x)))
          parsedScheme <- scheme.downField("type", _.getType).unwrapTracker.traverse {
            case SwSecurityScheme.Type.APIKEY        => extractApiKeySecurityScheme(schemeName, scheme, tpe).widen[SecurityScheme[L]]
            case SwSecurityScheme.Type.HTTP          => extractHttpSecurityScheme(schemeName, scheme, tpe).widen[SecurityScheme[L]]
            case SwSecurityScheme.Type.OPENIDCONNECT => extractOpenIdConnectSecurityScheme(schemeName, scheme, tpe).widen[SecurityScheme[L]]
            case SwSecurityScheme.Type.OAUTH2        => extractOAuth2SecurityScheme(schemeName, scheme, tpe).widen[SecurityScheme[L]]
            case SwSecurityScheme.Type.MUTUALTLS     => extractMutualTLSSecurityScheme(schemeName, scheme, tpe).widen[SecurityScheme[L]]
          }
        } yield parsedScheme.toList.map(scheme => schemeName -> scheme)
      }
      .map(_.toMap)
  }

  def prepareDefinitions[L <: LA, F[_]: Monad](
      kind: CodegenTarget,
      context: Context,
      spec: Tracker[OpenAPI],
      dtoPackage: List[String],
      supportPackage: NonEmptyList[String]
  )(implicit
      C: ClientTerms[L, F],
      Fw: FrameworkTerms[L, F],
      P: ProtocolTerms[L, F],
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Se: ServerTerms[L, F],
      Sw: OpenAPITerms[L, F]
  ): F[(ProtocolDefinitions[L], CodegenDefinitions[L])] = {
    import Fw.{ getFrameworkImports, getFrameworkImplicits }
    import Sw._

    Sw.log.function("prepareDefinitions")(for {
      proto @ ProtocolDefinitions(protocolElems, protocolImports, packageObjectImports, packageObjectContents, _) <- P.fromSpec(
        spec,
        dtoPackage,
        supportPackage,
        context.propertyRequirement
      )

      serverUrls = NonEmptyList.fromList(
        spec
          .downField("servers", _.getServers)
          .flatExtract(server =>
            server
              .downField("url", _.getUrl)
              .unwrapTracker
              .map { x =>
                val uri = new URI(x.iterateWhileM[Id](_.stripSuffix("/"))(_.endsWith("/")))
                @SuppressWarnings(Array("org.wartremover.warts.Null"))
                val scheme = Option(uri.getScheme)
                  .orElse(Option(uri.getHost).filterNot(_.isEmpty).map(_ => "http"))
                  .getOrElse(null) // Only force a scheme if we have a host, falling back to null as required by URI
                new URI(scheme, uri.getUserInfo, uri.getHost, uri.getPort, "", uri.getQuery, uri.getFragment)
              }
              .filterNot(_.toString().isEmpty)
              .toList
          )
      )
      basePath = spec
        .downField("servers", _.getServers)
        .cotraverse(_.downField("url", _.getUrl))
        .headOption
        .flatMap(_.unwrapTracker)
        .flatMap(url => Option(new URI(url).getPath))
        .filter(_ != "/")

      paths = spec.downField("paths", _.getPaths)
      globalSecurityRequirements = NonEmptyList
        .fromList(spec.downField("security", _.getSecurity).indexedDistribute)
        .flatMap(SecurityRequirements(_, SecurityRequirements.Global))
      components = spec.downField("components", _.getComponents)
      requestBodies    <- extractCommonRequestBodies(components)
      routes           <- extractOperations(paths, requestBodies, globalSecurityRequirements)
      prefixes         <- Cl.vendorPrefixes()
      securitySchemes  <- extractSecuritySchemes(spec.unwrapTracker, prefixes)
      classNamedRoutes <- routes.traverse(route => getClassName(route.operation, prefixes, context.tagsBehaviour).map(_ -> route))
      groupedRoutes = classNamedRoutes
        .groupMap(_._1)(_._2)
        .toList
      frameworkImports <- getFrameworkImports(context.tracing)

      codegen <- kind match {
        case CodegenTarget.Client =>
          for {
            clientMeta <- C.fromSpec(context, frameworkImports)(serverUrls, basePath, groupedRoutes)(protocolElems, securitySchemes, components)
            Clients(clients, supportDefinitions) = clientMeta
            frameworkImplicits <- getFrameworkImplicits()
          } yield CodegenDefinitions[L](clients, List.empty, supportDefinitions, frameworkImplicits)

        case CodegenTarget.Server =>
          for {
            serverMeta <- Se.fromSpec(context, supportPackage, basePath, frameworkImports)(groupedRoutes)(protocolElems, securitySchemes, components)
            Servers(servers, supportDefinitions) = serverMeta
            frameworkImplicits <- getFrameworkImplicits()
          } yield CodegenDefinitions[L](List.empty, servers, supportDefinitions, frameworkImplicits)
        case CodegenTarget.Models =>
          CodegenDefinitions[L](List.empty, List.empty, List.empty, Option.empty).pure[F]
      }
    } yield (proto, codegen))
  }

  def writePackage[L <: LA, F[_]: Monad](proto: ProtocolDefinitions[L], codegen: CodegenDefinitions[L], context: Context)(
      outputPath: Path,
      pkgName: List[String],
      dtoPackage: List[String],
      customImports: List[L#Import],
      protocolSupport: List[SupportDefinition[L]]
  )(implicit Sc: LanguageTerms[L, F], Fw: FrameworkTerms[L, F], Pt: ProtocolTerms[L, F]): F[List[WriteTree]] = {
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
      filteredDtoComponents: Option[NonEmptyList[String]] = Option(dtoComponents).filter(_ =>
        protocolElems.exists { case _: RandomType[_] => false; case _ => true }
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
      frameworkImplicitsFile <- frameworkImplicits.fold(Option.empty[WriteTree].pure[F]) { case (name, defn) =>
        renderFrameworkImplicits(pkgPath, formattedPkgName, frameworkImports, frameworkImplicitNames.filterNot(_ == name), protocolImports, defn, name)
          .map(Option.apply)
      }
      protocolImplicitsFile <- protoImplicits.fold(Option.empty[WriteTree].pure[F]) { case (name, defn) =>
        renderFrameworkImplicits(pkgPath, formattedPkgName, frameworkImports, frameworkImplicitNames.filterNot(_ == name), protocolImports, defn, name)
          .map(Option.apply)
      }
      frameworkDefinitionsFiles <- frameworkDefinitions.traverse { case (name, defn) =>
        renderFrameworkDefinitions(pkgPath, formattedPkgName, frameworkImports, defn, name)
      }

      protocolStaticImports <- Pt.staticProtocolImports(formattedPkgName.toList)

      supportDefinitionsFiles <- (supportDefinitions ++ protocolSupport).traverse {
        case SupportDefinition(name, imports, defn, true) => renderFrameworkDefinitions(pkgPath, formattedPkgName, imports ++ protocolStaticImports, defn, name)
        case SupportDefinition(name, imports, defn, false) => renderFrameworkDefinitions(supportPkgPath, formattedPkgName :+ "support", imports, defn, name)
      }
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

  def processArgs[L <: LA, F[_]: Monad](
      args: NonEmptyList[Args]
  )(implicit C: CoreTerms[L, F]): F[NonEmptyList[ReadSpec[Target[List[WriteTree]]]]] = {
    import C._
    args.traverse(arg =>
      for {
        defaultFramework  <- getDefaultFramework
        targetInterpreter <- extractGenerator(arg.context, defaultFramework)
        writeFile         <- processArgSet(targetInterpreter)(arg)
      } yield writeFile
    )
  }

  def runM[L <: LA, F[_]: Monad](
      args: NonEmptyList[Args]
  )(implicit C: CoreTerms[L, F]): F[NonEmptyList[ReadSpec[Target[List[WriteTree]]]]] = {
    import C._

    for {
      validated  <- validateArgs(args.toList)
      writeTrees <- processArgs[L, F](validated)
    } yield writeTrees
  }
}
