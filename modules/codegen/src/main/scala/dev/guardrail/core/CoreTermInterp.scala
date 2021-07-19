package dev.guardrail.core

import cats.data.{ NonEmptyList, State }
import cats.syntax.all._
import cats.{ FlatMap, Monad }
import dev.guardrail.{
  Args,
  CodegenTarget,
  Common,
  Context,
  Error,
  MissingArg,
  NoArgsSpecified,
  NoFramework,
  PrintHelp,
  ReadSwagger,
  Target,
  UnknownArguments,
  UnknownFramework,
  UnparseableArgument,
  WriteTree
}
import dev.guardrail.languages.LA
import dev.guardrail.terms._
import dev.guardrail.generators.Framework
import java.nio.file.Paths

import dev.guardrail.protocol.terms.protocol.{ PropertyRequirement, ProtocolSupportTerms }

import scala.util.control.NonFatal

class CoreTermInterp[L <: LA](
    val defaultFramework: String,
    val handleModules: NonEmptyList[String] => Target[Framework[L, Target]],
    val frameworkMapping: PartialFunction[String, Framework[L, Target]],
    val handleImport: String => Either[Error, L#Import]
) extends CoreTerms[L, Target] { self =>
  implicit def MonadF: Monad[Target] = Target.targetInstances

  def extendWith(
      defaultFramework: String = self.defaultFramework,
      handleModules: NonEmptyList[String] => Target[Framework[L, Target]] = self.handleModules,
      additionalFrameworkMappings: PartialFunction[String, Framework[L, Target]] = PartialFunction.empty,
      handleImport: String => Either[Error, L#Import] = self.handleImport
  ): CoreTermInterp[L] = new CoreTermInterp[L](defaultFramework, handleModules, additionalFrameworkMappings.orElse(self.frameworkMapping), handleImport)

  def getDefaultFramework =
    Target.log.function("getDefaultFramework") {
      Target.log.debug(s"Providing ${defaultFramework}") >> Target.pure(Some(defaultFramework))
    }

  def extractGenerator(context: Context, vendorDefaultFramework: Option[String]) =
    Target.log.function("extractGenerator") {
      for {
        _ <- Target.log.debug("Looking up framework")
        framework <- NonEmptyList
          .fromList(context.modules)
          .toRight(context.framework)
          .bitraverse(
            ctxFramework =>
              for {
                frameworkName <- Target.fromOption(ctxFramework.orElse(vendorDefaultFramework), NoFramework)
                framework     <- Target.fromOption(PartialFunction.condOpt(frameworkName)(frameworkMapping), UnknownFramework(frameworkName))
                _             <- Target.log.debug(s"Found: $framework")
              } yield framework,
            handleModules
          )
      } yield framework.merge
    }

  def validateArgs(parsed: List[Args]) =
    for {
      args <- Target.pure(parsed.filterNot(_.defaults))
      args <- Target.fromOption(NonEmptyList.fromList(args.filterNot(Args.isEmpty)), NoArgsSpecified)
      args <- if (args.exists(_.printHelp))
        Target.raiseError[NonEmptyList[Args]](PrintHelp)
      else Target.pure(args)
    } yield args

  def parseArgs(args: Array[String]) = {
    def expandTilde(path: String): String =
      path.replaceFirst("^~", System.getProperty("user.home"))
    val defaultArgs =
      Args.empty.copy(context = Args.empty.context, defaults = true)

    type From = (List[Args], List[String])
    type To   = List[Args]
    val start: From = (List.empty[Args], args.toList)
    import Target.log.debug
    Target.log.function("parseArgs") {
      FlatMap[Target].tailRecM[From, To](start)({
        case pair @ (sofars, rest) =>
          val empty = sofars
            .filter(_.defaults)
            .reverse
            .headOption
            .getOrElse(defaultArgs)
            .copy(defaults = false)
          def Continue(x: From): Target[Either[From, To]] = Target.pure(Either.left(x))
          def Return(x: To): Target[Either[From, To]]     = Target.pure(Either.right(x))
          def Bail(x: Error): Target[Either[From, To]]    = Target.raiseError(x)
          for {
            _ <- debug(s"Processing: ${rest.take(5).mkString(" ")}${if (rest.length > 3) "..." else ""} of ${rest.length}")
            step <- pair match {
              case (already, Nil) =>
                debug("Finished") >> Return(already)
              case (Nil, xs @ (_ :: _)) => Continue((empty :: Nil, xs))
              case (sofar :: already, "--defaults" :: xs) =>
                Continue((empty.copy(defaults = true) :: sofar :: already, xs))
              case (sofar :: already, "--client" :: xs) =>
                Continue((empty :: sofar :: already, xs))
              case (sofar :: already, "--server" :: xs) =>
                Continue((empty.copy(kind = CodegenTarget.Server) :: sofar :: already, xs))
              case (sofar :: already, "--models" :: xs) =>
                Continue((empty.copy(kind = CodegenTarget.Models) :: sofar :: already, xs))
              case (sofar :: already, "--framework" :: value :: xs) =>
                Continue((sofar.copyContext(framework = Some(value)) :: already, xs))
              case (sofar :: already, "--help" :: xs) =>
                Continue((sofar.copy(printHelp = true) :: already, List.empty))
              case (sofar :: already, "--specPath" :: value :: xs) =>
                Continue((sofar.copy(specPath = Option(expandTilde(value))) :: already, xs))
              case (sofar :: already, "--tracing" :: xs) =>
                Continue((sofar.copyContext(tracing = true) :: already, xs))
              case (sofar :: already, "--outputPath" :: value :: xs) =>
                Continue((sofar.copy(outputPath = Option(expandTilde(value))) :: already, xs))
              case (sofar :: already, "--packageName" :: value :: xs) =>
                Continue((sofar.copy(packageName = Option(value.trim.split('.').toList)) :: already, xs))
              case (sofar :: already, "--dtoPackage" :: value :: xs) =>
                Continue((sofar.copy(dtoPackage = value.trim.split('.').toList) :: already, xs))
              case (sofar :: already, "--import" :: value :: xs) =>
                Continue((sofar.copy(imports = sofar.imports :+ value) :: already, xs))
              case (sofar :: already, "--module" :: value :: xs) =>
                Continue((sofar.copyContext(modules = sofar.context.modules :+ value) :: already, xs))
              case (sofar :: already, "--custom-extraction" :: xs) =>
                Continue((sofar.copyContext(customExtraction = true) :: already, xs))
              case (sofar :: already, (arg @ "--optional-encode-as") :: value :: xs) =>
                for {
                  propertyRequirement <- parseOptionalProperty(arg, value)
                  res                 <- Continue((sofar.copyPropertyRequirement(encoder = propertyRequirement) :: already, xs))
                } yield res
              case (sofar :: already, (arg @ "--optional-decode-as") :: value :: xs) =>
                for {
                  propertyRequirement <- parseOptionalProperty(arg, value)
                  res                 <- Continue((sofar.copyPropertyRequirement(decoder = propertyRequirement) :: already, xs))
                } yield res
              case (_, unknown) =>
                debug("Unknown argument") >> Bail(UnknownArguments(unknown))
            }
          } yield step
      })
    }
  }

  private def parseOptionalProperty(arg: String, value: String): Target[PropertyRequirement.OptionalRequirement] =
    value match {
      case "required-nullable" => Target.pure(PropertyRequirement.RequiredNullable)
      case "optional"          => Target.pure(PropertyRequirement.Optional)
      case "legacy"            => Target.pure(PropertyRequirement.OptionalLegacy)
      case _                   => Target.raiseError(UnparseableArgument(arg, "Expected one of required-nullable, optional or legacy"))
    }

  private def verifyPropertyRequirement: PropertyRequirement.Configured => Target[Unit] = {
    val mapping: PropertyRequirement.OptionalRequirement => String = {
      case PropertyRequirement.Optional         => "optional"
      case PropertyRequirement.OptionalLegacy   => "legacy"
      case PropertyRequirement.RequiredNullable => "required-nullable"
    }
    {
      case PropertyRequirement.Configured(PropertyRequirement.Optional, decoder) if decoder != PropertyRequirement.Optional =>
        Target.log.warning(
          s"--optional-encode-as ${mapping(PropertyRequirement.Optional)} was used, which does not match value of --optional-decode-as ${mapping(decoder)}. This will result in the use of `Option[T]` as opposed to regular `Presence[T]`."
        )
      case PropertyRequirement.Configured(encoder, PropertyRequirement.Optional) if encoder != PropertyRequirement.Optional =>
        Target.log.warning(
          s"--optional-decode-as ${mapping(PropertyRequirement.Optional)} was used, which does not match value of --optional-encode-as ${mapping(encoder)}. This will result in the use of `Option[T]` as opposed to regular `Presence[T]`."
        )
      case _ => Target.pure(())
    }
  }

  def processArgSet(targetInterpreter: Framework[L, Target])(args: Args): Target[ReadSwagger[Target[List[WriteTree]]]] = {
    import scala.meta.parsers.Parsed
    implicit def parsed2Either[Z]: Parsed[Z] => Either[Parsed.Error, Z] = {
      case x: Parsed.Error      => Left(x)
      case Parsed.Success(tree) => Right(tree)
    }
    Target.log.function("processArgSet")(for {
      _          <- Target.log.debug("Processing arguments")
      specPath   <- Target.fromOption(args.specPath, MissingArg(args, Error.ArgName("--specPath")))
      outputPath <- Target.fromOption(args.outputPath, MissingArg(args, Error.ArgName("--outputPath")))
      pkgName    <- Target.fromOption(args.packageName, MissingArg(args, Error.ArgName("--packageName")))
      kind       = args.kind
      dtoPackage = args.dtoPackage
      context    = args.context
      _ <- verifyPropertyRequirement(context.propertyRequirement)
      customImports <- args.imports
        .traverse(
          x =>
            for {
              _ <- Target.log.debug(s"Attempting to parse $x as an import directive")
              customImport <- handleImport(x)
                .fold[Target[L#Import]](err => Target.raiseError(UnparseableArgument("import", err.toString)), Target.pure _)
            } yield customImport
        )
      _ <- Target.log.debug("Finished processing arguments")
    } yield {
      ReadSwagger(
        Paths.get(specPath), {
          swagger =>
            try {
              import targetInterpreter._
              val Sw = implicitly[SwaggerTerms[L, Target]]
              val Sc = implicitly[LanguageTerms[L, Target]]
              val Ps = implicitly[ProtocolSupportTerms[L, Target]]
              for {
                _                  <- Sw.log.debug("Running guardrail codegen")
                formattedPkgName   <- Sc.formatPackageName(pkgName)
                definitionsPkgName <- Sc.fullyQualifyPackageName(formattedPkgName)
                (proto, codegen) <- Common
                  .prepareDefinitions[L, Target](
                    kind,
                    context,
                    Tracker(swagger),
                    definitionsPkgName.toList ++ ("definitions" :: dtoPackage),
                    definitionsPkgName :+ "support"
                  )
                protocolSupport <- Ps.generateSupportDefinitions()
                result <- Common
                  .writePackage[L, Target](proto, codegen, context)(Paths.get(outputPath), formattedPkgName.toList, dtoPackage, customImports, protocolSupport)
              } yield result
            } catch {
              case NonFatal(ex) =>
                val stackTrace =
                  ex.getStackTrace()
                    .toList
                    .foldLeftM[State[Option[String], *], List[String]](List.empty)({
                      case (acc, next) =>
                        for {
                          lastClassName <- State.get
                          _             <- State.set(Option(next.getClassName()))
                        } yield {
                          if (next.getClassName().startsWith("dev.guardrail")) {
                            acc :+ s"        at ${next.toString()}"
                          } else {
                            if (lastClassName.exists(_.startsWith("dev.guardrail"))) {
                              acc :+ "          ..."
                            } else acc
                          }
                        }
                    })
                    .runA(Option.empty)
                    .value
                Target.raiseException(s"""
                  |Error attempting to process ${specPath}:
                  |
                  |${ex.toString()}
                  |${stackTrace.mkString("\n")}
                  |""".stripMargin.trim)
            }
        }
      )
    })
  }
}
