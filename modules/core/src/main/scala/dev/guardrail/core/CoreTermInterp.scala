package dev.guardrail.core

import cats.data.{ NonEmptyList, State }
import cats.syntax.all._
import cats.Monad
import java.nio.file.Paths
import scala.util.control.NonFatal

import dev.guardrail.generators.Framework
import dev.guardrail.languages.LA
import dev.guardrail.terms._
import dev.guardrail.terms.protocol.PropertyRequirement
import dev.guardrail.{
  Args,
  Common,
  Context,
  Error,
  MissingArg,
  NoArgsSpecified,
  NoFramework,
  PrintHelp,
  ReadSwagger,
  Target,
  UnknownFramework,
  UnparseableArgument,
  WriteTree
}

class CoreTermInterp[L <: LA](
    val defaultFramework: String,
    val handleModules: NonEmptyList[String] => Target[Framework[L, Target]],
    val frameworkMapping: PartialFunction[String, Target[Framework[L, Target]]],
    val handleImport: String => Either[Error, L#Import]
) extends CoreTerms[L, Target] { self =>
  implicit def MonadF: Monad[Target] = Target.targetInstances

  def extendWith(
      defaultFramework: String = self.defaultFramework,
      handleModules: NonEmptyList[String] => Target[Framework[L, Target]] = self.handleModules,
      additionalFrameworkMappings: PartialFunction[String, Framework[L, Target]] = PartialFunction.empty,
      handleImport: String => Either[Error, L#Import] = self.handleImport
  ): CoreTermInterp[L] =
    new CoreTermInterp[L](defaultFramework, handleModules, additionalFrameworkMappings.andThen(Target.pure _).orElse(self.frameworkMapping), handleImport)

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
                framework     <- Target.fromOption(PartialFunction.condOpt(frameworkName)(frameworkMapping), UnknownFramework(frameworkName)).flatten
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
      args <-
        if (args.exists(_.printHelp))
          Target.raiseError[NonEmptyList[Args]](PrintHelp)
        else Target.pure(args)
    } yield args

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

  def processArgSet(targetInterpreter: Framework[L, Target])(args: Args): Target[ReadSwagger[Target[List[WriteTree]]]] =
    Target.log.function("processArgSet")(
      for {
        _          <- Target.log.debug("Processing arguments")
        specPath   <- Target.fromOption(args.specPath, MissingArg(args, Error.ArgName("--specPath")))
        outputPath <- Target.fromOption(args.outputPath, MissingArg(args, Error.ArgName("--outputPath")))
        pkgName    <- Target.fromOption(args.packageName, MissingArg(args, Error.ArgName("--packageName")))
        kind       = args.kind
        dtoPackage = args.dtoPackage
        context    = args.context
        _ <- verifyPropertyRequirement(context.propertyRequirement)
        customImports <- args.imports
          .traverse(x =>
            for {
              _            <- Target.log.debug(s"Attempting to parse $x as an import directive")
              customImport <- Target.fromEither(handleImport(x).left.map(err => UnparseableArgument("import", err.toString)))
            } yield customImport
          )
        _ <- Target.log.debug("Finished processing arguments")
      } yield ReadSwagger(
        Paths.get(specPath),
        swagger =>
          try {
            import targetInterpreter._
            val Sw = implicitly[SwaggerTerms[L, Target]]
            val Sc = implicitly[LanguageTerms[L, Target]]
            val Ps = implicitly[ProtocolTerms[L, Target]]
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
                  .foldLeftM[State[Option[String], *], List[String]](List.empty) { case (acc, next) =>
                    for {
                      lastClassName <- State.get
                      _             <- State.set(Option(next.getClassName()))
                    } yield
                      if (next.getClassName().startsWith("dev.guardrail")) {
                        acc :+ s"        at ${next.toString()}"
                      } else {
                        if (lastClassName.exists(_.startsWith("dev.guardrail"))) {
                          acc :+ "          ..."
                        } else acc
                      }
                  }
                  .runA(Option.empty)
                  .value
              Target.raiseException(s"""
                  |Error attempting to process ${specPath}:
                  |
                  |${ex.toString()}
                  |${stackTrace.mkString("\n")}
                  |""".stripMargin.trim)
          }
      )
    )
}
