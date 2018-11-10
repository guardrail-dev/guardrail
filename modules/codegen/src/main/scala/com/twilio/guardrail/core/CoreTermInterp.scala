package com.twilio.guardrail
package core

import cats.data.NonEmptyList
import cats.instances.all._
import cats.syntax.flatMap._
import cats.syntax.either._
import cats.syntax.traverse._
import cats.{ FlatMap, ~> }
import com.twilio.guardrail.generators.{ AkkaHttp, Http4s }
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.terms._
import java.nio.file.Paths
import scala.io.AnsiColor
import scala.meta._

object CoreTermInterp extends (CoreTerm ~> CoreTarget) {
  def apply[T](x: CoreTerm[T]): CoreTarget[T] = x match {
    case GetDefaultFramework =>
      CoreTarget.log.debug("core", "extractGenerator")("Using default framework") >> CoreTarget
        .pure("akka-http")

    case ExtractGenerator(context) =>
      for {
        _ <- CoreTarget.log.debug("core", "extractGenerator")("Looking up framework")
        framework <- context.framework.fold(CoreTarget.raiseError[cats.arrow.FunctionK[CodegenApplication, Target]](NoFramework))({
          case "akka-http" => CoreTarget.pure(AkkaHttp)
          case "http4s"    => CoreTarget.pure(Http4s)
          case unknown     => CoreTarget.raiseError(UnknownFramework(unknown))
        })
        _ <- CoreTarget.log.debug("core", "extractGenerator")(s"Found: $framework")
      } yield framework

    case ValidateArgs(parsed) =>
      for {
        args <- CoreTarget.pure(parsed.filterNot(_.defaults))
        args <- CoreTarget.fromOption(NonEmptyList.fromList(args.filterNot(Args.isEmpty)), NoArgsSpecified)
        args <- if (args.exists(_.printHelp))
          CoreTarget.raiseError[NonEmptyList[Args]](PrintHelp)
        else CoreTarget.pure(args)
      } yield args

    case ParseArgs(args, defaultFramework) => {
      def expandTilde(path: String): String =
        path.replaceFirst("^~", System.getProperty("user.home"))
      val defaultArgs =
        Args.empty.copy(context = Args.empty.context.copy(framework = Some(defaultFramework)), defaults = true)

      type From = (List[Args], List[String])
      type To   = List[Args]
      val start: From = (List.empty[Args], args.toList)
      import CoreTarget.log.debug
      FlatMap[CoreTarget].tailRecM[From, To](start)({
        case pair @ (sofar, rest) =>
          val empty = sofar
            .filter(_.defaults)
            .reverse
            .headOption
            .getOrElse(defaultArgs)
            .copy(defaults = false)
          def Continue(x: From): CoreTarget[Either[From, To]] = CoreTarget.pure(Either.left(x))
          def Return(x: To): CoreTarget[Either[From, To]]     = CoreTarget.pure(Either.right(x))
          def Bail(x: Error): CoreTarget[Either[From, To]]    = CoreTarget.raiseError(x)
          for {
            _ <- debug("core", "parseArgs")(s"Processing: ${rest.take(5).mkString(" ")}${if (rest.length > 3) "..." else ""} of ${rest.length}")
            step <- pair match {
              case (already, Nil) =>
                debug("core", "parseArgs")("Finished") >> Return(already)
              case (Nil, xs @ (_ :: _)) => Continue((empty :: Nil, xs))
              case (sofar :: already, "--defaults" :: xs) =>
                Continue((empty.copy(defaults = true) :: sofar :: already, xs))
              case (sofar :: already, "--client" :: xs) =>
                Continue((empty :: sofar :: already, xs))
              case (sofar :: already, "--server" :: xs) =>
                Continue((empty.copy(kind = CodegenTarget.Server) :: sofar :: already, xs))
              case (sofar :: already, "--framework" :: value :: xs) =>
                Continue((sofar.copy(context = sofar.context.copy(framework = Some(value))) :: already, xs))
              case (sofar :: already, "--help" :: xs) =>
                Continue((sofar.copy(printHelp = true) :: already, List.empty))
              case (sofar :: already, "--specPath" :: value :: xs) =>
                Continue((sofar.copy(specPath = Option(expandTilde(value))) :: already, xs))
              case (sofar :: already, "--tracing" :: xs) =>
                Continue((sofar.copy(context = sofar.context.copy(tracing = true)) :: already, xs))
              case (sofar :: already, "--outputPath" :: value :: xs) =>
                Continue((sofar.copy(outputPath = Option(expandTilde(value))) :: already, xs))
              case (sofar :: already, "--packageName" :: value :: xs) =>
                Continue((sofar.copy(packageName = Option(value.trim.split('.').to[List])) :: already, xs))
              case (sofar :: already, "--dtoPackage" :: value :: xs) =>
                Continue((sofar.copy(dtoPackage = value.trim.split('.').to[List]) :: already, xs))
              case (sofar :: already, "--import" :: value :: xs) =>
                Continue((sofar.copy(imports = sofar.imports :+ value) :: already, xs))
              case (_, unknown) =>
                debug("core", "parseArgs")("Unknown argument") >> Bail(UnknownArguments(unknown))
            }
          } yield step
      })
    }

    case ProcessArgSet(targetInterpreter, args) =>
      import scala.meta.parsers.Parsed
      implicit def parsed2Either[Z]: Parsed[Z] => Either[Parsed.Error, Z] = {
        case x: Parsed.Error      => Left(x)
        case Parsed.Success(tree) => Right(tree)
      }
      for {
        _          <- CoreTarget.log.debug("core", "processArgSet")("Processing arguments")
        specPath   <- CoreTarget.fromOption(args.specPath, MissingArg(args, Error.ArgName("--specPath")))
        outputPath <- CoreTarget.fromOption(args.outputPath, MissingArg(args, Error.ArgName("--outputPath")))
        pkgName    <- CoreTarget.fromOption(args.packageName, MissingArg(args, Error.ArgName("--packageName")))
        kind       = args.kind
        dtoPackage = args.dtoPackage
        context    = args.context
        customImports <- args.imports
          .map(
            x =>
              for {
                _ <- CoreTarget.log.debug("core", "processArgSet")(s"Attempting to parse $x as an import directive")
                importer <- x
                  .parse[Importer]
                  .fold[CoreTarget[Importer]](err => CoreTarget.raiseError(UnparseableArgument("import", err.toString)), CoreTarget.pure(_))
              } yield Import(List(importer))
          )
          .sequence[CoreTarget, Import]
        _ <- CoreTarget.log.debug("core", "processArgSet")("Finished processing arguments")
      } yield {
        ReadSwagger(
          Paths.get(specPath), { swagger =>
            Common
              .writePackage[ScalaLanguage, CodegenApplication](kind, context, swagger, Paths.get(outputPath), pkgName, dtoPackage, customImports)
              .foldMap(targetInterpreter)
          }
        )
      }
  }
}
