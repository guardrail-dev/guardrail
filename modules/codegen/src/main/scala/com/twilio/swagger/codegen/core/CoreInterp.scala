package com.twilio.swagger.codegen
package core

import cats.data.NonEmptyList
import cats.instances.all._
import cats.syntax.flatMap._
import cats.syntax.either._
import cats.syntax.traverse._
import cats.{~>, FlatMap}
import com.twilio.swagger.codegen.generators.AkkaHttp
import com.twilio.swagger.codegen.terms._
import java.nio.file.Paths
import scala.io.AnsiColor
import scala.meta._

object CoreTermInterp extends (CoreTerm ~> CoreTarget) {
  def apply[T](x: CoreTerm[T]): CoreTarget[T] = x match {
    case GetDefaultFramework =>
      CoreTarget.pure("akka-http")

    case ExtractGenerator(context) =>
      context.framework match {
        case Some("akka-http") => CoreTarget.pure(AkkaHttp)
        case None => CoreTarget.error(NoFramework)
        case Some(unknown) => CoreTarget.error(UnknownFramework(unknown))
      }

    case ValidateArgs(parsed) =>
      for {
        args <- CoreTarget.pure(parsed.filterNot(_.defaults))
        args <- CoreTarget.fromOption(NonEmptyList.fromList(args.filterNot(Args.isEmpty)), NoArgsSpecified)
        args <- if (args.exists(_.printHelp)) CoreTarget.error(PrintHelp) else CoreTarget.pure(args)
      } yield args

    case ParseArgs(args, defaultFramework) => {
      def expandTilde(path: String): String = path.replaceFirst("^~", System.getProperty("user.home"))
      val defaultArgs = Args.empty.copy(context=Args.empty.context.copy(framework=Some(defaultFramework)), defaults=true)

      type From = (List[Args], List[String])
      type To = List[Args]
      val start: From = (List.empty[Args], args.toList)
      FlatMap[CoreTarget].tailRecM[From, To](start)({ case pair@(sofar, rest) =>
        val empty = sofar.filter(_.defaults).reverse.headOption.getOrElse(defaultArgs).copy(defaults=false)
        def Continue(x: From): CoreTarget[Either[From, To]] = CoreTarget.pure(Either.left(x))
        def Return(x: To): CoreTarget[Either[From, To]] = CoreTarget.pure(Either.right(x))
        def Bail(x: Error): CoreTarget[Either[From, To]] = CoreTarget.error(x)
        for {
          step <- pair match {
            case (already, Nil)                                     => Return(already)
            case (Nil, xs@(_ :: _))                                 => Continue((empty                                                              :: Nil     , xs))
            case (sofar :: already, "--defaults"             :: xs) => Continue((empty.copy(defaults=true) :: sofar                                 :: already , xs))
            case (sofar :: already, "--client"               :: xs) => Continue((empty :: sofar                                                     :: already , xs))
            case (sofar :: already, "--server"               :: xs) => Continue((empty.copy(kind=CodegenTarget.Server) :: sofar                     :: already , xs))
            case (sofar :: already, "--framework"   :: value :: xs) => Continue((sofar.copy(context     = sofar.context.copy(framework=Some(value))):: already , xs))
            case (sofar :: already, "--help"                 :: xs) => Continue((sofar.copy(printHelp   = true)                                     :: already , List.empty))
            case (sofar :: already, "--specPath"    :: value :: xs) => Continue((sofar.copy(specPath    = Option(expandTilde(value)))               :: already , xs))
            case (sofar :: already, "--tracing"              :: xs) => Continue((sofar.copy(context     = sofar.context.copy(tracing=true))         :: already , xs))
            case (sofar :: already, "--outputPath"  :: value :: xs) => Continue((sofar.copy(outputPath  = Option(expandTilde(value)))               :: already , xs))
            case (sofar :: already, "--packageName" :: value :: xs) => Continue((sofar.copy(packageName = Option(value.trim.split('.').to[List]))   :: already , xs))
            case (sofar :: already, "--dtoPackage"  :: value :: xs) => Continue((sofar.copy(dtoPackage  = value.trim.split('.').to[List])           :: already , xs))
            case (sofar :: already, "--import"      :: value :: xs) => Continue((sofar.copy(imports = sofar.imports :+ value)                       :: already , xs))
            case (_, unknown) => Bail(UnknownArguments(unknown))
          }
        } yield step
      })
    }

    case ProcessArgSet(targetInterpreter, args) =>
      import scala.meta.parsers.Parsed
      implicit def parsed2Either[Z]: Parsed[Z] => Either[Parsed.Error, Z] = {
        case x: Parsed.Error => Left(x)
        case Parsed.Success(tree) => Right(tree)
      }
      for {
        specPath <- CoreTarget.fromOption(args.specPath, MissingArg(args, Error.ArgName("--specPath")))
        outputPath <- CoreTarget.fromOption(args.outputPath, MissingArg(args, Error.ArgName("--outputPath")))
        pkgName <- CoreTarget.fromOption(args.packageName, MissingArg(args, Error.ArgName("--packageName")))
        kind = args.kind
        dtoPackage = args.dtoPackage
        context = args.context
        customImports <- args.imports.map(x =>
          for {
            importer <- x.parse[Importer].fold[CoreTarget[Importer]](err => CoreTarget.error(UnparseableArgument("import", err.toString)), CoreTarget.pure(_))
          } yield Import(List(importer))
        ).toList.sequenceU
      } yield {
        ReadSwagger(Paths.get(specPath), { swagger =>
          Common.writePackage(kind, context, swagger, Paths.get(outputPath), pkgName, dtoPackage, customImports)
            .foldMap(targetInterpreter)
        })
      }
  }
}
