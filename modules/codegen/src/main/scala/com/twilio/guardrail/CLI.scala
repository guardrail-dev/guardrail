package com.twilio.guardrail

import java.nio.file.Path
import cats.Applicative
import cats.implicits._
import cats.~>
import com.twilio.guardrail.core.CoreTermInterp
import com.twilio.guardrail.terms.{ CoreTerm, CoreTerms }
import com.twilio.swagger.core.{ LogLevel, LogLevels, StructuredLogger }
import com.twilio.guardrail.languages.{ JavaLanguage, LA, ScalaLanguage }
import scala.io.AnsiColor
import scala.util.{ Failure, Success }

object CLICommon {
  def run[L <: LA](args: Array[String])(interpreter: CoreTerm[L, ?] ~> CoreTarget): Unit = {
    val C = CoreTerms.coreTerm[L, CoreTerm[L, ?]]
    // Hacky loglevel parsing, only supports levels that come before absolutely
    // every other argument due to arguments being a small configuration
    // language themselves.
    val (levels, newArgs): (Array[String], Array[String]) =
      args.span(arg => LogLevels(arg.stripPrefix("--")).isDefined)
    val level: Option[String] = levels.lastOption.map(_.stripPrefix("--"))

    val fallback = List.empty[ReadSwagger[Target[List[WriteTree]]]]
    val runCore = for {
      defaultFramework <- C.getDefaultFramework
      args             <- C.parseArgs(newArgs, defaultFramework)
      result           <- Common.runM[L, CoreTerm[L, ?]](args)
    } yield result

    val result = runCore
      .foldMap(interpreter)
      .fold[List[ReadSwagger[Target[List[WriteTree]]]]](
        {
          case MissingArg(args, Error.ArgName(arg)) =>
            println(s"${AnsiColor.RED}Missing argument:${AnsiColor.RESET} ${AnsiColor.BOLD}${arg}${AnsiColor.RESET} (In block ${args})")
            unsafePrintHelp()
            fallback
          case NoArgsSpecified =>
            println(s"${AnsiColor.RED}No arguments specified${AnsiColor.RESET}")
            unsafePrintHelp()
            fallback
          case NoFramework =>
            println(s"${AnsiColor.RED}No framework specified${AnsiColor.RESET}")
            unsafePrintHelp()
            fallback
          case PrintHelp =>
            unsafePrintHelp()
            fallback
          case UnknownArguments(args) =>
            println(s"${AnsiColor.RED}Unknown arguments: ${args.mkString(" ")}${AnsiColor.RESET}")
            unsafePrintHelp()
            fallback
          case UnknownFramework(name) =>
            println(s"${AnsiColor.RED}Unknown framework specified: $name${AnsiColor.RESET}")
            fallback
          case UnparseableArgument(name, message) =>
            println(s"${AnsiColor.RED}Unparseable argument: --$name, $message")
            fallback
        },
        _.toList
      )

    implicit val logLevel: LogLevel = level
      .flatMap(level => LogLevels.members.find(_.level == level.toLowerCase))
      .getOrElse(LogLevels.Warning)

    val (coreLogger, deferred) = result.runEmpty

    print(coreLogger.show)

    val (logger, paths) = deferred
      .traverse({ rs =>
        def put(value: StructuredLogger): Logger[List[Path]] = {
          import cats.Id
          import cats.data.IndexedStateT
          IndexedStateT
            .modify[Id, StructuredLogger, StructuredLogger](_ |+| value)
            .map(_ => List.empty[Path])
        }
        def logRawError(err: String): Logger[List[Path]] = put(StructuredLogger.error(Nil, s"${AnsiColor.RED}${err}${AnsiColor.RESET}"))
        ReadSwagger
          .readSwagger(rs)
          .fold[Logger[List[Path]]](
            { err =>
              logRawError(s"Error in ${rs}") >> logRawError(err)
            },
            _.fold(
              {
                case (err, errorKind) =>
                  logRawError(s"Error in ${rs}") >> logRawError(err).map({ x =>
                    if (errorKind == UserError) unsafePrintHelp()
                    x
                  })
              },
              xs => Applicative[Logger].pure(xs.map(WriteTree.unsafeWriteTree))
            ).flatten
          ) <* put(StructuredLogger.reset)
      })
      .runEmpty

    print(logger.show)
  }

  def unsafePrintHelp(): Unit = {
    val text = s"""
    | ${AnsiColor.CYAN}guardrail${AnsiColor.RESET}
    |
    |  Required:
    |   --specPath path/to/[foo-swagger.json|foo-swagger.yaml] : ${AnsiColor.BOLD}Required${AnsiColor.RESET}, and must be valid
    |   --outputPath path/to/project                           : ${AnsiColor.BOLD}Required${AnsiColor.RESET}, intermediate paths will be created
    |   --packageName com.twilio.myservice.clients             : ${AnsiColor.BOLD}Required${AnsiColor.RESET}, Where to store your clients. Files will end up in the directory specified by replacing all dots with slashes.
    |
    |  Argmuent list separators:
    |   --client                                               : Start specifying arguments for a new client
    |   --server                                               : Start specifying arguments for a new server
    |
    |  Optional:
    |   --dtoPackage foo                                       : Where to put your client's DTOs. Effectively: "$${packageName}.definitions.$${dtoPackage}"
    |   --tracing                                              : Pass through tracing context to all requests
    |
    |Examples:
    |  Generate a client, put it in src/main/scala under the com.twilio.messaging.console.clients package, with OpenTracing support:
    |    guardrail --specPath client-specs/account-events-api.json --outputPath src/main/scala --packageName com.twilio.messaging.console.clients --tracing
    |
    |  Generate two clients, put both in src/main/scala, under different packages, one with tracing, one without:
    |    guardrail \\
    |      --client --specPath client-specs/account-events-api.json --outputPath src/main/scala --packageName com.twilio.messaging.console.clients.events \\
    |      --client --specPath client-specs/account-service.json --outputPath src/main/scala --packageName com.twilio.messaging.console.clients.account --tracing
    |
    |  Generate client and server routes for the same specification:
    |    guardrail \\
    |      --client --specPath client-specs/account-events-api.json --outputPath src/main/scala --packageName com.twilio.messaging.console.clients.events \\
    |      --server --specPath client-specs/account-events-api.json --outputPath src/main/scala --packageName com.twilio.messaging.console.clients.events
    |""".stripMargin

    System.err.println(text)
  }
}

trait CLICommon {
  val scalaInterpreter: CoreTerm[ScalaLanguage, ?] ~> CoreTarget
  val javaInterpreter: CoreTerm[JavaLanguage, ?] ~> CoreTarget

  val handleLanguage: PartialFunction[String, Array[String] => Unit] = {
    case "java"  => CLICommon.run(_)(javaInterpreter)
    case "scala" => CLICommon.run(_)(scalaInterpreter)
  }

  def main(args: Array[String]): Unit = {
    val (language, strippedArgs) = args.partition(handleLanguage.isDefinedAt _)
    handleLanguage(language.lastOption.getOrElse("scala"))(strippedArgs)
  }
}

object CLI extends CLICommon {
  import com.twilio.guardrail.generators.{ AkkaHttp, Endpoints, Http4s }
  import com.twilio.guardrail.generators.Java
  import scala.meta._
  val scalaInterpreter = CoreTermInterp[ScalaLanguage](
    "akka-http", {
      case "akka-http" => AkkaHttp
      case "endpoints" => Endpoints
      case "http4s"    => Http4s
    }, {
      _.parse[Importer].toEither.bimap(err => UnparseableArgument("import", err.toString), importer => Import(List(importer)))
    }
  )

  val javaInterpreter = CoreTermInterp[JavaLanguage](
    "dropwizard", {
      case "dropwizard" => Java.Dropwizard
    }, { str =>
      import com.github.javaparser.JavaParser
      import scala.util.Try
      Try(JavaParser.parseImport(s"import ${str};")) match {
        case Success(value) => Right(value)
        case Failure(t)     => Left(UnparseableArgument("import", t.getMessage))
      }
    }
  )
}
