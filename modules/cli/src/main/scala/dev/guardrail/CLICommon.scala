package dev.guardrail.cli

import java.nio.file.Path
import cats.data.NonEmptyList
import cats.syntax.all._
import scala.io.AnsiColor

import dev.guardrail._
import dev.guardrail.core.{ LogLevel, LogLevels, StructuredLogger }
import dev.guardrail.terms.CoreTerms
import dev.guardrail.languages.LA

object CLICommon {
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
    |   --framework <framework name>                           : Use one of the pre-composed frameworks
    |   --module <module name>                                 : Explicitly select libraries to satisfy composition requirements
    |   --custom-extraction                                    : Permit supplying an akka-http Directive into the generated guardrail routing layer (server only)
    |
    |Examples:
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

case class CommandLineResult(exitStatus: Int)

object CommandLineResult {
  val failure: CommandLineResult = CommandLineResult(1)
  val success: CommandLineResult = CommandLineResult(0)
}

trait CLICommon {
  def processArgs(args: Array[String]): CommandLineResult = {
    val (language, strippedArgs) = args.partition(handleLanguage.isDefinedAt _)
    handleLanguage(language.lastOption.getOrElse("scala"))(strippedArgs)
  }

  def handleLanguage: PartialFunction[String, Array[String] => CommandLineResult]

  def run[L <: LA](language: String, args: Array[String])(interpreter: CoreTerms[L, Target]): CommandLineResult = {
    val C = CoreTerms.coreTerm[L, Target](interpreter)
    // Hacky loglevel parsing, only supports levels that come before absolutely
    // every other argument due to arguments being a small configuration
    // language themselves.
    val (levels, newArgs): (Array[String], Array[String]) =
      args.span(arg => LogLevels(arg.stripPrefix("--")).isDefined)
    val level: Option[String] = levels.lastOption.map(_.stripPrefix("--"))

    level.foreach(_ => Target.loggerEnabled.set(true))

    // FIXME: The only reason we need the interpreter at all is to call parseArgs on it
    // This likely means the CLI should _not_ be part of CoreTerms. There's no reason
    // for it to be in there, as CLI is effectively a bespoke build tool whose unused
    // code is included into all other build tools.
    val coreArgs = C.parseArgs(newArgs).map(NonEmptyList.fromList(_))

    implicit val logLevel: LogLevel = level
      .flatMap(level => LogLevels.members.find(_.level == level.toLowerCase))
      .getOrElse(LogLevels.Warning)

    val result = coreArgs
      .flatMap({ args =>
        guardrailRunner(args.map(language -> _).toMap)
      })

    val fallback = List.empty[Path]
    import CLICommon.unsafePrintHelp
    val paths = result
      .fold(
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
            println(s"${AnsiColor.RED}Unparseable argument: --$name, $message${AnsiColor.RESET}")
            fallback
          case RuntimeFailure(message) =>
            println(s"${AnsiColor.RED}Error: $message${AnsiColor.RESET}")
            fallback
          case UserError(message) =>
            println(s"${AnsiColor.RED}Error: $message${AnsiColor.RESET}")
            unsafePrintHelp()
            fallback
          case UnconsumedModules(modules) =>
            println(s"${AnsiColor.RED}Error: Unconsumed modules: ${modules.mkString(", ")}${AnsiColor.RESET}")
            fallback
          case MissingModule(section, choices) =>
            println(s"${AnsiColor.RED}Error: Missing module ${section} (options are: ${choices.mkString(",")})${AnsiColor.RESET}")
            fallback
          case ModuleConflict(section) =>
            println(s"${AnsiColor.RED}Error: Too many modules specified for ${section}${AnsiColor.RESET}")
            fallback
        },
        identity
      )

    println(result.logEntries.show)

    if (paths.isEmpty) {
      CommandLineResult.failure
    } else {
      CommandLineResult.success
    }
  }

  def runLanguages(tasks: Map[String, NonEmptyList[Args]]): Target[List[ReadSwagger[Target[List[WriteTree]]]]]

  def guardrailRunner: Map[String, NonEmptyList[Args]] => Target[List[java.nio.file.Path]] = { tasks =>
    runLanguages(tasks)
      .flatMap(
        _.flatTraverse(
          rs =>
            ReadSwagger
              .readSwagger(rs)
              .flatMap(_.traverse(WriteTree.writeTree))
              .leftFlatMap(
                value =>
                  Target.pushLogger(StructuredLogger.error(s"${AnsiColor.RED}Error in ${rs.path}${AnsiColor.RESET}")) *> Target.raiseError[List[Path]](value)
              )
              .productL(Target.pushLogger(StructuredLogger.reset))
        )
      )
      .map(_.distinct)
  }
}
