package dev.guardrail.cli

import java.nio.file.Path
import cats.data.NonEmptyList
import cats.syntax.all._
import scala.io.AnsiColor
import cats.FlatMap

import dev.guardrail._
import dev.guardrail.core.{ LogLevel, LogLevels }
import dev.guardrail.terms.protocol.PropertyRequirement
import dev.guardrail.runner.GuardrailRunner

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
    |   --package-from-tags                                    : Use the tags, defined in the OpenAPI specification, to guide the generated package structures
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

trait CLICommon extends GuardrailRunner {
  def processArgs(args: Array[String]): CommandLineResult = {
    val (languages, strippedArgs) = args.span(!_.startsWith("-"))
    val language                  = languages.headOption.getOrElse("scala")
    run(language, strippedArgs)
  }

  def parseOptionalProperty(arg: String, value: String): Target[PropertyRequirement.OptionalRequirement] =
    value match {
      case "required-nullable" => Target.pure(PropertyRequirement.RequiredNullable)
      case "optional"          => Target.pure(PropertyRequirement.Optional)
      case "legacy"            => Target.pure(PropertyRequirement.OptionalLegacy)
      case _                   => Target.raiseError(UnparseableArgument(s"${arg} ${value}", "Expected one of required-nullable, optional or legacy"))
    }

  def parseAuthImplementation(arg: String, value: String): Target[AuthImplementation] =
    value match {
      case "disable" => Target.pure(AuthImplementation.Disable)
      case "native"  => Target.pure(AuthImplementation.Native)
      case "simple"  => Target.pure(AuthImplementation.Simple)
      case "custom"  => Target.pure(AuthImplementation.Custom)
      case _         => Target.raiseError(UnparseableArgument(s"${arg} ${value}", "Expected one of 'disable', 'native', 'simple' or 'custom'"))
    }

  def parseArgs(args: Array[String]): Target[List[Args]] = {
    def expandTilde(path: String): String =
      path.replaceFirst("^~", System.getProperty("user.home"))
    val defaultArgs =
      Args.empty.copy(context = Args.empty.context, defaults = true)

    type From = (List[Args], List[String])
    type To   = List[Args]
    val start: From = (List.empty[Args], args.toList)
    import Target.log.debug
    Target.log.function("parseArgs") {
      FlatMap[Target].tailRecM[From, To](start) { case pair @ (sofars, rest) =>
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
            case (sofar :: already, "--package-from-tags" :: xs) =>
              Continue((sofar.copyContext(tagsBehaviour = Context.PackageFromTags) :: already, xs))
            case (sofar :: already, (arg @ "--optional-encode-as") :: value :: xs) =>
              for {
                propertyRequirement <- parseOptionalProperty(arg.drop(2), value)
                res                 <- Continue((sofar.copyPropertyRequirement(encoder = propertyRequirement) :: already, xs))
              } yield res
            case (sofar :: already, (arg @ "--optional-decode-as") :: value :: xs) =>
              for {
                propertyRequirement <- parseOptionalProperty(arg.drop(2), value)
                res                 <- Continue((sofar.copyPropertyRequirement(decoder = propertyRequirement) :: already, xs))
              } yield res
            case (sofar :: already, (arg @ "--auth-implementation") :: value :: xs) =>
              for {
                auth <- parseAuthImplementation(arg, value)
                res  <- Continue((sofar.copyContext(authImplementation = auth) :: already, xs))
              } yield res
            case (_, unknown) =>
              debug("Unknown argument") >> Bail(UnknownArguments(unknown))
          }
        } yield step
      }
    }
  }

  def run(language: String, args: Array[String]): CommandLineResult = {
    // Hacky loglevel parsing, only supports levels that come before absolutely
    // every other argument due to arguments being a small configuration
    // language themselves.
    val (levels, newArgs): (Array[String], Array[String]) =
      args.span(arg => LogLevels(arg.stripPrefix("--")).isDefined)
    val level: Option[String] = levels.lastOption.map(_.stripPrefix("--"))

    level.foreach(_ => Target.loggerEnabled.set(true))

    val coreArgs = parseArgs(newArgs).map(NonEmptyList.fromList(_))

    implicit val logLevel: LogLevel = level
      .flatMap(level => LogLevels.members.find(_.level == level.toLowerCase))
      .getOrElse(LogLevels.Warning)

    val result = coreArgs
      .flatMap { args =>
        guardrailRunner(args.map(language -> _).toMap)
      }

    val fallback = List.empty[Path]
    import CLICommon.unsafePrintHelp
    val paths = result
      .fold(
        {
          case MissingArg(args, Error.ArgName(arg)) =>
            println(s"${AnsiColor.RED}Missing argument:${AnsiColor.RESET} ${AnsiColor.BOLD}${arg}${AnsiColor.RESET} (In block ${args})")
            unsafePrintHelp()
            fallback
          case MissingDependency(dependency) =>
            println(s"${AnsiColor.RED}Missing dependency:${AnsiColor.RESET} ${AnsiColor.BOLD}${dependency}${AnsiColor.RESET} not found on classpath")
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
          case UnspecifiedModules(choices) =>
            val result =
              choices.foldLeft(Seq.empty[String]) { case (acc, (module, choices)) =>
                val nextLabel = Option(choices).filter(_.nonEmpty).fold("<no choices found>")(_.toSeq.sorted.mkString(", "))
                acc :+ s"${AnsiColor.BOLD}${AnsiColor.WHITE}${module}:${AnsiColor.RESET} [${AnsiColor.BLUE}${nextLabel}${AnsiColor.RESET}]"
              }
            println(s"${AnsiColor.RED}Unsatisfied module(s):${AnsiColor.RESET} ${result.mkString(", ")}")
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
}
