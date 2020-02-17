package com.twilio.guardrail

import java.nio.file.Path
import cats.data.NonEmptyList
import cats.implicits._
import cats.~>
import com.twilio.guardrail.core.CoreTermInterp
import com.twilio.guardrail.terms.{ CoreTerm, CoreTerms }
import com.twilio.swagger.core.{ LogLevel, LogLevels, StructuredLogger }
import com.twilio.guardrail.languages.{ JavaLanguage, LA, ScalaLanguage }
import scala.io.AnsiColor
import scala.util.{ Failure, Success }

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
  def scalaInterpreter: CoreTerm[ScalaLanguage, ?] ~> CoreTarget
  def javaInterpreter: CoreTerm[JavaLanguage, ?] ~> CoreTarget

  def processArgs(args: Array[String]): CommandLineResult = {
    val (language, strippedArgs) = args.partition(handleLanguage.isDefinedAt _)
    handleLanguage(language.lastOption.getOrElse("scala"))(strippedArgs)
  }

  def chainFrameworkMappings[L <: LA](
      first: PartialFunction[String, CodegenApplication[L, ?] ~> Target],
      second: PartialFunction[String, CodegenApplication[L, ?] ~> Target]
  ): PartialFunction[String, CodegenApplication[L, ?] ~> Target] = first.orElse(second)

  def handleLanguage: PartialFunction[String, Array[String] => CommandLineResult] = {
    case "java"  => run("java", _)(javaInterpreter)
    case "scala" => run("scala", _)(scalaInterpreter)
  }

  def run[L <: LA](language: String, args: Array[String])(interpreter: CoreTerm[L, ?] ~> CoreTarget): CommandLineResult = {
    val C = CoreTerms.coreTerm[L, CoreTerm[L, ?]]
    // Hacky loglevel parsing, only supports levels that come before absolutely
    // every other argument due to arguments being a small configuration
    // language themselves.
    val (levels, newArgs): (Array[String], Array[String]) =
      args.span(arg => LogLevels(arg.stripPrefix("--")).isDefined)
    val level: Option[String] = levels.lastOption.map(_.stripPrefix("--"))

    // FIXME: The only reason we need the interpreter at all is to call parseArgs on it
    // This likely means the CLI should _not_ be part of CoreTerms. There's no reason
    // for it to be in there, as CLI is effectively a bespoke build tool whose unused
    // code is included into all other build tools.
    val coreArgs = C.parseArgs(newArgs).map(NonEmptyList.fromList(_))

    val result = coreArgs
      .foldMap(interpreter)
      .flatMap({ args =>
        guardrailRunner(args.map(language -> _).toMap)
      })

    implicit val logLevel: LogLevel = level
      .flatMap(level => LogLevels.members.find(_.level == level.toLowerCase))
      .getOrElse(LogLevels.Warning)

    val fallback = List.empty[Path]
    import CLICommon.unsafePrintHelp
    val /*(logger,*/ paths /*)*/ = result
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
          case MissingModule(section) =>
            println(s"${AnsiColor.RED}Error: Missing module ${section}${AnsiColor.RESET}")
            fallback
          case ModuleConflict(section) =>
            println(s"${AnsiColor.RED}Error: Too many modules specified for ${section}${AnsiColor.RESET}")
            fallback
        },
        identity
      )

    // println(logger.show)

    if (paths.isEmpty) {
      CommandLineResult.failure
    } else {
      CommandLineResult.success
    }
  }

  def runLanguages(
      tasks: Map[String, NonEmptyList[Args]]
  ): CoreTarget[List[ReadSwagger[Target[List[WriteTree]]]]] =
    tasks.toList.flatTraverse[CoreTarget, ReadSwagger[Target[List[WriteTree]]]]({
      case (language, args) =>
        (language match {
          case "java" =>
            Common
              .runM[JavaLanguage, CoreTerm[JavaLanguage, ?]](args)
              .foldMap(javaInterpreter)
          case "scala" =>
            Common
              .runM[ScalaLanguage, CoreTerm[ScalaLanguage, ?]](args)
              .foldMap(scalaInterpreter)
          case other =>
            CoreTarget.raiseError(UnparseableArgument("language", other))
        }).map(_.toList)
    })

  def guardrailRunner: Map[String, NonEmptyList[Args]] => CoreTarget[List[java.nio.file.Path]] = { tasks =>
    runLanguages(tasks)
      .flatMap(
        _.flatTraverse(
          rs =>
            ReadSwagger
              .readSwagger(rs)
              .map(_.map(WriteTree.unsafeWriteTree))
              .leftFlatMap(
                value =>
                  Target
                    .pushLogger(StructuredLogger.error(s"${AnsiColor.RED}Error in ${rs.path}${AnsiColor.RESET}"))
                    .toEitherT
                    .subflatMap(_ => Either.left[Error, List[Path]](value))
              )
              <* Target.pushLogger(StructuredLogger.reset).toEitherT
        )
      )
      .map(_.distinct)
  }
}

object CLI extends CLICommon {
  import com.twilio.guardrail.generators.{ AkkaHttp, Endpoints, Http4s }
  import com.twilio.guardrail.generators.{ Java, JavaModule, ScalaModule }
  import scala.meta._
  val scalaInterpreter = CoreTermInterp[ScalaLanguage](
    "akka-http",
    ScalaModule.extract, {
      case "akka-http" => AkkaHttp
      case "endpoints" => Endpoints
      case "http4s"    => Http4s
    }, {
      _.parse[Importer].toEither.bimap(err => UnparseableArgument("import", err.toString), importer => Import(List(importer)))
    }
  )

  val javaInterpreter = CoreTermInterp[JavaLanguage](
    "dropwizard",
    JavaModule.extract, {
      case "dropwizard" => Java.Dropwizard
      case "spring-mvc" => Java.SpringMvc
    }, { str =>
      import com.github.javaparser.StaticJavaParser
      import scala.util.Try
      Try(StaticJavaParser.parseImport(s"import ${str};")) match {
        case Success(value) => Right(value)
        case Failure(t)     => Left(UnparseableArgument("import", t.getMessage))
      }
    }
  )

  def main(args: Array[String]): Unit = {
    val result = processArgs(args)
    sys.exit(result.exitStatus)
  }
}

case class CommandLineResult(exitStatus: Int)

object CommandLineResult {
  val failure: CommandLineResult = CommandLineResult(1)
  val success: CommandLineResult = CommandLineResult(0)
}
