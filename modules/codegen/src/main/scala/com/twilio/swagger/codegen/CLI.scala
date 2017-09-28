package com.twilio.swagger.codegen

import cats.instances.all._
import cats.syntax.either._
import cats.syntax.traverse._
import cats.~>
import com.twilio.swagger.codegen.core.CoreTermInterp
import com.twilio.swagger.codegen.terms.CoreTerm
import scala.io.AnsiColor

object CLICommon {
  def run(args: Array[String])(interpreter: CoreTerm ~> CoreTarget): Unit = {
    Common.runM[CoreTerm](args).foldMap(interpreter)
      .fold({
        case MissingArg(args, Error.ArgName(arg)) =>
          println(s"${AnsiColor.RED}Missing argument:${AnsiColor.RESET} ${AnsiColor.BOLD}${arg}${AnsiColor.RESET} (In block ${args})")
          unsafePrintHelp()
        case NoArgsSpecified =>
          println(s"${AnsiColor.RED}No arguments specified${AnsiColor.RESET}")
          unsafePrintHelp()
        case NoFramework =>
          println(s"${AnsiColor.RED}No framework specified${AnsiColor.RESET}")
          unsafePrintHelp()
        case PrintHelp =>
          unsafePrintHelp()
        case UnknownArguments(args) =>
          println(s"${AnsiColor.RED}Unknown arguments: ${args.mkString(" ")}${AnsiColor.RESET}")
          unsafePrintHelp()
        case UnknownFramework(name) =>
          println(s"${AnsiColor.RED}Unknown framework specified: ${name}${AnsiColor.RESET}")
        case UnparseableArgument(name, message) =>
          println(s"${AnsiColor.RED}Unparseable argument: --${name}, ${message}")
      }, _.toList.foreach(rs =>
        ReadSwagger.unsafeReadSwagger(rs)
          .fold({ err =>
            println(s"${AnsiColor.RED}Error: ${err}${AnsiColor.RESET}")
            unsafePrintHelp()
          }, _.foreach(WriteTree.unsafeWriteTree))))
  }

  def unsafePrintHelp(): Unit = {
    val text = s"""
    | ${AnsiColor.CYAN}swagger-codegen${AnsiColor.RESET}
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
    |    swagger-codegen --specPath client-specs/account-events-api.json --outputPath src/main/scala --packageName com.twilio.messaging.console.clients --tracing
    |
    |  Generate two clients, put both in src/main/scala, under different packages, one with tracing, one without:
    |    swagger-codegen \\
    |      --client --specPath client-specs/account-events-api.json --outputPath src/main/scala --packageName com.twilio.messaging.console.clients.events \\
    |      --client --specPath client-specs/account-service.json --outputPath src/main/scala --packageName com.twilio.messaging.console.clients.account --tracing
    |
    |  Generate client and server routes for the same specification:
    |    swagger-codegen \\
    |      --client --specPath client-specs/account-events-api.json --outputPath src/main/scala --packageName com.twilio.messaging.console.clients.events \\
    |      --server --specPath client-specs/account-events-api.json --outputPath src/main/scala --packageName com.twilio.messaging.console.clients.events
    |""".stripMargin

    System.err.println(text)
  }
}

trait CLICommon {
  val interpreter: CoreTerm ~> CoreTarget

  def main(args: Array[String]): Unit = {
    CLICommon.run(args)(interpreter)
  }
}

object CLI extends CLICommon {
  val interpreter = CoreTermInterp
}
