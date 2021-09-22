package dev.guardrail.cli

import cats.data.NonEmptyList
import cats.syntax.all._
import com.github.javaparser.StaticJavaParser
import scala.meta._
import scala.util.{ Failure, Success, Try }

import dev.guardrail._
import dev.guardrail.core.CoreTermInterp
import dev.guardrail.generators.java
import dev.guardrail.generators.java.JavaLanguage
import dev.guardrail.generators.scala
import dev.guardrail.generators.scala.ScalaLanguage

object CLI extends CLICommon {
  implicit val scalaInterpreter = new CoreTermInterp[ScalaLanguage](
    "akka-http",
    scala.ScalaModule.extract, {
      case "akka-http"         => scala.akkaHttp.AkkaHttp
      case "endpoints"         => scala.endpoints.Endpoints
      case "http4s"            => scala.http4s.Http4s
      case "akka-http-jackson" => scala.akkaHttp.AkkaHttpJackson
      case "dropwizard"        => scala.dropwizard.Dropwizard
    }, {
      _.parse[Importer].toEither.bimap(err => UnparseableArgument("import", err.toString), importer => Import(List(importer)))
    }
  )

  implicit val javaInterpreter = new CoreTermInterp[JavaLanguage](
    "dropwizard",
    java.JavaModule.extract, {
      case "dropwizard" => java.dropwizard.Dropwizard
      case "spring-mvc" => java.springMvc.SpringMvc
    }, { str =>
      Try(StaticJavaParser.parseImport(s"import ${str};")) match {
        case Success(value) => Right(value)
        case Failure(t)     => Left(UnparseableArgument("import", t.getMessage))
      }
    }
  )

  def handleLanguage: PartialFunction[String, Array[String] => CommandLineResult] = {
    case "java"  => run("java", _)
    case "scala" => run("scala", _)
  }

  def runLanguages(
      tasks: Map[String, NonEmptyList[Args]]
  ): Target[List[ReadSwagger[Target[List[WriteTree]]]]] =
    tasks.toList.flatTraverse[Target, ReadSwagger[Target[List[WriteTree]]]]({
      case (language, args) =>
        (language match {
          case "java" =>
            Common.runM[JavaLanguage, Target](args)
          case "scala" =>
            Common.runM[ScalaLanguage, Target](args)
          case other =>
            Target.raiseError(UnparseableArgument("language", other))
        }).map(_.toList)
    })

  def main(args: Array[String]): Unit = {
    val result = processArgs(args)
    sys.exit(result.exitStatus)
  }
}
