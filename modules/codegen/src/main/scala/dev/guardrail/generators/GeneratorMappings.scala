package dev.guardrail.generators

import cats.data.NonEmptyList
import cats.syntax.all._
import com.github.javaparser.StaticJavaParser
import _root_.scala.meta._
import _root_.scala.util.{ Failure, Success, Try }

import dev.guardrail.core.CoreTermInterp
import dev.guardrail.generators.java.JavaLanguage
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.{ Args, Common, ReadSwagger, Target, UnparseableArgument, WriteTree }

object GeneratorMappings {
  implicit def scalaInterpreter = new CoreTermInterp[ScalaLanguage](
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

  implicit def javaInterpreter = new CoreTermInterp[JavaLanguage](
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

  def defaultLanguages: Map[String, NonEmptyList[Args] => Target[NonEmptyList[ReadSwagger[Target[List[WriteTree]]]]]] = Map(
    ("java", Common.runM[JavaLanguage, Target](_)),
    ("scala", Common.runM[ScalaLanguage, Target](_))
  )
}
