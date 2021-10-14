package dev.guardrail.generators

import cats.data.NonEmptyList
import cats.syntax.all._
import com.github.javaparser.StaticJavaParser
import _root_.scala.meta._
import _root_.scala.util.{ Failure, Success, Try }

import dev.guardrail.languages.LA
import dev.guardrail.core.CoreTermInterp
import dev.guardrail.generators.java.JavaLanguage
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.{ Args, Common, MissingDependency, ReadSwagger, Target, UnparseableArgument, WriteTree }

object GeneratorMappings {
  private abstract class LoaderIndirection[L <: LA] {
    val instance: Framework[L, Target]
  }

  private def catchClassNotFound[L <: LA](value: => LoaderIndirection[L], error: => MissingDependency): Target[Framework[L, Target]] =
    try {
      Target.pure(value.instance)
    } catch {
      case _: _root_.java.lang.NoClassDefFoundError =>
        Target.raiseError(error)
    }

  private def indirectAkkaHttp = new LoaderIndirection[ScalaLanguage] {
    val instance = scala.akkaHttp.AkkaHttp
  }
  private def indirectEndpoints = new LoaderIndirection[ScalaLanguage] {
    val instance = scala.endpoints.Endpoints
  }
  private def indirectHttp4s = new LoaderIndirection[ScalaLanguage] {
    val instance = scala.http4s.Http4s
  }
  private def indirectAkkaHttpJackson = new LoaderIndirection[ScalaLanguage] {
    val instance = scala.akkaHttp.AkkaHttpJackson
  }
  private def indirectScalaDropwizard = new LoaderIndirection[ScalaLanguage] {
    val instance = scala.dropwizard.Dropwizard
  }

  implicit def scalaInterpreter = new CoreTermInterp[ScalaLanguage](
    "akka-http",
    scala.ScalaModule.extract, {
      case "akka-http"         => catchClassNotFound(indirectAkkaHttp, MissingDependency("guardrail-scala-akka-http"))
      case "endpoints"         => catchClassNotFound(indirectEndpoints, MissingDependency("guardrail-scala-endpoints"))
      case "http4s"            => catchClassNotFound(indirectHttp4s, MissingDependency("guardrail-scala-http4s"))
      case "akka-http-jackson" => catchClassNotFound(indirectAkkaHttpJackson, MissingDependency("guardrail-scala-akka-http"))
      case "dropwizard"        => catchClassNotFound(indirectScalaDropwizard, MissingDependency("guardrail-scala-dropwizard"))
    }, {
      _.parse[Importer].toEither.bimap(err => UnparseableArgument("import", err.toString), importer => Import(List(importer)))
    }
  )

  private def indirectJavaDropwizard = new LoaderIndirection[JavaLanguage] {
    val instance = java.dropwizard.Dropwizard
  }
  private def indirectSpringMvc = new LoaderIndirection[JavaLanguage] {
    val instance = java.springMvc.SpringMvc
  }

  implicit def javaInterpreter = new CoreTermInterp[JavaLanguage](
    "dropwizard",
    java.JavaModule.extract, {
      case "dropwizard" => catchClassNotFound(indirectJavaDropwizard, MissingDependency("guardrail-java-dropwizard"))
      case "spring-mvc" => catchClassNotFound(indirectSpringMvc, MissingDependency("guardrail-java-spring-mvc"))
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
