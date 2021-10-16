package dev.guardrail.generators

import cats.data.NonEmptyList
import cats.syntax.all._
import com.github.javaparser.StaticJavaParser
import _root_.scala.meta._
import _root_.scala.util.{ Failure, Success, Try }

import dev.guardrail.core.CoreTermInterp
import dev.guardrail.generators.java.JavaLanguage
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.{ Args, Common, MissingDependency, ReadSwagger, Target, UnparseableArgument, WriteTree, monadForCore }

object GeneratorMappings {
  private abstract class LoaderIndirection[A] {
    val instance: A
  }

  private def catchClassNotFound[A](value: => LoaderIndirection[A], error: => MissingDependency): Target[A] =
    try {
      Target.pure(value.instance)
    } catch {
      case _: _root_.java.lang.NoClassDefFoundError =>
        Target.raiseError(error)
    }

  private def indirectScalaModule = new LoaderIndirection[AbstractModule[ScalaLanguage]] {
    val instance = scala.ScalaModule
  }

  val scalaModule = catchClassNotFound(indirectScalaModule, MissingDependency("guardrail-scala-support"))

  private def indirectAkkaHttp = new LoaderIndirection[Framework[ScalaLanguage, Target]] {
    val instance = scala.akkaHttp.AkkaHttp
  }
  private def indirectEndpoints = new LoaderIndirection[Framework[ScalaLanguage, Target]] {
    val instance = scala.endpoints.Endpoints
  }
  private def indirectHttp4s = new LoaderIndirection[Framework[ScalaLanguage, Target]] {
    val instance = scala.http4s.Http4s
  }
  private def indirectAkkaHttpJackson = new LoaderIndirection[Framework[ScalaLanguage, Target]] {
    val instance = scala.akkaHttp.AkkaHttpJackson
  }
  private def indirectScalaDropwizard = new LoaderIndirection[Framework[ScalaLanguage, Target]] {
    val instance = scala.dropwizard.Dropwizard
  }

  implicit def scalaInterpreter = new CoreTermInterp[ScalaLanguage](
    "akka-http",
    xs => scalaModule.flatMap(_.extract(xs)), {
      case "akka-http"         => scalaModule *> catchClassNotFound(indirectAkkaHttp, MissingDependency("guardrail-scala-akka-http"))
      case "endpoints"         => scalaModule *> catchClassNotFound(indirectEndpoints, MissingDependency("guardrail-scala-endpoints"))
      case "http4s"            => scalaModule *> catchClassNotFound(indirectHttp4s, MissingDependency("guardrail-scala-http4s"))
      case "akka-http-jackson" => scalaModule *> catchClassNotFound(indirectAkkaHttpJackson, MissingDependency("guardrail-scala-akka-http"))
      case "dropwizard"        => scalaModule *> catchClassNotFound(indirectScalaDropwizard, MissingDependency("guardrail-scala-dropwizard"))
    }, {
      _.parse[Importer].toEither.bimap(err => UnparseableArgument("import", err.toString), importer => Import(List(importer)))
    }
  )

  private def indirectJavaModule = new LoaderIndirection[AbstractModule[JavaLanguage]] {
    val instance = java.JavaModule
  }

  val javaModule = catchClassNotFound(indirectJavaModule, MissingDependency("guardrail-java-support"))

  private def indirectJavaDropwizard = new LoaderIndirection[Framework[JavaLanguage, Target]] {
    val instance = java.dropwizard.Dropwizard
  }
  private def indirectSpringMvc = new LoaderIndirection[Framework[JavaLanguage, Target]] {
    val instance = java.springMvc.SpringMvc
  }

  implicit def javaInterpreter = new CoreTermInterp[JavaLanguage](
    "dropwizard",
    xs => javaModule.flatMap(_.extract(xs)), {
      case "dropwizard" => javaModule *> catchClassNotFound(indirectJavaDropwizard, MissingDependency("guardrail-java-dropwizard"))
      case "spring-mvc" => javaModule *> catchClassNotFound(indirectSpringMvc, MissingDependency("guardrail-java-spring-mvc"))
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
