package dev.guardrail.generators

import cats.data.NonEmptyList
import cats.syntax.all._
import com.github.javaparser.StaticJavaParser
import _root_.scala.meta._
import _root_.scala.util.{ Failure, Success, Try }

import dev.guardrail.core.CoreTermInterp
import dev.guardrail.generators.java.JavaLanguage
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.{ Args, Common, MissingDependency, ReadSwagger, Target, UnparseableArgument, WriteTree }

object GeneratorMappings {
  private abstract class LoaderIndirection[A] {
    val instance: A
  }

  private def catchClassNotFound[A](value: => LoaderIndirection[A], error: => MissingDependency): Target[A] =
    try
      Target.pure(value.instance)
    catch {
      case _: _root_.java.lang.NoClassDefFoundError =>
        Target.raiseError(error)
    }

  private def indirectScalaModule = new LoaderIndirection[AbstractModule[ScalaLanguage]] {
    val instance = scala.ScalaModule
  }

  val scalaModule = catchClassNotFound(indirectScalaModule, MissingDependency("guardrail-scala-support"))

  implicit def scalaInterpreter = new CoreTermInterp[ScalaLanguage](
    "akka-http",
    xs => scalaModule.flatMap(_.extract(xs)),
    {
      case "akka-http"         => NonEmptyList.of("akka-http", "circe")
      case "http4s"            => NonEmptyList.of("circe", "http4s")
      case "http4s-v0.23"      => NonEmptyList.of("circe", "http4s-v0.23")
      case "http4s-v0.22"      => NonEmptyList.of("circe", "http4s-v0.22")
      case "akka-http-jackson" => NonEmptyList.of("akka-http", "jackson")
      case "dropwizard"        => NonEmptyList.of("dropwizard", "jackson")
    },
    _.parse[Importer].toEither.bimap(err => UnparseableArgument("import", err.toString), importer => Import(List(importer)))
  )

  private def indirectJavaModule = new LoaderIndirection[AbstractModule[JavaLanguage]] {
    val instance = java.JavaModule
  }

  val javaModule = catchClassNotFound(indirectJavaModule, MissingDependency("guardrail-java-support"))

  implicit def javaInterpreter = new CoreTermInterp[JavaLanguage](
    "dropwizard",
    xs => javaModule.flatMap(_.extract(xs)),
    {
      case "dropwizard" => NonEmptyList.of("dropwizard", "jackson", "java-stdlib", "async-http-client")
      case "spring-mvc" => NonEmptyList.of("spring-mvc", "jackson", "java-stdlib", "async-http-client")
    },
    { str =>
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
