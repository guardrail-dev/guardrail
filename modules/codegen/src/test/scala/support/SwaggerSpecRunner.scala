package support

import _root_.io.swagger.v3.oas.models._
import com.twilio.guardrail.languages.LA
import _root_.io.swagger.parser.OpenAPIParser
import _root_.io.swagger.v3.parser.core.models.ParseOptions
import cats.implicits._
import com.twilio.guardrail._
import com.twilio.guardrail.core.{ LogLevels, StructuredLogger, Tracker }
import com.twilio.guardrail.generators.Framework
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.terms.{ LanguageTerms, SwaggerTerms }
import scala.meta.Tree
import org.scalactic.Equality
import org.scalatest.{ EitherValues, OptionValues }

trait SwaggerSpecRunner extends EitherValues with OptionValues {
  implicit def TreeEquality[A <: Tree]: Equality[A] =
    new Equality[A] {
      def areEqual(a: A, b: Any): Boolean =
        b match {
          case x: Tree => a.structure == x.structure
          case _       => false
        }
    }

  def runSwaggerSpec[L <: LA](
      spec: String,
      dtoPackage: List[String] = List.empty
  ): (Context, Framework[L, Target]) => (ProtocolDefinitions[L], Clients[L], Servers[L]) = {
    val parseOpts = new ParseOptions
    parseOpts.setResolve(true)
    runSwagger(new OpenAPIParser().readContents(spec, new java.util.LinkedList(), parseOpts).getOpenAPI, dtoPackage) _
  }

  def runSwagger[L <: LA](
      swagger: OpenAPI,
      dtoPackage: List[String] = List.empty,
      supportPackage: List[String] = List.empty
  )(context: Context, framework: Framework[L, Target]): (ProtocolDefinitions[L], Clients[L], Servers[L]) = {
    import framework._
    val (proto, CodegenDefinitions(clients, Nil, clientSupportDefs, _)) =
      Common
        .prepareDefinitions[L, Target](
          CodegenTarget.Client,
          context,
          Tracker(swagger),
          dtoPackage,
          supportPackage
        )
        .valueOr({ err =>
          throw new Exception(err.toString)
        })

    val (_, CodegenDefinitions(Nil, servers, serverSupportDefs, _)) =
      Common
        .prepareDefinitions[L, Target](
          CodegenTarget.Server,
          context,
          Tracker(swagger),
          List.empty,
          List.empty
        )
        .valueOr({ err =>
          throw new Exception(err.toString)
        })

    // FIXME: In lieu of https://github.com/scalatest/scalatest/issues/405,
    // figure out a way to use https://stackoverflow.com/a/7219813 to only println
    // if a bracketed test fails.
    implicit val logLevel = LogLevels.Debug
//  println("Client Generator logs:")
//  println(clientLogger.show)
//  println("Server Generator logs:")
//  println(serverLogger.show)

    (proto, Clients(clients, clientSupportDefs), Servers(servers, serverSupportDefs))
  }

  def runInvalidSwaggerSpec[L <: LA](
      spec: String
  ): (Context, CodegenTarget, Framework[L, Target]) => (StructuredLogger, Error) = {
    val parseOpts = new ParseOptions
    parseOpts.setResolve(true)
    runInvalidSwagger[L](new OpenAPIParser().readContents(spec, new java.util.LinkedList(), parseOpts).getOpenAPI) _
  }

  def runInvalidSwagger[L <: LA](swagger: OpenAPI)(context: Context, kind: CodegenTarget, framework: Framework[L, Target]): (StructuredLogger, Error) = {
    import framework._
    Common
      .prepareDefinitions[L, Target](
        kind,
        context,
        Tracker(swagger),
        List.empty,
        List.empty
      ) match {
      case TargetError(err, la) => (la, err)
      case _                    => ???
    }
  }
}
