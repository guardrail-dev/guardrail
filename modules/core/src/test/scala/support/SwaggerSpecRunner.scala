package support

import _root_.io.swagger.parser.OpenAPIParser
import _root_.io.swagger.v3.oas.models._
import _root_.io.swagger.v3.parser.core.models.ParseOptions
import cats.data.NonEmptyList
import java.util.LinkedList
import org.scalatest.{ Assertions, EitherValues, OptionValues }
import org.scalactic.source.Position

import dev.guardrail._
import dev.guardrail.core.{ StructuredLogger, Tracker }
import dev.guardrail.generators.Framework
import dev.guardrail.generators._
import dev.guardrail.languages.LA

trait TargetValues { self: Assertions =>
  implicit class TargetSyntax[A](wrapped: Target[A])(implicit pos: Position) {
    def value: A = wrapped.valueOr { err =>
      fail(s"Attempted to get value for a failed Target: ${err}")
    }
  }
}

trait SwaggerSpecRunner extends EitherValues with OptionValues with TargetValues { self: Assertions =>
  def swaggerFromString(spec: String): OpenAPI = {
    val parseOpts = new ParseOptions
    parseOpts.setResolve(true)
    new OpenAPIParser().readContents(spec, new LinkedList(), parseOpts).getOpenAPI
  }

  def runSwaggerSpec[L <: LA](
      spec: String,
      dtoPackage: List[String] = List.empty,
      supportPackage: NonEmptyList[String] = NonEmptyList.one("support")
  )(
      context: Context,
      framework: Framework[L, Target],
      targets: NonEmptyList[CodegenTarget] = NonEmptyList.of(CodegenTarget.Client, CodegenTarget.Server)
  ): (ProtocolDefinitions[L], Clients[L], Servers[L]) =
    runSwagger(swaggerFromString(spec), dtoPackage, supportPackage)(
      context,
      framework,
      targets
    )

  private def runSwagger[L <: LA](
      swagger: OpenAPI,
      dtoPackage: List[String],
      supportPackage: NonEmptyList[String]
  )(context: Context, framework: Framework[L, Target], targets: NonEmptyList[CodegenTarget]): (ProtocolDefinitions[L], Clients[L], Servers[L]) = {
    import framework._
    targets
      .map(
        target =>
          Common
            .prepareDefinitions[L, Target](
              target,
              context,
              Tracker(swagger),
              dtoPackage,
              supportPackage
            )
            .valueOr({ err =>
              throw new Exception(err.toString)
            })
      )
      .foldLeft[(ProtocolDefinitions[L], Clients[L], Servers[L])]((ProtocolDefinitions(Nil, Nil, Nil, Nil, None), Clients(Nil, Nil), Servers(Nil, Nil)))(
        {
          case ((proto, clients, servers), (generatedProto, generatedDefs)) =>
            val newProto =
              if ((proto.elems ++ proto.packageObjectContents ++ proto.packageObjectImports ++ proto.protocolImports ++ proto.implicitsObject.toList).nonEmpty) {
                proto
              } else {
                generatedProto
              }
            (
              newProto,
              Clients(clients.clients ++ generatedDefs.clients, (clients.supportDefinitions ++ generatedDefs.supportDefinitions).distinct),
              Servers(servers.servers ++ generatedDefs.servers, (servers.supportDefinitions ++ generatedDefs.supportDefinitions).distinct)
            )
        }
      )
  }

  def runInvalidSwaggerSpec[L <: LA](
      spec: String
  ): (Context, CodegenTarget, Framework[L, Target]) => (StructuredLogger, Error) = {
    val parseOpts = new ParseOptions
    parseOpts.setResolve(true)
    runInvalidSwagger[L](new OpenAPIParser().readContents(spec, new LinkedList(), parseOpts).getOpenAPI) _
  }

  def runInvalidSwagger[L <: LA](swagger: OpenAPI)(context: Context, kind: CodegenTarget, framework: Framework[L, Target]): (StructuredLogger, Error) = {
    import framework._
    Common
      .prepareDefinitions[L, Target](
        kind,
        context,
        Tracker(swagger),
        List.empty,
        NonEmptyList.one("support")
      ) match {
      case TargetError(err, la) => (la, err)
      case _                    => ???
    }
  }
}
