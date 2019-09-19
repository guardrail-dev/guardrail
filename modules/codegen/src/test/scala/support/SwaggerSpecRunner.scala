package support

import _root_.io.swagger.v3.oas.models._
import com.twilio.guardrail.languages.LA
import _root_.io.swagger.parser.OpenAPIParser
import _root_.io.swagger.v3.parser.core.models.ParseOptions
import cats.arrow.FunctionK
import com.twilio.guardrail._
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.terms.{ ScalaTerms, SwaggerTerms }
import scala.meta.Tree
import org.scalactic.Equality

trait SwaggerSpecRunner {
  implicit def TreeEquality[A <: Tree]: Equality[A] =
    new Equality[A] {
      def areEqual(a: A, b: Any): Boolean =
        b match {
          case x: Tree => a.structure == x.structure
          case _       => false
        }
    }

  def runSwaggerSpec[L <: LA](
      spec: String
  ): (Context, FunctionK[CodegenApplication[L, ?], Target]) => (ProtocolDefinitions[L], Clients[L], Servers[L]) = {
    val parseOpts = new ParseOptions
    parseOpts.setResolve(true)
    runSwagger(new OpenAPIParser().readContents(spec, new java.util.LinkedList(), parseOpts).getOpenAPI) _
  }

  def runSwagger[L <: LA](swagger: OpenAPI)(context: Context, framework: FunctionK[CodegenApplication[L, ?], Target])(
      implicit Fw: FrameworkTerms[L, CodegenApplication[L, ?]],
      Sc: ScalaTerms[L, CodegenApplication[L, ?]],
      Sw: SwaggerTerms[L, CodegenApplication[L, ?]]
  ): (ProtocolDefinitions[L], Clients[L], Servers[L]) = {
    val (proto, CodegenDefinitions(clients, Nil, clientSupportDefs, _)) = Target.unsafeExtract(
      Common
        .prepareDefinitions[L, CodegenApplication[L, ?]](
          CodegenTarget.Client,
          context,
          swagger
        )
        .foldMap(framework)
    )

    val (_, CodegenDefinitions(Nil, servers, serverSupportDefs, _)) = Target.unsafeExtract(
      Common
        .prepareDefinitions[L, CodegenApplication[L, ?]](
          CodegenTarget.Server,
          context,
          swagger
        )
        .foldMap(framework)
    )

    (proto, Clients(clients, clientSupportDefs), Servers(servers, serverSupportDefs))
  }

}
