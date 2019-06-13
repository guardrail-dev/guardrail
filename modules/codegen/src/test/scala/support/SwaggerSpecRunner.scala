package support

import com.twilio.guardrail.languages.LA
import io.swagger.parser.OpenAPIParser
import io.swagger.v3.parser.core.models.ParseOptions
import java.util

trait SwaggerSpecRunner {

  import _root_.io.swagger.v3.oas.models._
  import cats.arrow.FunctionK
  import com.twilio.guardrail._
  import com.twilio.guardrail.terms.framework.FrameworkTerms
  import com.twilio.guardrail.terms.{ ScalaTerms, SwaggerTerms }

  def runSwaggerSpec[L <: LA](
      spec: String
  ): (Context, FunctionK[CodegenApplication[L, ?], Target]) => (ProtocolDefinitions[L], Clients[L], Servers[L]) = {
    val parseOpts = new ParseOptions
    parseOpts.setResolve(true)
    runSwagger(new OpenAPIParser().readContents(spec, new util.LinkedList(), parseOpts).getOpenAPI) _
  }

  def runSwagger[L <: LA](swagger: OpenAPI)(context: Context, framework: FunctionK[CodegenApplication[L, ?], Target])(
      implicit F: FrameworkTerms[L, CodegenApplication[L, ?]],
      Sc: ScalaTerms[L, CodegenApplication[L, ?]],
      Sw: SwaggerTerms[L, CodegenApplication[L, ?]]
  ): (ProtocolDefinitions[L], Clients[L], Servers[L]) = {
    import F._
    import Sw._

    val (proto, CodegenDefinitions(clients, Nil, clientSupportDefs)) = Target.unsafeExtract(
      Common
        .prepareDefinitions[L, CodegenApplication[L, ?]](
          CodegenTarget.Client,
          context,
          swagger
        )
        .foldMap(framework)
    )

    val (_, CodegenDefinitions(Nil, servers, serverSupportDefs)) = Target.unsafeExtract(
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
