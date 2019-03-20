package support
import java.util

import com.twilio.guardrail.Common._
import com.twilio.guardrail.shims._
import io.swagger.parser.OpenAPIParser
import io.swagger.v3.parser.core.models.ParseOptions

trait SwaggerSpecRunner {

  import _root_.io.swagger.v3.oas.models._
  import cats.arrow.FunctionK
  import cats.data.NonEmptyList
  import cats.implicits._
  import com.twilio.guardrail._
  import com.twilio.guardrail.languages.ScalaLanguage
  import com.twilio.guardrail.terms.framework.FrameworkTerms
  import com.twilio.guardrail.terms.{ ScalaTerms, SwaggerTerms }
  import scala.collection.JavaConverters._
  import java.net.URI

  def runSwaggerSpec(
      spec: String
  ): (Context, FunctionK[CodegenApplication[ScalaLanguage, ?], Target]) => (ProtocolDefinitions[ScalaLanguage], Clients[ScalaLanguage], Servers[ScalaLanguage]) = {
    val parseOpts = new ParseOptions
    parseOpts.setResolve(true)
    runSwagger(new OpenAPIParser().readContents(spec, new util.LinkedList(), parseOpts).getOpenAPI) _
  }

  def runSwagger(swagger: OpenAPI)(context: Context, framework: FunctionK[CodegenApplication[ScalaLanguage, ?], Target])(
      implicit F: FrameworkTerms[ScalaLanguage, CodegenApplication[ScalaLanguage, ?]],
      Sc: ScalaTerms[ScalaLanguage, CodegenApplication[ScalaLanguage, ?]],
      Sw: SwaggerTerms[ScalaLanguage, CodegenApplication[ScalaLanguage, ?]]
  ): (ProtocolDefinitions[ScalaLanguage], Clients[ScalaLanguage], Servers[ScalaLanguage]) = {
    import F._
    import Sw._

    val (proto, CodegenDefinitions(clients, Nil, Nil)) = Target.unsafeExtract(
      Common
        .prepareDefinitions[ScalaLanguage, CodegenApplication[ScalaLanguage, ?]](
          CodegenTarget.Client,
          context,
          swagger
        )
        .foldMap(framework)
    )

    val (_, CodegenDefinitions(Nil, servers, Nil)) = Target.unsafeExtract(
      Common
        .prepareDefinitions[ScalaLanguage, CodegenApplication[ScalaLanguage, ?]](
          CodegenTarget.Server,
          context,
          swagger
        )
        .foldMap(framework)
    )

    (proto, Clients(clients, Nil), Servers(servers, Nil))
  }

}
