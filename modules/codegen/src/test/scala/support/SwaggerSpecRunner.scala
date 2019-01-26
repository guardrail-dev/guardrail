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
  ): (Context, FunctionK[CodegenApplication[ScalaLanguage, ?], Target]) => (ProtocolDefinitions[ScalaLanguage], Clients[ScalaLanguage], Servers[ScalaLanguage]) =
    runSwagger(new OpenAPIParser().readContents(spec, new util.LinkedList(), new ParseOptions).getOpenAPI) _

  def runSwagger(swagger: OpenAPI)(context: Context, framework: FunctionK[CodegenApplication[ScalaLanguage, ?], Target])(
      implicit F: FrameworkTerms[ScalaLanguage, CodegenApplication[ScalaLanguage, ?]],
      Sc: ScalaTerms[ScalaLanguage, CodegenApplication[ScalaLanguage, ?]],
      Sw: SwaggerTerms[ScalaLanguage, CodegenApplication[ScalaLanguage, ?]]
  ): (ProtocolDefinitions[ScalaLanguage], Clients[ScalaLanguage], Servers[ScalaLanguage]) = {
    import F._
    import Sw._

    val prog = for {
      protocol <- ProtocolGenerator.fromSwagger[ScalaLanguage, CodegenApplication[ScalaLanguage, ?]](swagger)
      definitions = protocol.elems

      serverUrls = Option(swagger.getServers)
        .map(_.asScala.toList)
        .flatMap(NonEmptyList.fromList(_))
        .map(_.map(x => new URI(x.getUrl().stripSuffix("/"))))
      basePath = swagger.basePath()
      paths    = swagger.getPathsOpt()

      routes <- extractOperations(paths)
      classNamedRoutes <- routes
        .map(route => getClassName(route.operation).map(_ -> route))
        .sequence
      groupedRoutes = classNamedRoutes
        .groupBy(_._1)
        .mapValues(_.map(_._2))
        .toList
      frameworkImports <- getFrameworkImports(context.tracing)

      clients <- ClientGenerator
        .fromSwagger[ScalaLanguage, CodegenApplication[ScalaLanguage, ?]](context, frameworkImports)(serverUrls, basePath, groupedRoutes)(definitions)
      servers <- ServerGenerator
        .fromSwagger[ScalaLanguage, CodegenApplication[ScalaLanguage, ?]](context, swagger, frameworkImports)(definitions)
    } yield (protocol, clients, servers)

    Target.unsafeExtract(prog.foldMap(framework))
  }

}
