package support

trait SwaggerSpecRunner {

  import _root_.io.swagger.models._
  import _root_.io.swagger.parser.SwaggerParser
  import cats.arrow.FunctionK
  import cats.implicits._
  import com.twilio.guardrail._
  import com.twilio.guardrail.terms.framework.FrameworkTerms
  import com.twilio.guardrail.terms.{ ScalaTerms, SwaggerTerms }
  import com.twilio.guardrail.generators.GeneratorSettings

  import scala.collection.JavaConverters._

  def runSwaggerSpec(
      spec: String
  ): (Context, FunctionK[CodegenApplication, Target], GeneratorSettings) => (ProtocolDefinitions, Clients, Servers) =
    runSwagger(new SwaggerParser().parse(spec)) _

  def runSwagger(swagger: Swagger)(context: Context, framework: FunctionK[CodegenApplication, Target], generatorSettings: GeneratorSettings)(
      implicit F: FrameworkTerms[CodegenApplication],
      Sc: ScalaTerms[CodegenApplication],
      Sw: SwaggerTerms[CodegenApplication]
  ): (ProtocolDefinitions, Clients, Servers) = {
    import F._
    import Sw._

    val prog = for {
      protocol <- ProtocolGenerator.fromSwagger[CodegenApplication](swagger)
      definitions = protocol.elems

      schemes = Option(swagger.getSchemes)
        .fold(List.empty[String])(_.asScala.to[List].map(_.toValue))
      host     = Option(swagger.getHost)
      basePath = Option(swagger.getBasePath)
      paths = Option(swagger.getPaths)
        .map(_.asScala.toList)
        .getOrElse(List.empty)
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
        .fromSwagger[CodegenApplication](context, frameworkImports)(schemes, host, basePath, groupedRoutes)(definitions)
      servers <- ServerGenerator
        .fromSwagger[CodegenApplication](context, swagger, frameworkImports)(definitions)
    } yield (protocol, clients, servers)

    Target.unsafeExtract(prog.foldMap(framework), generatorSettings)
  }

}
