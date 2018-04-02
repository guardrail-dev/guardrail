package com.twilio

import _root_.io.swagger.models._
import _root_.io.swagger.parser.SwaggerParser
import cats.arrow.FunctionK
import com.twilio.swagger.codegen._
import com.twilio.swagger.codegen.generators.AkkaHttp
import org.scalatest._
import scala.collection.JavaConverters._
import scala.meta._
import com.twilio.swagger.codegen.terms.{ScalaTerms, SwaggerTerms}
import com.twilio.swagger.codegen.terms.framework.FrameworkTerms
import cats.implicits._

package object swagger {
  def runSwaggerSpec(spec: String)(context: Context, framework: FunctionK[CodegenApplication, Target]): (ProtocolDefinitions, Clients, Servers) = runSwagger(new SwaggerParser().parse(spec))(context, framework)
  def runSwagger(swagger: Swagger)(context: Context, framework: FunctionK[CodegenApplication, Target]
      )(implicit F: FrameworkTerms[CodegenApplication], Sc: ScalaTerms[CodegenApplication], Sw: SwaggerTerms[CodegenApplication]
      ): (ProtocolDefinitions, Clients, Servers) = {
    import F._
    import Sc._
    import Sw._

    val prog = for {
      protocol <- ProtocolGenerator.fromSwagger[CodegenApplication](swagger)
      definitions = protocol.elems

      schemes = Option(swagger.getSchemes).fold(List.empty[String])(_.asScala.to[List].map(_.toValue))
      host = Option(swagger.getHost)
      basePath = Option(swagger.getBasePath)
      paths = Option(swagger.getPaths).map(_.asScala.toList).getOrElse(List.empty)
      routes <- extractOperations(paths)
      classNamedRoutes <- routes.map(route => getClassName(route.operation).map(_ -> route)).sequence
      groupedRoutes = classNamedRoutes.groupBy(_._1).mapValues(_.map(_._2)).toList
      frameworkImports <- getFrameworkImports(context.tracing)

      clients <- ClientGenerator.fromSwagger[CodegenApplication](context, frameworkImports)(schemes, host, basePath, groupedRoutes)(definitions)
      servers <- ServerGenerator.fromSwagger[CodegenApplication](context, swagger, frameworkImports)(definitions)
    } yield (protocol, clients, servers)

    Target.unsafeExtract(prog.foldMap(framework))
  }
}
