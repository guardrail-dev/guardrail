package com.twilio.guardrail.docs

import cats.arrow.FunctionK
import io.swagger.parser.OpenAPIParser
import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.parser.core.models.ParseOptions
import com.twilio.guardrail._
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.protocol.terms.server.{ ServerTerm, ServerTerms }
import scala.meta._

sealed trait SnippetComponent
case object GeneratingAServer extends SnippetComponent

object DocsHelpers {
  def sampleSpec = "modules/microsite/docs/sample-user.json"
  def renderScalaSnippet(generator: FunctionK[CodegenApplication[ScalaLanguage, ?], Target], identifier: SnippetComponent)(prefix: String, suffix: String): Unit = {
    val parseOpts = new ParseOptions
    parseOpts.setResolve(true)
    val openAPI = Tracker(new OpenAPIParser().readLocation(sampleSpec, new java.util.LinkedList(), parseOpts).getOpenAPI)

    val segments: List[Option[String]] = identifier match {
      case GeneratingAServer =>
        val (_, codegenDefinitions) = Target.unsafeExtract(Common.prepareDefinitions[ScalaLanguage, CodegenApplication[ScalaLanguage, ?]](CodegenTarget.Server, Context.empty, openAPI).foldMap(generator))
        val server = codegenDefinitions.servers.head
        val q"object ${oname} { ..${stats} }" = server.serverDefinitions.head
        val routeStats = stats.collectFirst({ case line@q"def routes(...$_): $_ = $_" => line }).toList
        List(
          Some(server.handlerDefinition.toString),
          Some(""),
          Some(q"""
            object ${oname} {
              ..${routeStats};
              `...`
            }
          """.toString)
        )
    }

    println((
      List[Option[String]](
        Some("```scala"),
        Option(prefix).filter(_.nonEmpty),
      ) ++ segments ++ List[Option[String]](
        Option(suffix).filter(_.nonEmpty),
        Some("```")
      )
    ).flatten.mkString("\n"))
  }
}
