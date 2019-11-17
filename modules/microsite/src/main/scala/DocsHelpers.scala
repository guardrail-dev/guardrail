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
case object GeneratingClients extends SnippetComponent

object DocsHelpers {
  def sampleSpec = "modules/microsite/docs/sample-user.json"
  def renderScalaSnippet(generator: FunctionK[CodegenApplication[ScalaLanguage, ?], Target], identifier: SnippetComponent)(prefix: String, suffix: String): Unit = {
    val parseOpts = new ParseOptions
    parseOpts.setResolve(true)
    val openAPI = Tracker(new OpenAPIParser().readLocation(sampleSpec, new java.util.LinkedList(), parseOpts).getOpenAPI)

    val segments: List[Option[String]] = identifier match {
      case GeneratingAServer =>
        val (_, codegenDefinitions) = Target.unsafeExtract(
          Common.prepareDefinitions[ScalaLanguage, CodegenApplication[ScalaLanguage, ?]](CodegenTarget.Server, Context.empty, openAPI, List("definitions"))
            .foldMap(generator)
        )
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
      case GeneratingClients =>
        val (_, codegenDefinitions) = Target.unsafeExtract(
          Common.prepareDefinitions[ScalaLanguage, CodegenApplication[ScalaLanguage, ?]](CodegenTarget.Client, Context.empty, openAPI, List("definitions"))
            .foldMap(generator)
        )
        codegenDefinitions.clients match {
          case g :: Nil =>
            val StaticDefns(className, extraImports, definitions) = g.staticDefns
            val o = q"object ${Term.Name(className)} { ..${definitions} }"
            val q"""class ${name}(...${args}) {
              ..${defns}
            }""" = g.client.head.right.get
            val basePath = defns.collectFirst {
              case v@q"val basePath: String = $_" => v
            }
            val (firstName, firstDefn) = defns.collectFirst({
              case q"def ${name}(...${args}): $tpe = $body" => (name, q"def ${name}(...${args}): $tpe = $body")
            }).toList.unzip
            val rest = defns.collect {
              case q"def ${name}(...${args}): $tpe = $_" if ! firstName.contains(name) => q"def ${name}(...${args}): $tpe = ???"
            }
            val matched = (basePath ++ firstDefn ++ rest).toList
            val c = q"""class ${name}(...${args}) {
              ..${matched}
            }"""

            List(
              Some(o.toString),
              Some(""),
              Some(c.toString)
            )
          case _ => ???
        }
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
