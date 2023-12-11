package dev.guardrail.docs

import cats.data.NonEmptyList

import io.swagger.parser.OpenAPIParser
import io.swagger.v3.parser.core.models.ParseOptions
import scala.meta._

import dev.guardrail._
import dev.guardrail.core.Tracker
import dev.guardrail.generators.Framework
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.terms.protocol.StaticDefns
import dev.guardrail.generators.spi.{ FrameworkLoader, ModuleMapperLoader }

sealed trait SnippetComponent
case object GeneratingAServer extends SnippetComponent
case object GeneratingClients extends SnippetComponent

@SuppressWarnings(Array("org.wartremover.warts.EitherProjectionPartial", "org.wartremover.warts.TraversableOps"))
object DocsHelpers {
  def sampleSpec = "modules/microsite/docs/sample-user.json"
  def renderScalaSnippet(framework: String, identifier: SnippetComponent)(prefix: String, suffix: String): Unit = {
    val generator: Framework[ScalaLanguage, Target] = Target.unsafeExtract(for {
      modules   <- ModuleMapperLoader.load[ScalaLanguage](framework)
      framework <- FrameworkLoader.load[ScalaLanguage](modules)
    } yield framework)
    import generator._
    val parseOpts = new ParseOptions
    parseOpts.setResolve(true)
    val openAPI = Tracker(new OpenAPIParser().readLocation(sampleSpec, new java.util.LinkedList(), parseOpts).getOpenAPI)

    val segments: List[Option[String]] = (framework, identifier) match {
      case ("akka-http", GeneratingAServer) =>
        val (_, codegenDefinitions) = Target.unsafeExtract(
          Common.prepareDefinitions[ScalaLanguage, Target](CodegenTarget.Server, Context.empty, openAPI, List("definitions"), NonEmptyList.one("support"))
        )
        val server                            = codegenDefinitions.servers.head
        val q"object ${oname} { ..${stats} }" = server.serverDefinitions.head: @unchecked
        val routeStats                        = stats.collectFirst { case line @ q"def routes(...$_): $_ = $_" => line }.toList
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
      case ("akka-http", GeneratingClients) =>
        val (_, codegenDefinitions) = Target.unsafeExtract(
          Common.prepareDefinitions[ScalaLanguage, Target](CodegenTarget.Client, Context.empty, openAPI, List("definitions"), NonEmptyList.one("support"))
        )
        codegenDefinitions.clients match {
          case g :: Nil =>
            val StaticDefns(className, extraImports, definitions, statements) = g.staticDefns
            val o                                                             = q"object ${Term.Name(className)} { ..${definitions} }"
            val Right(q"""class ${name}(...${args}) {
              ..${defns}
            }""") = g.client.head: @unchecked
            val basePath = defns.collectFirst { case v @ q"val basePath: String = $_" =>
              v
            }
            val (firstName, firstDefn) = defns
              .collectFirst { case q"def ${name}(...${args}): $tpe = $body" =>
                (name, q"def ${name}(...${args}): $tpe = $body")
              }
              .toList
              .unzip
            val rest = defns.collect {
              case q"def ${name}(...${args}): $tpe = $_" if !firstName.contains(name) => q"def ${name}(...${args}): $tpe = ???"
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
      case ("http4s", GeneratingAServer) =>
        val (_, codegenDefinitions) = Target.unsafeExtract(
          Common.prepareDefinitions[ScalaLanguage, Target](CodegenTarget.Server, Context.empty, openAPI, List("definitions"), NonEmptyList.one("support"))
        )
        val server = codegenDefinitions.servers.head
        val q"""
          class ${oname}[..${tparms}](...${resourceParams}) extends ..${xtends} {
            ..${stats}
          }
        """ = server.serverDefinitions.head: @unchecked

        val routeStats = stats.collectFirst { case q"""def routes(...$parms): $rtpe = HttpRoutes.of(${Term.Block(List(Term.PartialFunction(cases)))})""" =>
          q"""
          def routes(...${parms}): ${rtpe} = ${Term.Apply(
              fun = q"HttpRoutes.of",
              argClause = Term.ArgClause(List(Term.Block(stats = List(Term.PartialFunction(cases = cases.take(2))))))
            )}
        """
        }.toList
        List(
          Some(server.handlerDefinition.toString),
          Some(""),
          Some(q"""
            class ${oname}[..${tparms}](...${resourceParams}) extends ..${xtends} {
              ..${routeStats};
              `...`
            }
          """.toString)
        )
      case ("http4s", GeneratingClients) =>
        val (_, codegenDefinitions) = Target.unsafeExtract(
          Common.prepareDefinitions[ScalaLanguage, Target](CodegenTarget.Client, Context.empty, openAPI, List("definitions"), NonEmptyList.one("support"))
        )
        // codegenDefinitions.clients.map(x => Option(x.toString())).toList
        codegenDefinitions.clients match {
          case g :: Nil =>
            val StaticDefns(className, extraImports, definitions, statements) = g.staticDefns
            val o                                                             = q"object ${Term.Name(className)} { ..${definitions} }"
            val Right(q"""class ${name}[..${tparms}](...${args}) {
              ..${defns}
            }""") = g.client.head: @unchecked
            val basePath = defns.collectFirst { case v @ q"val basePath: String = $_" =>
              v
            }
            val (firstName, firstDefn) = defns
              .collectFirst { case q"def ${name}(...${args}): $tpe = $body" =>
                (name, q"def ${name}(...${args}): $tpe = $body")
              }
              .toList
              .unzip
            val rest = defns.collect {
              case q"def ${name}(...${args}): $tpe = $_" if !firstName.contains(name) => q"def ${name}(...${args}): $tpe = ???"
            }
            val matched = (basePath ++ firstDefn ++ rest).toList
            val c = q"""class ${name}[..${tparms}](...${args}) {
              ..${matched}
            }"""

            List(
              Some(o.toString),
              Some(""),
              Some(c.toString)
            )
          case _ => ???
        }
      case (framework, term) =>
        List(Some(s"Unknown docs sample: ${framework}/${term.toString()}"))
    }

    println(
      (
        List[Option[String]](
          Some("```scala"),
          Option(prefix).filter(_.nonEmpty)
        ) ++ segments ++ List[Option[String]](
          Option(suffix).filter(_.nonEmpty),
          Some("```")
        )
      ).flatten.mkString("\n")
    )
  }
}
