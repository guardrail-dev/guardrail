package tests.generators.dropwizardScala.server

import cats.data.NonEmptyList
import dev.guardrail.{ CodegenTarget, Context }
import dev.guardrail.generators.{ Server, Servers }
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter

import scala.meta._
import support.SwaggerSpecRunner

class DropwizardScalaJsr310Test extends AnyFreeSpec with Matchers with OptionValues with SwaggerSpecRunner {
  private val openapi =
    s"""openapi: 3.0.2
       |info:
       |  title: dw-scala date/time tests
       |  version: 1.0
       |paths:
       |  /foo/{when}:
       |    post:
       |      operationId: doFoo
       |      parameters:
       |        - name: when
       |          in: path
       |          required: true
       |          schema:
       |            type: string
       |            format: date-time
       |        - name: date-time
       |          in: query
       |          required: false
       |          schema:
       |            type: string
       |            format: date-time
       |      requestBody:
       |        required: true
       |        content:
       |          application/x-www-form-urlencoded:
       |            schema:
       |              type: object
       |              required:
       |                - date
       |              properties:
       |                date:
       |                  type: string
       |                  format: date
       |      responses:
       |        200: {}
       |""".stripMargin

  "Date/time params should use Param indirection" in {
    val (
      _,
      _,
      Servers(Server(_, _, handler, server) :: Nil, _)
    ) = runSwaggerSpec(scalaInterpreter)(openapi)(Context.empty, "dropwizard", targets = NonEmptyList.of(CodegenTarget.Server))

    handler match {
      case Defn.Trait.After_4_6_0(
            _,
            Type.Name("Handler"),
            _,
            _,
            Template.After_4_4_0(_, _, _, List(defn: Decl.Def), _)
          ) =>
        val List(
          Term.ParamClause(List(_), None),
          Term.ParamClause(List(
            Term.Param(_, Term.Name("when"), Some(t"java.time.OffsetDateTime"), None),
            Term.Param(_, Term.Name("dateTime"), Some(t"Option[java.time.OffsetDateTime]"), None),
            Term.Param(_, Term.Name("date"), Some(t"java.time.LocalDate"), None)
          ), None)
        ) = defn.paramClauses
    }

    val resourceDefns = server.collectFirst { case Defn.Class.After_4_6_0(_, t"Resource", _, _, Template.After_4_4_0(_, _, _, defns, _)) =>
      defns
    }.value
    val doFooParamss: List[Member.ParamClauseGroup] = resourceDefns.collectFirst { case Defn.Def.After_4_7_3(_, Term.Name("doFoo"), paramClauseGroups, _, _) =>
      paramClauseGroups
    }.value

    val List(
      Member.ParamClauseGroup(
        _,
        List(
          Term.ParamClause(List(
            Term.Param(_, Term.Name("when"), Some(t"GuardrailJerseySupport.Jsr310.OffsetDateTimeParam"), _),
            Term.Param(_, Term.Name("dateTime"), Some(t"Option[GuardrailJerseySupport.Jsr310.OffsetDateTimeParam]"), _),
            Term.Param(_, Term.Name("date"), Some(t"GuardrailJerseySupport.Jsr310.LocalDateParam"), _),
            Term.Param(_, Term.Name("asyncResponse"), Some(t"AsyncResponse"), _)
          ), None)
        )
      )
    ) = doFooParamss
  }
}
