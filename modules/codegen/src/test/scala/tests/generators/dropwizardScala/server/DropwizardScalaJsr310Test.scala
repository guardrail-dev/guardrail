package tests.generators.dropwizardScala.server

import cats.data.NonEmptyList
import dev.guardrail.{ CodegenTarget, Context, Server, Servers }
import dev.guardrail.generators.Scala.Dropwizard
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

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
    ) = runSwaggerSpec(openapi)(Context.empty, Dropwizard, targets = NonEmptyList.of(CodegenTarget.Server))

    handler match {
      case Defn.Trait(
          _,
          Type.Name("Handler"),
          _,
          _,
          Template(_, _, _, List(defn: Decl.Def))
          ) =>
        val List(
          List(_),
          List(
            Term.Param(_, Name("when"), Some(t"java.time.OffsetDateTime"), _),
            Term.Param(_, Name("dateTime"), Some(t"Option[java.time.OffsetDateTime]"), _),
            Term.Param(_, Name("date"), Some(t"java.time.LocalDate"), _)
          )
        ) = defn.paramss
    }

    val resourceDefns = server
      .collectFirst({
        case Defn.Class(_, t"Resource", _, _, Template(_, _, _, defns)) => defns
      })
      .value
    val doFooParamss = resourceDefns
      .collectFirst({
        case Defn.Def(_, Term.Name("doFoo"), _, paramss, _, _) => paramss
      })
      .value

    val List(
      List(
        Term.Param(_, Name("when"), Some(t"GuardrailJerseySupport.Jsr310.OffsetDateTimeParam"), _),
        Term.Param(_, Name("dateTime"), Some(t"Option[GuardrailJerseySupport.Jsr310.OffsetDateTimeParam]"), _),
        Term.Param(_, Name("date"), Some(t"GuardrailJerseySupport.Jsr310.LocalDateParam"), _),
        Term.Param(_, Name("asyncResponse"), Some(t"AsyncResponse"), _)
      )
    ) = doFooParamss
  }
}
