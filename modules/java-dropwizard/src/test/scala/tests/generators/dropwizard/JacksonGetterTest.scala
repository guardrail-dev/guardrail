package tests.generators.dropwizard

import cats.data.NonEmptyList
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import support.SwaggerSpecRunner

import dev.guardrail.CodegenTarget
import dev.guardrail.Context
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.java.JavaLanguage
import dev.guardrail.generators.java.dropwizard.Dropwizard
import dev.guardrail.terms.protocol.ClassDefinition

class JacksonGetterTest extends AnyFreeSpec with Matchers with SwaggerSpecRunner {
  private val openapi =
    s"""openapi: 3.0.2
       |info:
       |  title: Jackson Getter Test
       |  version: 1.0.0
       |paths: {}
       |components:
       |  schemas:
       |    Foo:
       |      type: object
       |      required:
       |        - class
       |        - blah
       |      properties:
       |        class:
       |          type: string
       |        blah:
       |          type: string
       |""".stripMargin

  "Jackson POJO getter for property called 'class' should not clash with Object.getClass()" in {
    val (ProtocolDefinitions(ClassDefinition("Foo", _, _, classDef, _, _) :: Nil, _, _, _, _), _, _) =
      runSwaggerSpec[JavaLanguage](openapi)(Context.empty, Dropwizard, targets = NonEmptyList.of(CodegenTarget.Models))

    classDef.getMethodsByName("getClass").size mustBe 0
    classDef.getMethodsByName("getClass_").size mustBe 1

    classDef.getMethodsByName("getBlah_").size mustBe 0
    classDef.getMethodsByName("getBlah").size mustBe 1
  }
}
