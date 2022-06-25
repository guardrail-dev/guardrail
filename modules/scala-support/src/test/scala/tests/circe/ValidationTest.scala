package tests.circe

import cats.data.NonEmptyList
import dev.guardrail.Target
import dev.guardrail.core.Tracker
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.ProtocolGenerator
import dev.guardrail.generators.SwaggerGenerator
import dev.guardrail.generators.scala.ScalaCollectionsGenerator
import dev.guardrail.generators.scala.ScalaGenerator
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.generators.scala.circe.CirceRefinedProtocolGenerator
import dev.guardrail.generators.scala.syntax.companionForStaticDefns
import dev.guardrail.generators.scala.{ CirceRefinedModelGenerator, ScalaCollectionsGenerator, ScalaGenerator, ScalaLanguage }
import dev.guardrail.generators.{ ProtocolDefinitions, ProtocolGenerator, SwaggerGenerator }
import dev.guardrail.terms.ProtocolTerms
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.protocol.ClassDefinition
import dev.guardrail.terms.protocol.PropertyRequirement
import dev.guardrail.terms.protocol.{ ClassDefinition, PropertyRequirement }
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import scala.meta._
import support.SwaggerSpecRunner
import support.SwaggerSpecRunner

class ValidationTest extends AnyFreeSpec with Matchers with SwaggerSpecRunner {

  val swagger: String =
    s"""
     |swagger: "2.0"
     |info:
     |  title: Whatever
     |  version: 1.0.0
     |host: localhost:1234
     |schemes:
     |  - http
     |definitions:
     |  ValidatedObject:
     |    type: object
     |    properties:
     |      v1:
     |        type: integer
     |        format: int32
     |        default: 10
     |        maximum: 100
     |        minimum: 1
     |""".stripMargin

  "Validation" - {

    "should work" in {

      implicit def CollectionsLibInterp = ScalaCollectionsGenerator()
      implicit val mockFW = new FrameworkTerms[ScalaLanguage, Target] {
        def MonadF                                                                                                    = Target.targetInstances
        def fileType(format: Option[String]): dev.guardrail.Target[dev.guardrail.generators.scala.ScalaLanguage#Type] = ???
        def getFrameworkDefinitions(
            tracing: Boolean
        ): dev.guardrail.Target[List[(dev.guardrail.generators.scala.ScalaLanguage#TermName, List[dev.guardrail.generators.scala.ScalaLanguage#Definition])]] =
          ???
        def getFrameworkImplicits(): dev.guardrail.Target[Option[
          (dev.guardrail.generators.scala.ScalaLanguage#TermName, dev.guardrail.generators.scala.ScalaLanguage#ObjectDefinition)
        ]] = ???
        def getFrameworkImports(tracing: Boolean): dev.guardrail.Target[List[dev.guardrail.generators.scala.ScalaLanguage#Import]] = ???
        def lookupStatusCode(key: String): dev.guardrail.Target[(Int, dev.guardrail.generators.scala.ScalaLanguage#TermName)]      = ???
        def objectType(format: Option[String]): dev.guardrail.Target[dev.guardrail.generators.scala.ScalaLanguage#Type] = Target.pure(t"io.circe.Json")
      }
      implicit val circeProtocolGenerator: ProtocolTerms[ScalaLanguage, Target] = CirceRefinedProtocolGenerator(CirceRefinedModelGenerator.V012)
      implicit val scalaGenerator                                               = ScalaGenerator()
      implicit val swaggerGenerator                                             = SwaggerGenerator[ScalaLanguage]()
      val ProtocolDefinitions(ClassDefinition(_, _, _, cls, staticDefns, _) :: Nil, _, _, _, _) = ProtocolGenerator
        .fromSwagger[ScalaLanguage, Target](
          Tracker(swaggerFromString(swagger)),
          dtoPackage = Nil,
          supportPackage = NonEmptyList.one("foop"),
          defaultPropertyRequirement = PropertyRequirement.OptionalLegacy
        )
        .value
      val cmp = companionForStaticDefns(staticDefns)

      val expected = q"""case class ValidatedObject(v1: Option[Int Refined Interval.Closed[Witness.`1`.T, Witness.`100`.T]] = Option(10))"""

      cls.structure should equal(expected.structure)
    }

  }

}
