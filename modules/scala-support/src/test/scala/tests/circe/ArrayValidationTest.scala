package tests.circe

import cats.data.NonEmptyList
import dev.guardrail.Target
import dev.guardrail.core.Tracker
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.ProtocolGenerator
import dev.guardrail.generators.SwaggerGenerator
import dev.guardrail.generators.scala.CirceRefinedModelGenerator
import dev.guardrail.generators.scala.ScalaCollectionsGenerator
import dev.guardrail.generators.scala.ScalaGenerator
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.generators.scala.circe.CirceRefinedProtocolGenerator
import dev.guardrail.terms.ProtocolTerms
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.protocol.ClassDefinition
import dev.guardrail.terms.protocol.PropertyRequirement
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import scala.meta._
import support.SwaggerSpecRunner

class ArrayValidationTest extends AnyFreeSpec with Matchers with SwaggerSpecRunner {

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
       |  ValidatedCollections:
       |    type: object
       |    properties:
       |      bounded_size_array:
       |        type: array
       |        items:
       |          type: integer
       |          format: int32
       |        minItems: 1
       |        maxItems: 10
       |""".stripMargin

  "Validation" - {

    "should work" in {

      implicit def CollectionsLibInterp = ScalaCollectionsGenerator()
      implicit val mockFW = new FrameworkTerms[ScalaLanguage, Target] {
        def MonadF                                                                                                    = Target.targetInstances
        def fileType(format: Option[String]): dev.guardrail.Target[dev.guardrail.generators.scala.ScalaLanguage#Type] = Target.pure(t"String")
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

      val expected =
        q"""case class ValidatedCollections(boundedSizeArray: Option[Vector[Int] Refined
           _root_.eu.timepit.refined.collection.Size[_root_.eu.timepit.refined.numeric.Interval.Closed[_root_.shapeless.Witness.`1`.T, _root_.shapeless.Witness.`10`.T]]] = None)"""

      println(cls)

      cls.structure should equal(expected.structure)
    }

  }
}
