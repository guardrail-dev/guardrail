package tests.circe

import cats.data.NonEmptyList
import dev.guardrail.Target
import dev.guardrail.core.Tracker
import dev.guardrail.generators.ProtocolDefinitions
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
import support.{ ScalaMetaMatchers, SwaggerSpecRunner }

class ArrayValidationTest extends AnyFreeSpec with Matchers with SwaggerSpecRunner with ScalaMetaMatchers {

  val spec: String =
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
       |          minimum: 1
       |        minItems: 1
       |        maxItems: 10
       |""".stripMargin

  "Array Validation" - {

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
      def objectType(format: Option[String]): dev.guardrail.Target[dev.guardrail.generators.scala.ScalaLanguage#Type]            = Target.pure(t"io.circe.Json")
    }
    implicit val circeProtocolGenerator: ProtocolTerms[ScalaLanguage, Target] = CirceRefinedProtocolGenerator(CirceRefinedModelGenerator.V012)
    implicit val scalaGenerator                                               = ScalaGenerator()
    implicit val swaggerGenerator                                             = SwaggerGenerator[ScalaLanguage]()

    "pattern" in {

      val collectionElementsWithPattern: String =
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
           |          type: string
           |          pattern: "pet"
           |        minItems: 1
           |        maxItems: 10
           |      should_not_collide:
           |        type: string
           |        pattern: "pet"
           |""".stripMargin

      val ProtocolDefinitions(ClassDefinition(_, _, _, cls, staticDefns, _) :: Nil, _, _, _, _) = circeProtocolGenerator
        .fromSpec(
          Tracker(swaggerFromString(collectionElementsWithPattern)),
          dtoPackage = Nil,
          supportPackage = NonEmptyList.one("foop"),
          defaultPropertyRequirement = PropertyRequirement.OptionalLegacy
        )
        .value

      val regexHelperDefinition = q"""val `".*pet.*"` = _root_.shapeless.Witness(".*pet.*")"""
      staticDefns.definitions.map(_.structure) should contain(regexHelperDefinition.structure)
      staticDefns.definitions.map(_.structure).count(_ == regexHelperDefinition.structure) shouldBe 1

      val expected =
        q"""case class ValidatedCollections(
            boundedSizeArray: Option[Vector[String Refined _root_.eu.timepit.refined.string.MatchesRegex[ValidatedCollections.`".*pet.*"`.T]] Refined
              _root_.eu.timepit.refined.collection.Size[_root_.eu.timepit.refined.numeric.Interval.Closed[_root_.shapeless.Witness.`1`.T, _root_.shapeless.Witness.`10`.T]]] = None,
            shouldNotCollide: Option[String Refined _root_.eu.timepit.refined.string.MatchesRegex[ValidatedCollections.`".*pet.*"`.T]] = None)"""

      cls should matchStructure(expected)

    }

    "should generate size boundary constrains" in {

      val ProtocolDefinitions(ClassDefinition(_, _, _, cls, staticDefns, _) :: Nil, _, _, _, _) = circeProtocolGenerator
        .fromSpec(
          Tracker(swaggerFromString(spec)),
          dtoPackage = Nil,
          supportPackage = NonEmptyList.one("foop"),
          defaultPropertyRequirement = PropertyRequirement.OptionalLegacy
        )
        .value

      val expected =
        q"""case class ValidatedCollections(boundedSizeArray: Option[Vector[Int Refined _root_.eu.timepit.refined.numeric.GreaterEqual[_root_.shapeless.Witness.`1`.T]] Refined
           _root_.eu.timepit.refined.collection.Size[_root_.eu.timepit.refined.numeric.Interval.Closed[_root_.shapeless.Witness.`1`.T, _root_.shapeless.Witness.`10`.T]]] = None)"""

      cls should matchStructure(expected)
    }

  }
}
