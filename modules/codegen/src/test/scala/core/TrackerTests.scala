package core

import dev.guardrail.core.{ Tracker, TrackerTestExtensions }
import dev.guardrail.core.implicits._
import dev.guardrail.generators.Scala.Http4s
import dev.guardrail.{ CodegenTarget, Context, UserError }
import org.scalacheck.{ Arbitrary, Gen }
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import support.SwaggerSpecRunner
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TrackerTests extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks with TrackerTestExtensions with SwaggerSpecRunner {
  class Parent(val child1: List[Child1], val child2: Map[String, Child2]) { override def toString(): String = s"Parent($child1, $child2)" }
  class Child1(val grandchild: Option[Grandchild1]) extends Parent(grandchild.toList, Map.empty) { override def toString(): String = s"Child1($grandchild)" }
  class Child2                                      extends Parent(List.empty, Map.empty)        { override def toString(): String = s"Child2()"            }
  class Grandchild1                                 extends Child1(Option.empty)                 { override def toString(): String = s"Grandchild1()"       }

  object Instances {
    val grandchild1: Gen[Grandchild1] = Gen.const(new Grandchild1)
    val child2: Gen[Child2]           = Gen.const(new Child2)
    val child1: Gen[Child1]           = Gen.option(grandchild1).map(new Child1(_))
    val parent: Gen[Parent] = for {
      a <- Gen.listOf(child1)
      b <- Gen.mapOf(Gen.zip(Gen.alphaLowerStr.map(_.take(5)), child2))
    } yield new Parent(a, b)
    implicit val arbitraryParent: Arbitrary[Parent] = Arbitrary(parent)
  }
  import Instances.arbitraryParent

  /*
Tracker should:
- keep track of history
- provide helpful methods to avoid boilerplate
- prevent null and auto-convert return types
   */

  "Tracker" - {
    "convert" - {
      case class Holder[A](value: A)
      "array" in {
        assert(Tracker(Holder(Array(1, 2, 3, 4))).downField("value", _.value).unwrapTracker === List(1, 2, 3, 4))
      }
      "j.u.List" in {
        val xs = new java.util.LinkedList[Int]()
        List(1, 2, 3, 4).foreach(xs.addLast(_))
        assert(Tracker(Holder[java.util.List[Int]](xs)).downField("value", _.value).unwrapTracker === List(1, 2, 3, 4))
      }
      "List" in {
        assert(Tracker(Holder(List(1, 2, 3, 4))).downField("value", _.value).unwrapTracker === List(1, 2, 3, 4))
      }
      "Option" in {
        assert(Tracker(Holder(Option(1))).downField("value", _.value).unwrapTracker === Option(1))
      }
      "j.u.Map" in {
        val xs = new java.util.HashMap[String, Int]()
        List(("foo", 1), ("bar", 2)).foreach((xs.put _).tupled)
        // Assumption here is that HashMap -> List sort order is stable. Note that foo and bar have been reversed, but it's still consistent.
        // This is necessary, as swagger-parser represents many ordered structures as Map internally.
        // We eagerly convert to List[(K, V)] to ensure order is not lost.
        assert(Tracker(Holder[java.util.Map[String, Int]](xs)).downField("value", _.value).unwrapTracker.value === List(("bar", 2), ("foo", 1)))
      }
      "Map" in {
        assert(Tracker(Holder(Map("foo" -> 1, "bar" -> 2))).downField("value", _.value).unwrapTracker.value === List(("foo", 1), ("bar", 2)))
      }
      "Paths" in {
        import _root_.io.swagger.v3.oas.models.{ PathItem, Paths }
        val xs = new Paths
        val x  = new PathItem
        List(("/foo", x)).foreach((xs.put _).tupled)
        assert(Tracker(Holder[Paths](xs)).downField("value", _.value).unwrapTracker.value === List(("/foo" -> x)))
      }
      "Fallback" in {
        assert(Tracker(Holder(Holder(5L))).downField("value", _.value).unwrapTracker === Option(Holder(5L)))
      }
    }
    "history" - {
      "downField" - {
        "list" in {
          def pattern(i: Int) = s".child1[${i}].grandchild"
          forAll { parent: Parent =>
            Tracker(parent)
              .downField("child1", _.child1)
              .cotraverse(_.downField("grandchild", _.grandchild).indexedCosequence)
              .zipWithIndex
              .foreach({ case (x, i) => x.foreach(x => assert(pattern(i) === x.showHistory)) })
          }
        }

        "map" in {
          def pattern(name: String) = s".child2.${name}"
          forAll { parent: Parent =>
            Tracker(parent)
              .downField("child2", _.child2)
              .indexedCosequence
              .value
              .foreach({ case (k, v) => assert(pattern(k) === v.showHistory) })
          }
        }
      }
    }
    "refinement" - {
      "refine" - {
        "property" in {
          val g         = new Grandchild1
          val p: Parent = new Child1(Some(g))
          assert(
            Tracker(p)
              .refine({ case x: Child1 => x })(_.downField("grandchild", _.grandchild))
              .toOption
              .flatMap(_.unwrapTracker) === Some(g)
          )
        }
        "variance" in {
          assert(Tracker(new Child2).refine({ case x: Parent => x })(identity _).isRight)
        }
      }
      "partialRefine" in {
        val p: Parent = new Child2
        assert(
          Tracker(p)
            .refine[Boolean]({ case x: Child1 => x })(_ => ???)
            .orRefine({ case x: Child2 => x })(_ => true)
            .orRefine({ case x: Grandchild1 => x })(_ => ???)
            .getOrElse(???)
        )
      }
    }
  }

  "swagger" - {
    "operationId" in {
      val swagger = s"""
        |swagger: "2.0"
        |host: localhost:1234
        |paths:
        |  /foo:
        |    get:
        |      responses:
        |        200:
        |          description: Success
        |""".stripMargin
      val (_, UserError("Missing operationId (.paths./foo.operations.GET.operationId)")) =
        runInvalidSwaggerSpec(swagger)(Context.empty, CodegenTarget.Server, Http4s)
    }

    "responses" in {
      val swagger = s"""
        |swagger: "2.0"
        |host: localhost:1234
        |paths:
        |  /foo:
        |    get:
        |      operationId: foo
        |""".stripMargin
      val (_, UserError("No responses defined for foo (.paths./foo.operations.GET.responses)")) =
        runInvalidSwaggerSpec(swagger)(Context.empty, CodegenTarget.Server, Http4s)
    }
  }
}
