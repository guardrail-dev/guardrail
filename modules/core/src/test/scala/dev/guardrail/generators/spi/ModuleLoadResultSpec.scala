package dev.guardrail.generators.spi

import cats.implicits._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues

class ModuleLoadResultSpec extends AnyFunSuite with Matchers with OptionValues {
  private def getMissing[A](value: ModuleLoadResult[A]): Set[String] = value match {
    case x: ModuleLoadFailed => x.choices.keys.toSet
    case _                   => fail("Expected failure")
  }

  private def getConsumed[A](value: ModuleLoadResult[A]): Set[String] = value match {
    case x: ModuleLoadSuccess[A] => x.modulesConsumed
    case _                       => fail("Expected success")
  }

  test("Combine Tuple2") {
    val a: ModuleLoadResult[Long]   = ModuleLoadFailed.of(Set.empty, Set.empty, Map("foo" -> Set.empty))
    val b: ModuleLoadResult[String] = ModuleLoadFailed.of(Set.empty, Set.empty, Map("bar" -> Set.empty))
    val c: ModuleLoadResult[String] = new ModuleLoadSuccess(Set("c"), Set("C"), "woo")
    val d: ModuleLoadResult[String] = new ModuleLoadSuccess(Set("d"), Set("D"), "woo")

    getMissing((a, b).mapN(Tuple2.apply)) should contain theSameElementsAs (Set("foo", "bar"))
    getMissing((a, c).mapN(Tuple2.apply)) should contain theSameElementsAs (Set("foo"))
    getMissing((c, a).mapN(Tuple2.apply)) should contain theSameElementsAs (Set("foo"))

    getConsumed((c, d).mapN(Tuple2.apply)) should contain theSameElementsAs (Set("c", "d"))
  }

  test("ModuleLoadResult forProduct2") {
    val a: Map[String, Boolean] = Map("false" -> false)
    val b: Map[String, Long]    = Map("1" -> 1L)
    val extractor = ModuleLoadResult.forProduct2(
      ("A Component", Seq(a)),
      ("B Component", Seq(b))
    )((a, b) => (a, b))

    getMissing(extractor(Set("a", "b", "c", "d"))) should contain theSameElementsAs (Set("A Component", "B Component"))
  }

  test("ModuleLoadResult forProduct4") {
    val a: Map[String, Boolean] = Map("false" -> false)
    val b: Map[String, Long]    = Map("1" -> 1L)
    val c: Map[String, Int]     = Map("1" -> 2)
    val d: Map[String, Char]    = Map("c" -> 'c')
    val extractor = ModuleLoadResult.forProduct4(
      ("A Component", Seq(a)),
      ("B Component", Seq(b)),
      ("C Component", Seq(c)),
      ("D Component", Seq(d))
    )((a, b, c, d) => (a, b, c, d))

    getMissing(extractor(Set.empty)) should contain theSameElementsAs (Set(
      "A Component",
      "B Component",
      "C Component",
      "D Component"
    ))
  }

  test("ModuleLoadResult forProduct5") {
    val a: Map[String, Boolean] = Map("false" -> false)
    val b: Map[String, Long]    = Map("1" -> 1L)
    val c: Map[String, Int]     = Map("1" -> 2)
    val d: Map[String, Char]    = Map("c" -> 'c')
    val e: Map[String, Float]   = Map("pi" -> 3.14f)
    val extractor = ModuleLoadResult.forProduct5(
      ("A Component", Seq(a)),
      ("B Component", Seq(b)),
      ("C Component", Seq(c)),
      ("D Component", Seq(d)),
      ("E Component", Seq(e))
    )((a, b, c, d, e) => (a, b, c, d, e))

    getMissing(extractor(Set.empty)) should contain theSameElementsAs (Set(
      "A Component",
      "B Component",
      "C Component",
      "D Component",
      "E Component"
    ))
  }

  test("ModuleLoadResult forProduct6") {
    val a: Map[String, Boolean] = Map("false" -> false)
    val b: Map[String, Long]    = Map("1" -> 1L)
    val c: Map[String, Int]     = Map("1" -> 2)
    val d: Map[String, Char]    = Map("c" -> 'c')
    val e: Map[String, Float]   = Map("pi" -> 3.14f)
    val f: Map[String, String]  = Map("foo" -> "foo")
    val extractor = ModuleLoadResult.forProduct6(
      ("A Component", Seq(a)),
      ("B Component", Seq(b)),
      ("C Component", Seq(c)),
      ("D Component", Seq(d)),
      ("E Component", Seq(e)),
      ("F Component", Seq(f))
    )((a, b, c, d, e, f) => (a, b, c, d, e, f))

    getMissing(extractor(Set.empty)) should contain theSameElementsAs (Set(
      "A Component",
      "B Component",
      "C Component",
      "D Component",
      "E Component",
      "F Component"
    ))
  }

}
