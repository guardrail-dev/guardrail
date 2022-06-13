package dev.guardrail.generators.spi

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues
import scala.util.Try

class ModuleLoadResultSpec extends AnyFunSuite with Matchers with OptionValues {
  test("ModuleLoadResult.forProduct1") {
    val extractor = ModuleLoadResult.forProduct1( Seq((x: String) => Try(x.toBoolean).toOption)) { b => b }

    extractor(Set("foop", "true")).value shouldBe true
  }

  test("ModuleLoadResult.forProduct2") {
    val extractor = ModuleLoadResult.forProduct2(
      Seq((x: String) => Try(x.toBoolean).toOption),
      Seq((x: String) => Try(x.toLong).toOption)
    ) { (a, b) => (a, b) }

    extractor(Set("foop", "true", "5")).value shouldBe ((true, 5L))
  }

  test("ModuleLoadResult.forProduct3") {
    {
      val extractor = ModuleLoadResult.forProduct3(
        Seq((x: String) => Try(x.toBoolean).toOption),
        Seq((x: String) => Try(x.toLong).toOption),
        Seq((x: String) => Option(x).filter(_.nonEmpty)), // Expect the first string to be found
      ) { (a, b, c) => (a, b, c) }

      extractor(Set("foop", "true", "5", "boop")).value shouldBe ((true, 5L, "5"))
    }

    {
      val extractor = ModuleLoadResult.forProduct3(
        Seq((x: String) => Try(x.toBoolean).toOption),
        Seq((x: String) => Try(x.toLong).toOption),
        Seq((x: String) => Option(x).filter(_ == "foop")),
      ) { (a, b, c) => (a, b, c) }

      extractor(Set("foop", "true", "5", "boop")).value shouldBe ((true, 5L, "foop"))
    }
  }

}
