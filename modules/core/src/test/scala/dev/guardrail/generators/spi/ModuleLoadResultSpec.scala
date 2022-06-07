package dev.guardrail.generators.spi

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues
import scala.util.Try

class ModuleLoadResultSpec extends AnyFunSuite with Matchers with OptionValues {
  test("ModuleLoadResult.buildFrom") {
    val extractor = ModuleLoadResult.buildFrom[(String, Boolean), Boolean](x =>
      Try(x.toBoolean)
        .fold(
          t => new ModuleLoadFailed(Set(x), Set.empty, Set.empty),
          b => new ModuleLoadSuccess(Set.empty, Set(x), Set.empty, (x, b))
        )
    ) { case (_, b) => b }

    extractor(Set("foop", "true")).value shouldBe true
  }
}
