package tests.core

import dev.guardrail.generators.syntax.RichString
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

object CaseConvertersTest {
  private case class CaseTest(raw: String, expectedPascal: String, expectedCamel: String, expectedSnake: String, expectedDashed: String) {
    override val toString: String = s"""(Test case for: "$raw")"""
  }

  private val TEST_CASES = List(
    CaseTest("foo", "Foo", "foo", "foo", "foo"),
    CaseTest("Foo", "Foo", "foo", "foo", "foo"),
    CaseTest("FOO", "Foo", "foo", "foo", "foo"),
    CaseTest("fooBar", "FooBar", "fooBar", "foo_bar", "foo-bar"),
    CaseTest("fooBarBaz", "FooBarBaz", "fooBarBaz", "foo_bar_baz", "foo-bar-baz"),
    CaseTest("foo-bar-baz", "FooBarBaz", "fooBarBaz", "foo_bar_baz", "foo-bar-baz"),
    CaseTest("foo.bar.baz", "FooBarBaz", "fooBarBaz", "foo_bar_baz", "foo-bar-baz"),
    CaseTest("FooBarBaz", "FooBarBaz", "fooBarBaz", "foo_bar_baz", "foo-bar-baz"),
    CaseTest("Foo-Bar-Baz", "FooBarBaz", "fooBarBaz", "foo_bar_baz", "foo-bar-baz"),
    CaseTest("Foo.Bar-Baz", "FooBarBaz", "fooBarBaz", "foo_bar_baz", "foo-bar-baz"),
    CaseTest("foo-Bar-Baz", "FooBarBaz", "fooBarBaz", "foo_bar_baz", "foo-bar-baz"),
    CaseTest("foo.Bar-Baz", "FooBarBaz", "fooBarBaz", "foo_bar_baz", "foo-bar-baz"),
    CaseTest("Foo-Bar-BazQuux", "FooBarBazQuux", "fooBarBazQuux", "foo_bar_baz_quux", "foo-bar-baz-quux"),
    CaseTest("Foo-Bar.BazQuux", "FooBarBazQuux", "fooBarBazQuux", "foo_bar_baz_quux", "foo-bar-baz-quux"),
    CaseTest("foo9bar", "Foo9bar", "foo9bar", "foo9bar", "foo9bar"),
    CaseTest("foo9Bar", "Foo9Bar", "foo9Bar", "foo9_bar", "foo9-bar"),
    CaseTest("9fooBar", "9fooBar", "9fooBar", "9foo_bar", "9foo-bar"),
    CaseTest("Foo-_Bar__ baz.", "FooBarBaz", "fooBarBaz", "foo_bar_baz", "foo-bar-baz"),
    CaseTest("FOO BAR BAZ", "FooBarBaz", "fooBarBaz", "foo_bar_baz", "foo-bar-baz"),
    CaseTest("FOO BAR bazQuux", "FooBarBazQuux", "fooBarBazQuux", "foo_bar_baz_quux", "foo-bar-baz-quux"),
    CaseTest("FOOBarBaz", "FooBarBaz", "fooBarBaz", "foo_bar_baz", "foo-bar-baz"),
    CaseTest("FooBARBaz", "FooBarBaz", "fooBarBaz", "foo_bar_baz", "foo-bar-baz"),
    CaseTest("UNITS_USD$3", "UnitsUsd$3", "unitsUsd$3", "units_usd$3", "units-usd$3")
  )
}

class CaseConvertersTest extends AnyFreeSpec with Matchers {
  import CaseConvertersTest._

  "Pascal case converter should work" in {
    TEST_CASES.foreach({ testCase =>
      withClue(testCase)(testCase.raw.toPascalCase shouldBe testCase.expectedPascal)
    })
  }

  "Camel case converter should work" in {
    TEST_CASES.foreach({ testCase =>
      withClue(testCase)(testCase.raw.toCamelCase shouldBe testCase.expectedCamel)
    })
  }

  "Snake case converter should work" in {
    TEST_CASES.foreach({ testCase =>
      withClue(testCase)(testCase.raw.toSnakeCase shouldBe testCase.expectedSnake)
    })
  }

  "Dashed case converter should work" in {
    TEST_CASES.foreach({ testCase =>
      withClue(testCase)(testCase.raw.toDashedCase shouldBe testCase.expectedDashed)
    })
  }
}
