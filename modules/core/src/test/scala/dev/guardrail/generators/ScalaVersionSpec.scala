package dev.guardrail.generators

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ScalaVersionSpec extends AnyFunSuite with Matchers {

  test("ScalaVersion.fromString should parse 2.12") {
    ScalaVersion.fromString("2.12") shouldBe Right(ScalaVersion.Scala212)
  }

  test("ScalaVersion.fromString should parse 2.12.x") {
    ScalaVersion.fromString("2.12.x") shouldBe Right(ScalaVersion.Scala212)
  }

  test("ScalaVersion.fromString should parse 2.13") {
    ScalaVersion.fromString("2.13") shouldBe Right(ScalaVersion.Scala213)
  }

  test("ScalaVersion.fromString should parse 2.13.x") {
    ScalaVersion.fromString("2.13.x") shouldBe Right(ScalaVersion.Scala213)
  }

  test("ScalaVersion.fromString should parse 3") {
    ScalaVersion.fromString("3") shouldBe Right(ScalaVersion.Scala3)
  }

  test("ScalaVersion.fromString should parse 3.x") {
    ScalaVersion.fromString("3.x") shouldBe Right(ScalaVersion.Scala3)
  }

  test("ScalaVersion.fromString should parse 3.3.4") {
    ScalaVersion.fromString("3.3.4") shouldBe Right(ScalaVersion.Scala3)
  }

  test("ScalaVersion.fromString should parse 3.4.0") {
    ScalaVersion.fromString("3.4.0") shouldBe Right(ScalaVersion.Scala3)
  }

  test("ScalaVersion.fromString should handle whitespace") {
    ScalaVersion.fromString("  3  ") shouldBe Right(ScalaVersion.Scala3)
    ScalaVersion.fromString("  2.13  ") shouldBe Right(ScalaVersion.Scala213)
  }

  test("ScalaVersion.fromString should be case insensitive") {
    ScalaVersion.fromString("3.X") shouldBe Right(ScalaVersion.Scala3)
    ScalaVersion.fromString("2.13.X") shouldBe Right(ScalaVersion.Scala213)
  }

  test("ScalaVersion.fromString should return error for unsupported versions") {
    ScalaVersion.fromString("2.11") shouldBe a[Left[_, _]]
    ScalaVersion.fromString("invalid") shouldBe a[Left[_, _]]
    ScalaVersion.fromString("") shouldBe a[Left[_, _]]
  }

  test("ScalaVersion.fromString error message should be informative") {
    val result = ScalaVersion.fromString("2.11")
    result match {
      case Left(msg) =>
        msg should include("Unsupported Scala version")
        msg should include("2.11")
      case Right(_) => fail("Expected Left")
    }
  }

  test("ScalaVersion.default should be Scala213") {
    ScalaVersion.default shouldBe ScalaVersion.Scala213
  }

  test("isScala3 should return true for Scala3") {
    ScalaVersion.Scala3.isScala3 shouldBe true
    ScalaVersion.Scala213.isScala3 shouldBe false
    ScalaVersion.Scala212.isScala3 shouldBe false
  }

  test("isScala2 should return true for Scala 2 versions") {
    ScalaVersion.Scala3.isScala2 shouldBe false
    ScalaVersion.Scala213.isScala2 shouldBe true
    ScalaVersion.Scala212.isScala2 shouldBe true
  }

  test("ScalaVersion.value should return correct string") {
    ScalaVersion.Scala212.value shouldBe "2.12"
    ScalaVersion.Scala213.value shouldBe "2.13"
    ScalaVersion.Scala3.value shouldBe "3"
  }
}
