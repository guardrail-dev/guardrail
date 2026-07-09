package dev.guardrail.generators.scala

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.meta._

import dev.guardrail.generators.ScalaVersion

class Scala3CompatSpec extends AnyFunSuite with Matchers {

  test("implicitsClause should generate implicit modifier for Scala 2.12") {
    val params = List(param"ec: scala.concurrent.ExecutionContext")
    val result = Scala3Compat.implicitsClause(params, ScalaVersion.Scala212)

    result.mod.map(_.structure) shouldBe Some(Mod.Implicit().structure)
    result.values.map(_.structure) shouldBe params.map(_.structure)
  }

  test("implicitsClause should generate implicit modifier for Scala 2.13") {
    val params = List(param"ec: scala.concurrent.ExecutionContext")
    val result = Scala3Compat.implicitsClause(params, ScalaVersion.Scala213)

    result.mod.map(_.structure) shouldBe Some(Mod.Implicit().structure)
    result.values.map(_.structure) shouldBe params.map(_.structure)
  }

  test("implicitsClause should generate using modifier for Scala 3") {
    val params = List(param"ec: scala.concurrent.ExecutionContext")
    val result = Scala3Compat.implicitsClause(params, ScalaVersion.Scala3)

    result.mod.map(_.structure) shouldBe Some(Mod.Using().structure)
    result.values.map(_.structure) shouldBe params.map(_.structure)
  }

  test("implicitsClause should handle multiple parameters") {
    val params = List(
      param"ec: scala.concurrent.ExecutionContext",
      param"mat: akka.stream.Materializer"
    )

    val scala2Result = Scala3Compat.implicitsClause(params, ScalaVersion.Scala213)
    scala2Result.mod.map(_.structure) shouldBe Some(Mod.Implicit().structure)
    scala2Result.values.size shouldBe 2

    val scala3Result = Scala3Compat.implicitsClause(params, ScalaVersion.Scala3)
    scala3Result.mod.map(_.structure) shouldBe Some(Mod.Using().structure)
    scala3Result.values.size shouldBe 2
  }

  test("implicitsClause should handle empty parameters") {
    val params = List.empty[Term.Param]

    val scala2Result = Scala3Compat.implicitsClause(params, ScalaVersion.Scala213)
    scala2Result.mod.map(_.structure) shouldBe Some(Mod.Implicit().structure)
    scala2Result.values shouldBe empty

    val scala3Result = Scala3Compat.implicitsClause(params, ScalaVersion.Scala3)
    scala3Result.mod.map(_.structure) shouldBe Some(Mod.Using().structure)
    scala3Result.values shouldBe empty
  }

  test("implicitsClause syntax output should be correct for Scala 2") {
    val params = List(param"ec: ExecutionContext")
    val result = Scala3Compat.implicitsClause(params, ScalaVersion.Scala213)

    // The syntax should contain "implicit"
    result.syntax should include("implicit")
    result.syntax should not include "using"
  }

  test("implicitsClause syntax output should be correct for Scala 3") {
    val params = List(param"ec: ExecutionContext")
    val result = Scala3Compat.implicitsClause(params, ScalaVersion.Scala3)

    // The syntax should contain "using"
    result.syntax should include("using")
    result.syntax should not include "implicit"
  }

  test("implicitVal should generate implicit val for Scala 2") {
    val name = q"encoder"
    val tpe  = t"Encoder[String]"
    val rhs  = q"Encoder.encodeString"

    val result = Scala3Compat.implicitVal(name, tpe, rhs, ScalaVersion.Scala213)

    result.syntax should include("implicit")
    result.syntax should include("val")
    result.syntax should include("encoder")
  }

  test("implicitVal should generate given for Scala 3") {
    val name = q"encoder"
    val tpe  = t"Encoder[String]"
    val rhs  = q"Encoder.encodeString"

    val result = Scala3Compat.implicitVal(name, tpe, rhs, ScalaVersion.Scala3)

    result.syntax should include("given")
    result.syntax should include("encoder")
    result.syntax should not include "implicit"
  }

  test("implicitDef should generate implicit def for Scala 2") {
    val name           = q"showEncoder"
    val typeParams     = List(tparam"A")
    val implicitParams = List(param"ev: Show[A]")
    val returnType     = t"Encoder[A]"
    val body           = q"Encoder.encodeString.contramap(ev.show)"

    val result = Scala3Compat.implicitDef(name, typeParams, implicitParams, returnType, body, ScalaVersion.Scala213)

    result.syntax should include("implicit")
    result.syntax should include("def")
    result.syntax should include("showEncoder")
  }

  test("implicitDef should generate given for Scala 3") {
    val name           = q"showEncoder"
    val typeParams     = List(tparam"A")
    val implicitParams = List(param"ev: Show[A]")
    val returnType     = t"Encoder[A]"
    val body           = q"Encoder.encodeString.contramap(ev.show)"

    val result = Scala3Compat.implicitDef(name, typeParams, implicitParams, returnType, body, ScalaVersion.Scala3)

    result.syntax should include("given")
    result.syntax should include("showEncoder")
    result.syntax should not include "implicit def"
  }

  test("implicitDef without type params should work for Scala 2") {
    val name           = q"defaultEncoder"
    val typeParams     = List.empty[Type.Param]
    val implicitParams = List.empty[Term.Param]
    val returnType     = t"Encoder[String]"
    val body           = q"Encoder.encodeString"

    val result = Scala3Compat.implicitDef(name, typeParams, implicitParams, returnType, body, ScalaVersion.Scala213)

    result.syntax should include("implicit")
    result.syntax should include("def")
    result.syntax should include("defaultEncoder")
  }

  test("implicitDef without type params should work for Scala 3") {
    val name           = q"defaultEncoder"
    val typeParams     = List.empty[Type.Param]
    val implicitParams = List.empty[Term.Param]
    val returnType     = t"Encoder[String]"
    val body           = q"Encoder.encodeString"

    val result = Scala3Compat.implicitDef(name, typeParams, implicitParams, returnType, body, ScalaVersion.Scala3)

    result.syntax should include("given")
    result.syntax should include("defaultEncoder")
  }
}
