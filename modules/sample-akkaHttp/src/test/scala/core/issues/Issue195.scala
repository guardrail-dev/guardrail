package core.issues

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import io.circe._, io.circe.syntax._
import issues.issue195.server.akkaHttp.definitions._

class Issue195 extends AnyFunSpec with Matchers {
  describe("oneOf mapping") {
    def emit[A](label: String, func: () => A, expected: Json)(implicit ev: A => Foo): Unit =
      describe(label) {
        it("should encode") {
          Foo(func()).asJson shouldBe expected
        }

        it("should decode") {
          expected.as[Foo] shouldBe Right(Foo(func()))
        }

        it("should round-trip") {
          Foo(func()).asJson.as[Foo] shouldBe Right(Foo(func()))
        }
      }
    emit("TypeA", () => TypeA("foo"), Json.obj("type" -> Json.fromString("foo"), "beepBorp" -> Json.fromString("typea")))
    emit("TypeB", () => TypeB("foo"), Json.obj("type" -> Json.fromString("foo"), "beepBorp" -> Json.fromString("TypeB")))
    emit("TypeC", () => Foo.WrappedC(TypeC("foo")), Json.obj("C" -> Json.obj("type" -> Json.fromString("foo")), "beepBorp" -> Json.fromString("WrappedC")))
    emit("typed", () => Foo.Nested2(Typed("foo")), Json.obj("D" -> Json.obj("type" -> Json.fromString("foo")), "beepBorp" -> Json.fromString("Nested2")))
    emit("TypeE", () => Foo.Nested3(TypeE("foo")), Json.obj("E" -> Json.obj("type" -> Json.fromString("foo")), "beepBorp" -> Json.fromString("Nested3")))
  }

  describe("anonymous oneOf mapping") {
    def emit[A](label: String, func: () => A, expected: Json)(implicit ev: A => X.Links): Unit =
      describe(label) {
        it("should encode") {
          X(func()).asJson shouldBe expected
        }

        it("should decode") {
          expected.as[X] shouldBe Right(X(func()))
        }

        it("should round-trip") {
          X(func()).asJson.as[X] shouldBe Right(X(func()))
        }
      }

    emit("first branch, Vector[String]", () => X.Links(Vector("1", "2")), Json.obj("links" -> Json.arr(Json.fromString("1"), Json.fromString("2"))))
    emit("second branch, object", () => X.Links(Json.obj("a" -> Json.fromString("hey"))), Json.obj("links" -> Json.obj("a" -> Json.fromString("hey"))))
  }
}
