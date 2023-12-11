package core.issues

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import io.circe._, io.circe.syntax._
import issues.issue195.server.akkaHttp.definitions._

class Issue195 extends AnyFunSpec with Matchers {
  describe("oneOf mapping") {
    it("should encode") {
      Foo(TypeA("foo")).asJson shouldBe Json.obj("type" -> Json.fromString("foo"), "beepBorp" -> Json.fromString("typea"))
    }

    it("should decode") {
      Json.obj("type" -> Json.fromString("foo"), "beepBorp" -> Json.fromString("typea")).as[Foo] shouldBe Right(Foo(TypeA("foo")))
    }

    it("should round-trip") {
      Foo(TypeA("foo")).asJson.as[Foo] shouldBe Right(Foo(TypeA("foo")))
    }
  }
}
