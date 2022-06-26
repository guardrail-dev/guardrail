package generators.Circe

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import redaction.client.akkaHttp.definitions.Redaction

class CirceRedactionTest extends AnyFreeSpec with Matchers {

  "Redacted fields should get replaced with '[redacted]'" in {
    val redaction = Redaction("a", "b", Some("c"), Some("d"))
    redaction.toString shouldBe "Redaction(a,[redacted],Some(c),[redacted])"
  }
}
