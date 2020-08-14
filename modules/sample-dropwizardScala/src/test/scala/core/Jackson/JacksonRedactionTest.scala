package core.Jackson

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import redaction.server.dropwizardScala.definitions.Redaction

class JacksonRedactionTest extends AnyFreeSpec with Matchers {
  "Redacted fields should get replaced with '[redacted]'" in {
    val redaction = Redaction("a", "b", visibleOptional = Some("c"), redactedOptional = Some("d"))
    redaction.toString shouldBe "Redaction(a,[redacted],Some(c),[redacted])"
  }
}
