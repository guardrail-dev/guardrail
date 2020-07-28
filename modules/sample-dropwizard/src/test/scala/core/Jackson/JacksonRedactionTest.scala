package core.Jackson

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import redaction.client.dropwizard.definitions.Redaction

class JacksonRedactionTest extends AnyFreeSpec with Matchers {
  "Redacted fields should get replaced with '[redacted]'" in {
    val redaction = new Redaction.Builder("a", "b")
      .withVisibleOptional("c")
      .withRedactedOptional("d")
      .build()

    redaction.toString shouldBe "Redaction{visibleRequired=a, redactedRequired=[redacted], visibleOptional=Optional[c], redactedOptional=[redacted]}"
  }
}
