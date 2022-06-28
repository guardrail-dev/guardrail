package generators.Circe.refined

import eu.timepit.refined.auto._
import io.circe.parser.parse
import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import validation.client.akkaHttp.definitions.Validated

class ValidationTest extends AnyFreeSpec with Matchers with EitherValues {

  "regex validation" - {
    "should succeed on correct input" in {
      Validated.decodeValidated(parse("""{ "pattern_validation": "123" }""").right.get.hcursor).right.value shouldBe Validated(None, None, None, Some("123"))
    }

    "should fail on non matching input" in {
      Validated.decodeValidated(parse("""{ "pattern_validation": "123notanumber" }""").right.get.hcursor).isLeft shouldBe true
    }

  }

  "maximum validation" - {
    "should be inclusive" in {
      Validated.decodeValidated(parse("""{ "max_validation": 100 }""").value.hcursor).value shouldBe Validated(Some(100), None, None)
    }
    "should succeed" in {
      Validated.decodeValidated(parse("""{ "max_validation": 10 }""").value.hcursor).value shouldBe Validated(Some(10), None, None)
    }
    "should fail on incorrect input" in {
      Validated.decodeValidated(parse("""{ "max_validation": 101 }""").value.hcursor).isLeft shouldBe true
    }
  }

  "minimum validation" - {
    "should be inclusive" in {
      Validated.decodeValidated(parse("""{ "min_validation": 1 }""").value.hcursor).value shouldBe Validated(None, Some(1), None)
    }
    "should succeed" in {
      Validated.decodeValidated(parse("""{ "min_validation": 10 }""").value.hcursor).value shouldBe Validated(None, Some(10), None)
    }
    "should fail on incorrect input" in {
      Validated.decodeValidated(parse("""{ "min_validation": 0 }""").value.hcursor).isLeft shouldBe true
    }
  }

  "range validation" - {
    "should succeed within the range" in {
      Validated.decodeValidated(parse("""{ "range_validation": 10 }""").value.hcursor).value shouldBe Validated(None, None, Some(10))
    }

    "should be inclusive - higher bound" in {
      Validated.decodeValidated(parse("""{ "range_validation": 100 }""").value.hcursor).value shouldBe Validated(None, None, Some(100))
    }

    "should be inclusive - lower bound" in {
      Validated.decodeValidated(parse("""{ "range_validation": 0 }""").value.hcursor).value shouldBe Validated(None, None, Some(0))
    }

    "should fail on incorrect input - lower bound" in {
      Validated.decodeValidated(parse("""{ "range_validation": -1 }""").value.hcursor).isLeft shouldBe true
    }

    "should fail on incorrect input - higher bound" in {
      Validated.decodeValidated(parse("""{ "range_validation": 101 }""").value.hcursor).isLeft shouldBe true
    }

  }

}
