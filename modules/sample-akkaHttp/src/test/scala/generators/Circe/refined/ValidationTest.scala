package generators.Circe.refined

import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Interval
import io.circe.parser.parse
import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import validation.client.akkaHttp.definitions.{ Validated, Validated2, ValidatedCollections }
import eu.timepit.refined.collection._
import eu.timepit.refined._

class ValidationTest extends AnyFreeSpec with Matchers with EitherValues {

  "collections validation" - {
    "should fail when the max collection size limit is violated" in {
      ValidatedCollections.decodeValidatedCollections(parse("""{ "bounded_size_array" : [0,1,2,3,4,5,6,7,8,9,0,1] }""").value.hcursor).isLeft shouldBe true
    }

    "should fail when the min collection size limit is violated" in {
      ValidatedCollections.decodeValidatedCollections(parse("""{ "bounded_size_array" : [] }""").value.hcursor).isLeft shouldBe true
    }

    "should work within the array boundaries" in {
      type CollectionLimit = Size[Interval.Closed[shapeless.Witness.`1`.T, shapeless.Witness.`10`.T]]
      val array = refineV[CollectionLimit](Vector(1, 2, 3)).right.value
      ValidatedCollections.decodeValidatedCollections(parse("""{ "bounded_size_array" : [1,2,3] }""").value.hcursor).value shouldBe ValidatedCollections(
        Option(array)
      )
    }

  }

  "regex validation" - {

    "partial match" in {
      Validated2.decodeValidated2(parse("""{ "pattern_validation_partial": "carpet" }""").value.hcursor).value shouldBe Validated2(Some("carpet"), None)
    }

    "exact match" in {
      Validated2.decodeValidated2(parse("""{ "pattern_validation_exact": "carpet" }""").value.hcursor).isLeft shouldBe true

      Validated2.decodeValidated2(parse("""{ "pattern_validation_exact": "pet" }""").value.hcursor).value shouldBe Validated2(None, Some("pet"))
    }

    "should succeed on correct input" in {
      Validated.decodeValidated(parse("""{ "pattern_validation": "123" }""").value.hcursor).value shouldBe Validated(None, None, None, Some("123"))
    }

    "should succeed on a partially matching input" in {
      Validated.decodeValidated(parse("""{ "pattern_validation": "123notanumber" }""").value.hcursor).value shouldBe Validated(
        None,
        None,
        None,
        Some("123notanumber")
      )
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
