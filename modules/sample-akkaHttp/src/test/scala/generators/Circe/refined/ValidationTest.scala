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
import eu.timepit.refined.api.Refined
import shapeless.Witness

class ValidationTest extends AnyFreeSpec with Matchers with EitherValues {

  "collection items validation" - {

    "should fail if collection elements rejected by the predicate" in {
      ValidatedCollections
        .decodeValidatedCollections(
          parse("""{ "collection_element_validation" : [0,2,3,4,5,6] }""").value.hcursor
        )
        .isLeft shouldBe true
    }

    "should succeed if all of the collection elements passed the predicate" in {
      type VectorWithPositiveNumbers = Vector[Int Refined numeric.GreaterEqual[Witness.`1`.T]]
      val vector: VectorWithPositiveNumbers = Vector(1, 2, 3, 4, 5, 6)

      val array = refineV[Size[numeric.GreaterEqual[Witness.`5`.T]]](vector).right.get

      ValidatedCollections
        .decodeValidatedCollections(
          parse("""{ "collection_element_validation" : [1,2,3,4,5,6] }""").value.hcursor
        )
        .value shouldBe ValidatedCollections(collectionElementValidation = Some(array))
    }

  }

  "collection size validation" - {
    "max size" - {
      "should fail when the min collection size limit is violated" in {
        ValidatedCollections.decodeValidatedCollections(parse("""{ "max_size_array" : [1,2,3,4,5,6] }""").value.hcursor).isLeft shouldBe true
      }
    }

    "min size" - {
      "should fail when the min collection size limit is violated" in {
        ValidatedCollections.decodeValidatedCollections(parse("""{ "min_size_array" : [9] }""").value.hcursor).isLeft shouldBe true
      }
//      "should work within the array boundaries" in {
//        type VectorWithPositiveNumbers = Vector[Int Refined numeric.GreaterEqual[shapeless.Witness.`1`.T]]
//        val vector: VectorWithPositiveNumbers = Vector(1, 2, 3, 4, 5, 6)
//
//        type CollectionLimit = Size[_root_.eu.timepit.refined.numeric.GreaterEqual[_root_.shapeless.Witness.`5`.T]]
//        val array = refineV[CollectionLimit](vector).right.value
//        ValidatedCollections.decodeValidatedCollections(parse("""{ "min_size_array" : [1,2,3,4,5,6] }""").value.hcursor).value shouldBe ValidatedCollections(
//          None, None, Option(array)
//        )
//      }
    }
    "range" - {
      "should fail when the max collection size limit is violated" in {
        ValidatedCollections.decodeValidatedCollections(parse("""{ "bounded_size_array" : [0,1,2,3,4,5,6,7,8,9,0,1] }""").value.hcursor).isLeft shouldBe true
      }

      "should fail when the min collection size limit is violated" in {
        ValidatedCollections.decodeValidatedCollections(parse("""{ "bounded_size_array" : [] }""").value.hcursor).isLeft shouldBe true
      }

      "should work within the array boundaries" in {
        val array     = refineV[Size[Interval.Closed[Witness.`1`.T, Witness.`10`.T]]](Vector(1, 2, 3))
        val validated = array.right.get
        ValidatedCollections.decodeValidatedCollections(parse("""{ "bounded_size_array" : [1,2,3] }""").value.hcursor).value shouldBe ValidatedCollections(
          Option(validated)
        )
      }
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
