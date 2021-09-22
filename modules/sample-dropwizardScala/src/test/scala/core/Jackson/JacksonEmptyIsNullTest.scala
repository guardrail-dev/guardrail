package core.Jackson

import com.fasterxml.jackson.databind.{ JsonMappingException, ObjectMapper }
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import emptyIsNull.server.dropwizardScala.definitions.EmptyIsNullProperties
import org.scalatest.TryValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.util.Try

class JacksonEmptyIsNullTest extends AnyFreeSpec with Matchers with TryValues {
  private val mapper = new ObjectMapper().registerModule(DefaultScalaModule)

  private val okAllPresent =
    """{
      |  "empty_allowed": "",
      |  "empty_not_allowed": "hi",
      |  "empty_allowed_opt": "",
      |  "empty_not_allowed_opt": "hi"
      |}""".stripMargin

  private val okOptionalEmpty =
    """{
      |  "empty_allowed": "",
      |  "empty_not_allowed": "hi",
      |  "empty_allowed_opt": "",
      |  "empty_not_allowed_opt": ""
      |}""".stripMargin

  private val badRequiredEmpty =
    """{
      |  "empty_allowed": "",
      |  "empty_not_allowed": "",
      |  "empty_allowed_opt": "",
      |  "empty_not_allowed_opt": "hi"
      |}""".stripMargin

  "When deserializing an object with x-empty-is null" - {
    "a valid JSON object is deserialized successfully" in {
      val result = mapper.readValue(okAllPresent, classOf[EmptyIsNullProperties])
      result.emptyAllowed mustBe ""
      result.emptyNotAllowed mustBe "hi"
      result.emptyAllowedOpt mustBe Some("")
      result.emptyNotAllowedOpt mustBe Some("hi")
    }

    "a JSON object with an empty optional property deserializes with an empty optional" in {
      val result = mapper.readValue(okOptionalEmpty, classOf[EmptyIsNullProperties])
      result.emptyAllowed mustBe ""
      result.emptyNotAllowed mustBe "hi"
      result.emptyAllowedOpt mustBe Some("")
      result.emptyNotAllowedOpt mustBe None
    }

    "a JSON object with an empty required property fails to deserialize" in {
      val result = Try(mapper.readValue(badRequiredEmpty, classOf[EmptyIsNullProperties]))
      result.isFailure mustBe true
      result.failure.exception.getClass mustBe classOf[JsonMappingException]
      result.failure.exception.getMessage must startWith("Value cannot be null")
    }
  }
}
