package core.Jackson

import java.util.Optional

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.exc.ValueInstantiationException
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module
import emptyIsNull.client.dropwizard.definitions.EmptyIsNullProperties
import org.scalatest.TryValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.util.Try

class JacksonEmptyIsNullTest extends AnyFreeSpec with Matchers with TryValues {
  private val mapper = new ObjectMapper().registerModule(new Jdk8Module)

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
      result.getEmptyAllowed mustBe ""
      result.getEmptyNotAllowed mustBe "hi"
      result.getEmptyAllowedOpt mustBe Optional.of("")
      result.getEmptyNotAllowedOpt mustBe Optional.of("hi")
    }

    "a JSON object with an empty optional property deserializes with an empty optional" in {
      val result = mapper.readValue(okOptionalEmpty, classOf[EmptyIsNullProperties])
      result.getEmptyAllowed mustBe ""
      result.getEmptyNotAllowed mustBe "hi"
      result.getEmptyAllowedOpt mustBe Optional.of("")
      result.getEmptyNotAllowedOpt mustBe Optional.empty()
    }

    "a JSON object with an empty required property fails to deserialize" in {
      val result = Try(mapper.readValue(badRequiredEmpty, classOf[EmptyIsNullProperties]))
      result.isFailure mustBe true
      result.failure.exception.getClass mustBe classOf[ValueInstantiationException]
      result.failure.exception.getCause.getClass mustBe classOf[NullPointerException]
    }
  }
}
