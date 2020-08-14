package core.Jackson

import com.fasterxml.jackson.databind.JsonMappingException
import core.TestImplicits
import issues.issue315.legacylegacy.client.akkaHttpJackson.definitions.TestObject
import issues.issue315.legacylegacy.client.akkaHttpJackson.support.Presence
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class PresenceTest extends AnyFreeSpec with TestImplicits with Matchers {
  "Required nullable serialization should work" in {
    val present = """{"required":"","required-nullable":"i am present","legacy":null}"""
    val nulled  = """{"required":"","required-nullable":null,"legacy":null}"""

    mapper.writeValueAsString(TestObject("", requiredNullable = Some("i am present"), Presence.absent, Presence.absent, None)) mustBe present
    mapper.writeValueAsString(TestObject("", requiredNullable = None, Presence.absent, Presence.absent, None)) mustBe nulled
  }

  "Optional non-nullable serialization should work" in {
    val present = """{"required":"","required-nullable":null,"optional":"i am present","legacy":null}"""
    val absent  = """{"required":"","required-nullable":null,"legacy":null}"""

    mapper.writeValueAsString(TestObject("", None, optional = Presence.present("i am present"), Presence.absent, None)) mustBe present
    mapper.writeValueAsString(TestObject("", None, optional = Presence.absent, Presence.absent, None)) mustBe absent
  }

  "Optional nullable serialization should work" in {
    val present = """{"required":"","required-nullable":null,"optional-nullable":"i am present","legacy":null}"""
    val nulled  = """{"required":"","required-nullable":null,"optional-nullable":null,"legacy":null}"""
    val absent  = """{"required":"","required-nullable":null,"legacy":null}"""

    mapper.writeValueAsString(TestObject("", None, Presence.absent, optionalNullable = Presence.present(Option("i am present")), None)) mustBe present
    mapper.writeValueAsString(TestObject("", None, Presence.absent, optionalNullable = Presence.present(None), None)) mustBe nulled
    mapper.writeValueAsString(TestObject("", None, Presence.absent, optionalNullable = Presence.absent, None)) mustBe absent
  }

  "Optional legacy serialization should work" in {
    val present = """{"required":"","required-nullable":null,"legacy":"i am present"}"""
    val nulled  = """{"required":"","required-nullable":null,"legacy":null}"""

    mapper.writeValueAsString(TestObject("", None, Presence.absent, Presence.absent, legacy = Option("i am present"))) mustBe present
    mapper.writeValueAsString(TestObject("", None, Presence.absent, Presence.absent, legacy = None)) mustBe nulled
  }

  "Required nullable deserialization should work" in {
    val present = TestObject("", requiredNullable = Option("i am present"), Presence.absent, Presence.absent, None)
    val nulled  = present.copy(requiredNullable = None)

    mapper.readValue("""{"required":"","required-nullable":"i am present"}""", classOf[TestObject]) mustBe present
    mapper.readValue("""{"required":"","required-nullable":null}""", classOf[TestObject]) mustBe nulled
    intercept[JsonMappingException] { mapper.readValue("""{"required":""}""", classOf[TestObject]) }
  }

  "Optional non-nullable deserialization should work" in {
    val present = TestObject("", None, optional = Presence.present("i am present"), Presence.absent, None)
    val absent  = present.copy(optional = Presence.absent)

    mapper.readValue("""{"required":"","required-nullable":null,"optional":"i am present"}""", classOf[TestObject]) mustBe present
    intercept[JsonMappingException] { mapper.readValue("""{"required":"","required-nullable":null,"optional":null}""", classOf[TestObject]) }
    mapper.readValue("""{"required":"","required-nullable":null}""", classOf[TestObject]) mustBe absent
  }

  "Optional nullable deserialization should work" in {
    val present = TestObject("", None, Presence.absent, optionalNullable = Presence.present(Option("i am present")), None)
    val nulled  = present.copy(optionalNullable = Presence.present(None))
    val absent  = present.copy(optionalNullable = Presence.absent)

    mapper.readValue("""{"required":"","required-nullable":null,"optional-nullable":"i am present"}""", classOf[TestObject]) mustBe present
    mapper.readValue("""{"required":"","required-nullable":null,"optional-nullable":null}""", classOf[TestObject]) mustBe nulled
    mapper.readValue("""{"required":"","required-nullable":null}""", classOf[TestObject]) mustBe absent
  }

  "Optional legacy deserialization should work" in {
    val present        = TestObject("", None, Presence.absent, Presence.absent, Some("i am present"))
    val nulledOrAbsent = present.copy(legacy = None)

    mapper.readValue("""{"required":"","required-nullable":null,"legacy": "i am present"}""", classOf[TestObject]) mustBe present
    mapper.readValue("""{"required":"","required-nullable":null,"legacy": null}""", classOf[TestObject]) mustBe nulledOrAbsent
    mapper.readValue("""{"required":"","required-nullable":null}""", classOf[TestObject]) mustBe nulledOrAbsent
  }
}
