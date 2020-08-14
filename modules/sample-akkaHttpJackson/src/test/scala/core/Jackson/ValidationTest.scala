package core.Jackson

import core.TestImplicits
import examples.server.akkaHttpJackson.JacksonImplicits.GuardrailValidator
import examples.server.akkaHttpJackson.definitions.User
import org.scalatest.TryValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.util.Success

class ValidationTest extends AnyFreeSpec with Matchers with TryValues with TestImplicits {
  "GuardrailValidator" - {
    "should validate successfully" in {
      val user = User(id = Some(42), username = Some("root"))
      implicitly[GuardrailValidator[User]].validate(user) mustBe Success(user)
    }

    "validates successfully for containers" in {
      val user = User(id = Some(42), username = Some("root"))
      implicitly[GuardrailValidator[Option[User]]].validate(Option(user)) mustBe Success(Some(user))
      implicitly[GuardrailValidator[Vector[User]]].validate(Vector(user)) mustBe Success(Vector(user))
      implicitly[GuardrailValidator[Map[String, User]]].validate(Map("root" -> user)) mustBe Success(Map("root" -> user))
    }

    "passes through primitive types" in {
      implicitly[GuardrailValidator[Int]].validate(42) mustBe Success(42)
    }

    "fails on bad data" in {
      val user = User(id = null)
      implicitly[GuardrailValidator[User]].validate(user).failure.exception must have message "Validation of User failed: id: may not be null"
    }

    "fails on bad data in containers" in {
      val user = User(id = null)
      implicitly[GuardrailValidator[Option[User]]].validate(Option(user)).failure.exception must have message "Validation of User failed: id: may not be null"
      implicitly[GuardrailValidator[Vector[User]]].validate(Vector(user)).failure.exception must have message "Validation of User failed: id: may not be null"
      implicitly[GuardrailValidator[Map[String, User]]]
        .validate(Map("root" -> user))
        .failure
        .exception must have message "Validation of User failed: id: may not be null"
    }
  }
}
