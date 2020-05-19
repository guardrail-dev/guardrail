package generators.circe

import org.scalatest.{ EitherValues, FunSuite, Matchers }
import issues.issue315.client.http4s.definitions._
import issues.issue315.client.http4s.support._
import io.circe._
import io.circe.syntax._
class NullableTest extends FunSuite with Matchers with EitherValues {
  val constant   = "constant"
  val defaultObj = TestObject(required = constant, optionalNullable = Presence.Absent)

  test("Nullability should be implemented correctly for required nullable") {
    val json = defaultObj.asJson
    getKey(json, "required-nullable") should equal(Some(Json.Null))
    json.as[TestObject].right.value should equal(defaultObj)

    val obj2  = defaultObj.copy(requiredNullable = Some(constant))
    val json2 = obj2.asJson
    getKey(json2, "required-nullable") should equal(Some(Json.fromString(constant)))
    json2.as[TestObject].right.value should equal(obj2)

    dropKey(json, "required-nullable").as[TestObject] should be('left)
  }

  test("Nullability should be implemented correctly for optional") {
    val json = defaultObj.asJson
    getKey(json, "optional") should equal(None)
    json.asObject.get.keys should not contain ("optional")
    json.as[TestObject].right.value should equal(defaultObj)

    val obj2  = defaultObj.copy(optional = Presence.Present(constant))
    val json2 = obj2.asJson
    getKey(json2, "optional") should equal(Some(Json.fromString(constant)))
    json2.as[TestObject].right.value should equal(obj2)

    val updated = json.asObject.get.add("optional", Json.Null)
    updated.asJson.as[TestObject] should be('left)
  }

  test("Nullability should be implemented correctly for optional legacy") {
    val json = defaultObj.asJson
    getKey(json, "legacy") should equal(Some(Json.Null))
    json.asObject.get.keys should contain("legacy")
    json.as[TestObject].right.value should equal(defaultObj)
    val updated = json.asObject.get.add("legacy", Json.Null)
    updated.asJson.as[TestObject].right.value should equal(defaultObj)

    val obj2  = defaultObj.copy(legacy = Some(constant))
    val json2 = obj2.asJson
    getKey(json2, "legacy") should equal(Some(Json.fromString(constant)))
    json2.as[TestObject].right.value should equal(obj2)
  }

  test("Nullability should be implemented correctly for optional nullable") {
    val json = defaultObj.asJson
    getKey(json, "optional-nullable") should equal(None)
    json.asObject.get.keys should not contain ("optional-nullable")
    json.as[TestObject].right.value should equal(defaultObj)

    val objPresent  = defaultObj.copy(optionalNullable = Presence.Present(None))
    val jsonPresent = objPresent.asJson
    getKey(jsonPresent, "optional-nullable") should equal(Some(Json.Null))
    jsonPresent.as[TestObject].right.value should equal(objPresent)

    val objValue  = defaultObj.copy(optionalNullable = Presence.Present(Some(constant)))
    val jsonValue = objValue.asJson
    getKey(jsonValue, "optional-nullable") should equal(Some(Json.fromString(constant)))
    jsonValue.as[TestObject].right.value should equal(objValue)
  }

  private def getKey(json: Json, key: String): Option[Json] = json.hcursor.downField(key).as[Json].toOption
  private def dropKey(json: Json, key: String): Json =
    json.mapObject(_.filterKeys(_ != key))
}
