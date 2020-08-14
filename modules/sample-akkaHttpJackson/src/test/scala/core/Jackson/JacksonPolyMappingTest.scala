package core.Jackson

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import polymorphismMapped.client.akkaHttpJackson.definitions.{ A, B, Base, C, DiscrimEnum, EnumA, EnumB, EnumBase, EnumC }
import scala.reflect.ClassTag

class JacksonPolyMappingTest extends AnyFreeSpec with Matchers {
  private val mapper = new ObjectMapper()
    .registerModule(DefaultScalaModule)

  "Polymorphic definitions with discriminator mappings" - {
    "should have their discriminator initialized properly" in {
      val a = A(42)
      a.polytype shouldBe "this_is_a"

      val b = B("foo")
      b.polytype shouldBe "this_is_b"

      val c = C(42.42)
      c.polytype shouldBe "C"
    }

    "should deserialize properly" in {
      def verify[T](json: String, discriminatorValue: String)(implicit cls: ClassTag[T]): Unit = {
        val pojo = mapper.readValue(json, classOf[Base])
        pojo shouldNot be(null)
        pojo.getClass shouldBe cls.runtimeClass
        pojo.polytype shouldBe discriminatorValue
      }

      verify[A]("""{"polytype": "this_is_a", "some_a": 42}""", "this_is_a")
      verify[B]("""{"polytype": "this_is_b", "some_b": "foo"}""", "this_is_b")
      verify[C]("""{"polytype": "C", "some_c": 42.42}""", "C")
    }
  }

  "Polymorphic definitions with enum discriminator mappings" - {
    "should have their discriminator initialized properly" in {
      val a = EnumA(42)
      a.polytype shouldBe DiscrimEnum.SomeValueOne

      val b = EnumB("foo")
      b.polytype shouldBe DiscrimEnum.AnotherValue

      val c = EnumC(42.42)
      c.polytype shouldBe DiscrimEnum.YetAnotherValue
    }

    "should deserialize properly" in {
      def verify[T](json: String, discriminatorValue: DiscrimEnum)(implicit cls: ClassTag[T]): Unit = {
        val pojo = mapper.readValue(json, classOf[EnumBase])
        pojo shouldNot be(null)
        pojo.getClass shouldBe cls.runtimeClass
        pojo.polytype shouldBe discriminatorValue
      }

      verify[EnumA]("""{"polytype": "some-value-one", "some_a": 42}""", DiscrimEnum.SomeValueOne)
      verify[EnumB]("""{"polytype": "another-value", "some_b": "foo"}""", DiscrimEnum.AnotherValue)
      verify[EnumC]("""{"polytype": "yet-another-value", "some_c": 42.42}""", DiscrimEnum.YetAnotherValue)
    }
  }
}
