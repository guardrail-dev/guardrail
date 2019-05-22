package core.Jackson

import com.fasterxml.jackson.databind.ObjectMapper
import org.scalatest.{FreeSpec, Matchers}
import polymorphismMapped.client.dropwizard.definitions.{A, B, Base, C}
import scala.reflect.ClassTag

class JacksonPolyMappingTest extends FreeSpec with Matchers {
  private val mapper = new ObjectMapper

  "Polymorphic definitions with discriminator mappings" - {
    "should have their discriminator initialized properly" in {
      val a = new A.Builder(42).build()
      a.getPolytype shouldBe "this_is_a"

      val b = new B.Builder("foo").build()
      b.getPolytype shouldBe "this_is_b"

      val c = new C.Builder(42.42).build()
      c.getPolytype shouldBe "C"
    }

    "should deserialize properly" in {
      def verify[T](json: String, discriminatorValue: String)(implicit cls: ClassTag[T]): Unit = {
        val pojo = mapper.readValue(json, classOf[Base])
        pojo shouldNot be(null)
        pojo.getClass shouldBe cls.runtimeClass
        pojo.getPolytype shouldBe discriminatorValue
      }

      verify[A]("""{"polytype": "this_is_a", "some_a": 42}""", "this_is_a")
      verify[B]("""{"polytype": "this_is_b", "some_b": "foo"}""", "this_is_b")
      verify[C]("""{"polytype": "C", "some_c": 42.42}""", "C")
    }
  }
}
