package core.Jackson

import com.fasterxml.jackson.databind.ObjectMapper
import issues.issue389.client.dropwizard.definitions.{ Bar, Foo }
import java.util
import org.scalatest.{ FreeSpec, Matchers }
import scala.collection.JavaConverters._
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module

class Issue389 extends FreeSpec with Matchers {
  "x-jvm-type in Array should use the correct type in the array" in {
    val mapper = new ObjectMapper().registerModule(new Jdk8Module())
    val foo = new Foo.Builder()
      .withCustomArray(
        (1 until 5)
          .map(new Bar.Builder().withA(_).build())
          .toList
          .asJava
      ).build()
    val expected = """{"customArray":[{"a":1},{"a":2},{"a":3},{"a":4}],"customMap":null}"""
    assertResult(expected)(mapper.writeValueAsString(foo))
  }

  "x-jvm-type in Map should use the correct type in the map" in {
    val mapper = new ObjectMapper().registerModule(new Jdk8Module())
    val foo = new Foo.Builder()
      .withCustomMap(
        (1 until 5)
          .map(x => x.toString() -> new Bar.Builder().withA(x).build())
          .toMap
          .asJava
      ).build()
    val expected = """{"customArray":null,"customMap":{"1":{"a":1},"2":{"a":2},"3":{"a":3},"4":{"a":4}}}"""
    assertResult(expected)(mapper.writeValueAsString(foo))
  }
}
