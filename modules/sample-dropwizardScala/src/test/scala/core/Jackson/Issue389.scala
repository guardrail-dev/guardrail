package core.Jackson

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import issues.issue389.server.dropwizardScala.definitions.{ Bar, Foo }
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Issue389 extends AnyFreeSpec with Matchers {
  private val mapper = new ObjectMapper().registerModule(DefaultScalaModule)

  "x-jvm-type in Array should use the correct type in the array" in {
    val foo = Foo(
      customArray = Some(
        (1L until 5)
          .map(i => Bar(Some(i)))
          .toVector
      )
    )
    val expected = """{"customArray":[{"a":1},{"a":2},{"a":3},{"a":4}],"customMap":null}"""
    assertResult(expected)(mapper.writeValueAsString(foo))
  }

  "x-jvm-type in Map should use the correct type in the map" in {
    val foo = Foo(
      customMap = Some(
        (1 until 5)
          .map(x => x.toString -> Bar(a = Some(x)))
          .toMap
      )
    )
    val expected = """{"customArray":null,"customMap":{"1":{"a":1},"2":{"a":2},"3":{"a":3},"4":{"a":4}}}"""
    assertResult(expected)(mapper.writeValueAsString(foo))
  }
}
