package core.Jackson

import additionalProperties.server.dropwizardScala.definitions.{ Foo, FooMapValues }
import java.util
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Issue263 extends AnyFreeSpec with Matchers {
  "additionalProperties with a $ref should emit a Map" in {
    val foo = new Foo(Map("yeah" -> FooMapValues("no", 42)))
    assert(classOf[Map[String, FooMapValues]].isAssignableFrom(foo.stuff.getClass))
  }
}
