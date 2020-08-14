package core.Jackson

import additionalProperties.client.dropwizard.definitions.{ Foo, FooMapValues }
import java.util
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Issue263 extends AnyFreeSpec with Matchers {
  "additionalProperties with a $ref should emit a Map" in {
    val stuff = new util.HashMap[String, FooMapValues]
    stuff.put("yeah", new FooMapValues.Builder("no", 42).build())
    val foo = new Foo.Builder().withStuff(stuff).build()
    assert(classOf[util.Map[String, FooMapValues]].isAssignableFrom(foo.getStuff.getClass))
  }
}
