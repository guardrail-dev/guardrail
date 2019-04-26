package issues

import issues.issue247.client.dropwizard.definitions.Issue247Foo
import org.scalatest.{FreeSpec, Matchers}

class Issue247 extends FreeSpec with Matchers {
  "Optional list properties in POJOs should not be wrapped in Optional" in {
    val field = classOf[Issue247Foo].getDeclaredField("optionalList")
    field shouldNot be(null)
    field.getType shouldBe classOf[java.util.List[_]]
  }
}
