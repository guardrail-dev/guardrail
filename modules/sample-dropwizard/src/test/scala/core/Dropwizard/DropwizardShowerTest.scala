package core.Dropwizard

import alias.client.dropwizard.Shower
import java.math
import java.net.{ URI, URL }
import java.time.{ LocalDate, OffsetDateTime }
import org.scalatest.{ FreeSpec, Matchers }
import scala.util.{ Failure, Success, Try }

class DropwizardShowerTest extends FreeSpec with Matchers {
  private val shower = Shower.getInstance

  "Showers should be able to show for each builtin type" - {
    "Boolean" in {
      assert(shower.canShow(classOf[java.lang.Boolean]))
      val b: java.lang.Boolean = java.lang.Boolean.TRUE
      shower.show(b) shouldBe "true"
    }

    "Byte" in {
      assert(shower.canShow(classOf[java.lang.Byte]))
      val b: java.lang.Byte = java.lang.Byte.valueOf("42")
      shower.show(b) shouldBe "42"
    }

    "Character" in {
      assert(shower.canShow(classOf[java.lang.Character]))
      val c: java.lang.Character = "*".toCharArray()(0)
      shower.show(c) shouldBe "*"
    }

    "Short" in {
      assert(shower.canShow(classOf[java.lang.Short]))
      val s: java.lang.Short = java.lang.Short.valueOf("42", 10)
      shower.show(s) shouldBe "42"
    }

    "Integer" in {
      assert(shower.canShow(classOf[java.lang.Integer]))
      val i: java.lang.Integer = 42
      shower.show(i) shouldBe "42"
    }

    "Long" in {
      assert(shower.canShow(classOf[java.lang.Long]))
      val l: java.lang.Long = 42L
      shower.show(l) shouldBe "42"
    }

    "BigInteger" in {
      assert(shower.canShow(classOf[math.BigInteger]))
      val bi: math.BigInteger = math.BigInteger.valueOf(42)
      shower.show(bi) shouldBe "42"
    }

    "Float" in {
      assert(shower.canShow(classOf[java.lang.Float]))
      val f: java.lang.Float = 42f
      shower.show(f) shouldBe "42.0"
    }

    "Double" in {
      assert(shower.canShow(classOf[java.lang.Double]))
      val d: java.lang.Double = 42d
      shower.show(d) shouldBe "42.0"
    }

    "BigDecimal" in {
      assert(shower.canShow(classOf[math.BigDecimal]))
      val bd: math.BigDecimal = math.BigDecimal.valueOf(42d)
      shower.show(bd) shouldBe "42.0"
    }

    "String" in {
      assert(shower.canShow(classOf[java.lang.String]))
      val s: java.lang.String = "42"
      shower.show(s) shouldBe "42"
    }

    "LocalDate" in {
      assert(shower.canShow(classOf[LocalDate]))
      val dateStr         = "2010-04-01"
      val date: LocalDate = LocalDate.parse(dateStr)
      shower.show(date) shouldBe dateStr
    }

    "OffsetDateTime" in {
      assert(shower.canShow(classOf[OffsetDateTime]))
      val dateTimeStr              = "2010-04-01T09:42:42Z"
      val dateTime: OffsetDateTime = OffsetDateTime.parse(dateTimeStr)
      shower.show(dateTime) shouldBe dateTimeStr
    }

    "URI" in {
      assert(shower.canShow(classOf[URI]))
      val uriStr = "https://example.com/"
      val uri    = new URI(uriStr)
      shower.show(uri) shouldBe uriStr
    }

    "URL" in {
      assert(shower.canShow(classOf[URL]))
      val urlStr = "https://example.com/"
      val url    = new URL(urlStr)
      shower.show(url) shouldBe urlStr
    }

    "UUID" in {
      assert(shower.canShow(classOf[java.util.UUID]))
      val str  = "c7cb2c94-0bd2-4cb3-a0f2-eaef5024955b"
      val uuid = java.util.UUID.fromString(str)
      shower.show(uuid) shouldBe str
    }
  }

  "Shower should be able to show for registered custom types" - {
    "Some random case class I'm going to make right here" in {
      case class Foo(first: String, last: String)
      assert(!shower.canShow(classOf[Foo]))
      shower.register[Foo](classOf[Foo], value => s"${value.first} ${value.last}")
      assert(shower.canShow(classOf[Foo]))
      val foo = Foo("Blast", "Hardcheese")
      shower.show(foo) shouldBe s"${foo.first} ${foo.last}"
    }
  }

  "Shower should not be able to show for unregistered types" - {
    "This test class" in {
      assert(!shower.canShow(getClass))
      Try(shower.show(this)) match {
        case Failure(t) => t.getClass shouldBe classOf[Shower.UnshowableInstanceException]
        case Success(_) => fail("shower.show() should have thrown and UnshowableInstanceException")
      }
    }
  }
}
