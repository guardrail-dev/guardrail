package com.twilio.guardrail.generators.syntax

import com.twilio.guardrail.generators.syntax.Java._
import org.scalatest.{ FreeSpec, Matchers }
import scala.util.Random

object JavaSyntaxTest {
  val TEST_RESERVED_WORDS = List(
    "for",
    "public",
    "if",
    "else",
    "throw"
  )
}

class JavaSyntaxTest extends FreeSpec with Matchers {
  import JavaSyntaxTest._

  "Reserved work escaper should" - {
    "Escape reserved words" in {
      TEST_RESERVED_WORDS.foreach({ word =>
        word.escapeReservedWord shouldBe (word + "_")
      })
    }

    "Not escape non-reserved words" in {
      List(
        "foo",
        "bar",
        "baz",
        "monkey",
        "cheese",
        "blah-moo",
        "aasdad2"
      ).foreach({ word =>
        word.escapeReservedWord shouldBe word
      })
    }
  }

  "Identifier escaper should" - {
    "Escape identifiers that are reserved words" in {
      TEST_RESERVED_WORDS.foreach({ word =>
        word.escapeIdentifier shouldBe (word + "_")
      })
    }

    "Escape identifiers that start with a number" in {
      List(
        "2",
        "3foo",
        "4-bar"
      ).foreach({ word =>
        word.escapeIdentifier shouldBe ("_" + word)
      })
    }

    "Not escape identifiers that don't start with numbers" in {
      List(
        "f",
        "foo",
        "bar-baz",
        "quux"
      ).foreach({ word =>
        word.escapeIdentifier shouldBe word
      })
    }

    "Escape properly with a bunch of random stuff thrown at it" in {
      new Random().alphanumeric
        .grouped(20)
        .take(500)
        .foreach({ wordChars =>
          val word    = wordChars.mkString
          val escaped = word.escapeIdentifier
          if ("^[0-9]".r.findFirstMatchIn(word).isDefined) {
            escaped shouldBe ("_" + word)
          } else {
            escaped shouldBe word
          }
        })
    }
  }
}
