package tests.core

import com.twilio.guardrail.generators.ScalaParameter
import com.twilio.guardrail.{ SwaggerUtil, Target }
import org.scalatest.{ EitherValues, FunSuite, Matchers, OptionValues }
import support.ScalaMetaMatchers._
import com.twilio.guardrail.generators.GeneratorSettings
import com.twilio.guardrail.tests._
import scala.meta._

class PathParserSpec extends FunSuite with Matchers with EitherValues with OptionValues {

  implicit val gs = new GeneratorSettings(t"io.circe.Json", t"BodyPartEntity")

  val args: List[ScalaParameter] = List(
    ScalaParameter.fromParam(param"foo: Int = 1"),
    ScalaParameter.fromParam(param"bar: Int = 1"),
    ScalaParameter.fromParam("foo_bar")(param"fooBar: Int = 1"),
    ScalaParameter.fromParam("bar_baz")(param"barBaz: Int = 1")
  )

  List[(String, Term)](
    ("", q""" host + basePath """),
    ("/", q""" host + basePath + "/" """),
    ("/foo", q""" host + basePath + "/foo" """),
    ("/foo/", q""" host + basePath + "/foo/" """),
    ("/{foo}", q""" host + basePath + "/" + Formatter.addPath(foo) """),
    ("/{foo}.json", q""" host + basePath + "/" + Formatter.addPath(foo) + ".json" """),
    ("/{foo}/{bar}.json", q""" host + basePath + "/" + Formatter.addPath(foo) + "/" + Formatter.addPath(bar) + ".json" """),
    ("/{foo_bar}/{bar_baz}.json", q""" host + basePath + "/" + Formatter.addPath(fooBar) + "/" + Formatter.addPath(barBaz) + ".json" """)
  ).foreach {
    case (str, expected) =>
      test(s"Client $str") {
        val gen = Target.unsafeExtract(SwaggerUtil.paths.generateUrlPathParams(str, args), defaults.akkaGeneratorSettings)
        gen.toString shouldBe expected.toString
      }
  }

  List[(String, Term)](
    ("", q""" pathEnd """),
    ("foo", q""" path("foo") """),
    ("foo/", q""" pathPrefix("foo") & pathEndOrSingleSlash """),
    ("{foo}", q""" path(IntNumber) """),
    ("{foo}.json", q""" path(new scala.util.matching.Regex("^" + "" + "(.*)" + ".json" + ${Lit
      .String("$")}).flatMap(str => io.circe.Json.fromString(str).as[Int].toOption)) """),
    ("{foo}/{bar}.json", q""" path(IntNumber / new scala.util.matching.Regex("^" + "" + "(.*)" + ".json" + ${Lit
      .String("$")}).flatMap(str => io.circe.Json.fromString(str).as[Int].toOption)) """),
    ("{foo_bar}/{bar_baz}.json", q""" path(IntNumber / new scala.util.matching.Regex("^" + "" + "(.*)" + ".json" + ${Lit
      .String("$")}).flatMap(str => io.circe.Json.fromString(str).as[Int].toOption)) """),
    ("foo?abort=1", q""" path("foo") & parameter("abort").require(_ == "1") """),
    ("{foo}.json?abort=1", q""" path(new scala.util.matching.Regex("^" + "" + "(.*)" + ".json" + ${Lit
      .String("$")}).flatMap(str => io.circe.Json.fromString(str).as[Int].toOption)) & parameter("abort").require(_ == "1") """),
    ("?", q""" pathEnd """),
    ("?a", q""" pathEnd & parameter("a").require(_ == "") """),
    ("?=", q""" pathEnd & parameter("").require(_ == "") """),
    ("?=b", q""" pathEnd & parameter("").require(_ == "b") """)
  ).foreach {
    case (str, expected) =>
      test(s"Server ${str}") {
        val gen = Target.unsafeExtract(SwaggerUtil.paths.generateUrlAkkaPathExtractors(str, args), defaults.akkaGeneratorSettings)
        gen.toString shouldBe (expected.toString)
      }
  }

  test("individual extractor components") {
    import atto._
    import Atto._
    import SwaggerUtil.paths.akkaExtractor._

    implicit val params: List[ScalaParameter] = List.empty

    plainString.parseOnly("foo/").either.right.value shouldBe "foo"
    plainNEString.parseOnly("foo/").either.right.value shouldBe "foo"
    plainNEString.parseOnly("").either.isLeft shouldBe true
    stringSegment.parseOnly("").either.isLeft shouldBe true
    stringSegment.parseOnly("foo").either.right.value should matchPattern {
      case (None, Lit.String("foo")) =>
    }
    segments.parseOnly("foo").either.right.value should matchPattern {
      case (None, Lit.String("foo")) :: Nil =>
    }
    segments.parseOnly("foo/bar").either.right.value should matchPattern {
      case (None, Lit.String("foo")) :: (None, Lit.String("bar")) :: Nil =>
    }
    qsValueOnly.parseOnly("").either.isLeft shouldBe true
    qsValueOnly.parseOnly("a=b").either.isLeft shouldBe true
    qsValueOnly.parseOnly("=").either.right.value shouldBe ("", "")
    qsValueOnly.parseOnly("=b").either.right.value shouldBe ("", "b")
    staticQSArg.parseOnly("=b").either.isLeft shouldBe true
    staticQSArg.parseOnly("a").either.right.value shouldBe ("a", "")
    staticQSArg.parseOnly("a=").either.right.value shouldBe ("a", "")
    staticQSArg.parseOnly("a=b").either.right.value shouldBe ("a", "b")
    staticQSTerm.parseOnly("a").either.right.value should matchStructure(q""" parameter("a").require(_ == "") """)
    staticQSTerm.parseOnly("a=b").either.right.value should matchStructure(q""" parameter("a").require(_ == "b") """)
    staticQSTerm.parseOnly("=b").either.right.value should matchStructure(q""" parameter("").require(_ == "b") """)
    staticQS.parseOnly("?").either.right.value.isEmpty shouldBe true
    staticQS.parseOnly("?=").either.right.value.value should matchStructure(q""" parameter("").require(_ == "") """)
    staticQS.parseOnly("?a=").either.right.value.value should matchStructure(q""" parameter("a").require(_ == "") """)
    staticQS.parseOnly("?=b").either.right.value.value should matchStructure(q""" parameter("").require(_ == "b") """)
    staticQS.parseOnly("?a=b").either.right.value.value should matchStructure(q""" parameter("a").require(_ == "b") """)
    staticQS
      .parseOnly("?a=b&c=d")
      .either
      .right
      .value
      .value should matchStructure(q""" parameter("a").require(_ == "b") & parameter("c").require(_ == "d") """)
    pattern.parseOnly("").either.right.value should matchPattern {
      case (Nil, (false, None)) =>
    }
    pattern.parseOnly("?").either.right.value should matchPattern {
      case (Nil, (false, None)) =>
    }
  }
}
