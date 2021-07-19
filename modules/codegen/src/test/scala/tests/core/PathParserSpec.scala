package tests.core

import cats.data.NonEmptyList
import dev.guardrail.{ SwaggerUtil, Target }
import dev.guardrail.core.{ Tracker, TrackerTestExtensions }
import dev.guardrail.generators.LanguageParameter
import dev.guardrail.generators.Scala.model.{ CirceModelGenerator, ModelGeneratorType }
import dev.guardrail.generators.syntax.Scala._
import org.scalatest.{ EitherValues, OptionValues }
import support.ScalaMetaMatchers._
import dev.guardrail.languages.ScalaLanguage
import scala.meta._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PathParserSpec extends AnyFunSuite with Matchers with EitherValues with OptionValues with TrackerTestExtensions {

  val args: List[LanguageParameter[ScalaLanguage]] = List(
    LanguageParameter.fromParam(param"foo: Int = 1"),
    LanguageParameter.fromParam(param"bar: Int = 1"),
    LanguageParameter.fromParam(param"fooBar: Int = 1").withRawName("foo_bar"),
    LanguageParameter.fromParam(param"barBaz: Int = 1").withRawName("bar_baz")
  )

  implicit val modelGeneratorType: ModelGeneratorType = CirceModelGenerator.V012

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
        val gen = Target.unsafeExtract(generateUrlPathParams(Tracker(str), args))
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
        val NonEmptyList((gen, _), _) = Target.unsafeExtract(SwaggerUtil.paths.generateUrlAkkaPathExtractors(Tracker(str), args, CirceModelGenerator.V012))
        gen.toString shouldBe ((expected.toString))
      }
  }

  test("individual extractor components") {
    import atto._
    import Atto._
    import SwaggerUtil.paths.akkaExtractor._

    implicit val params: List[LanguageParameter[ScalaLanguage]] = List.empty

    plainString.parseOnly("foo/").either.value shouldBe "foo"
    plainNEString.parseOnly("foo/").either.value shouldBe "foo"
    plainNEString.parseOnly("").either.isLeft shouldBe true
    stringSegment.parseOnly("").either.isLeft shouldBe true
    stringSegment.parseOnly("foo").either.value should matchPattern {
      case (None, Lit.String("foo")) =>
    }
    segments.parseOnly("foo").either.value should matchPattern {
      case (None, Lit.String("foo")) :: Nil =>
    }
    segments.parseOnly("foo/bar").either.value should matchPattern {
      case (None, Lit.String("foo")) :: (None, Lit.String("bar")) :: Nil =>
    }
    qsValueOnly.parseOnly("").either.isLeft shouldBe true
    qsValueOnly.parseOnly("a=b").either.isLeft shouldBe true
    qsValueOnly.parseOnly("=").either.value shouldBe (("", ""))
    qsValueOnly.parseOnly("=b").either.value shouldBe (("", "b"))
    staticQSArg.parseOnly("=b").either.isLeft shouldBe true
    staticQSArg.parseOnly("a").either.value shouldBe (("a", ""))
    staticQSArg.parseOnly("a=").either.value shouldBe (("a", ""))
    staticQSArg.parseOnly("a=b").either.value shouldBe (("a", "b"))
    staticQSTerm.parseOnly("a").either.value should matchStructure(q""" parameter("a").require(_ == "") """)
    staticQSTerm.parseOnly("a=b").either.value should matchStructure(q""" parameter("a").require(_ == "b") """)
    staticQSTerm.parseOnly("=b").either.value should matchStructure(q""" parameter("").require(_ == "b") """)
    staticQS.parseOnly("?").either.value.isEmpty shouldBe true
    staticQS.parseOnly("?=").either.value.value should matchStructure(q""" parameter("").require(_ == "") """)
    staticQS.parseOnly("?a=").either.value.value should matchStructure(q""" parameter("a").require(_ == "") """)
    staticQS.parseOnly("?=b").either.value.value should matchStructure(q""" parameter("").require(_ == "b") """)
    staticQS.parseOnly("?a=b").either.value.value should matchStructure(q""" parameter("a").require(_ == "b") """)
    staticQS
      .parseOnly("?a=b&c=d")
      .either
      .value
      .value should matchStructure(q""" parameter("a").require(_ == "b") & parameter("c").require(_ == "d") """)
    pattern.parseOnly("").either.value should matchPattern {
      case (Nil, (false, None)) =>
    }
    pattern.parseOnly("?").either.value should matchPattern {
      case (Nil, (false, None)) =>
    }
  }
}
