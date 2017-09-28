package swagger

import com.twilio.swagger.codegen.SwaggerUtil
import com.twilio.swagger.codegen.generators.ScalaParameter
import org.scalatest.{FunSuite, Matchers}
import scala.meta._

class PathParserSpec extends FunSuite with Matchers {

  val args: List[ScalaParameter] = List(
    ScalaParameter(param"foo: Int = 1", q"foo", q"foo", t"Int"),
    ScalaParameter(param"bar: Int = 1", q"bar", q"bar", t"Int"),
    ScalaParameter(param"fooBar: Int = 1", q"fooBar", q"foo_bar", t"Int"),
    ScalaParameter(param"barBaz: Int = 1", q"barBaz", q"bar_baz", t"Int")
  )

  List[(String, Term)](
    ("", q""" host + basePath """)
  , ("/", q""" host + basePath + "/" """)
  , ("/foo", q""" host + basePath + "/foo" """)
  , ("/foo/", q""" host + basePath + "/foo/" """)
  , ("/{foo}", q""" host + basePath + "/" + Formatter.addPath(foo) """)
  , ("/{foo}.json", q""" host + basePath + "/" + Formatter.addPath(foo) + ".json" """)
  , ("/{foo}/{bar}.json", q""" host + basePath + "/" + Formatter.addPath(foo) + "/" + Formatter.addPath(bar) + ".json" """)
  , ("/{foo_bar}/{bar_baz}.json", q""" host + basePath + "/" + Formatter.addPath(fooBar) + "/" + Formatter.addPath(barBaz) + ".json" """)
  ).foreach { case (str, expected) =>
    test(str) {
      val gen = SwaggerUtil.paths.generateUrlPathParams(str, args)(identity).right.get
      gen.toString shouldBe(expected.toString)
    }
  }
}
