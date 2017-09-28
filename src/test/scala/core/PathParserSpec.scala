package swagger

import com.twilio.swagger.codegen.SwaggerUtil
import org.scalatest.{FunSuite, Matchers}
import scala.meta._

class PathParserSpec extends FunSuite with Matchers {

  List[(String, Term)](
    ("", q""" host + basePath """)
  , ("/", q""" host + basePath + "/" """)
  , ("/foo", q""" host + basePath + "/foo" """)
  , ("/foo/", q""" host + basePath + "/foo/" """)
  , ("/{foo}", q""" host + basePath + "/" + Formatter.addPath(foo) """)
  , ("/{foo}.json", q""" host + basePath + "/" + Formatter.addPath(foo) + ".json" """)
  , ("/{foo}/{bar}.json", q""" host + basePath + "/" + Formatter.addPath(foo) + "/" + Formatter.addPath(bar) + ".json" """)
  ).foreach { case (str, expected) =>
    test(str) {
      val gen = SwaggerUtil.paths.generateUrlPathParams(str)(identity).right.get
      gen.structure shouldBe(expected.structure)
    }
  }
}
