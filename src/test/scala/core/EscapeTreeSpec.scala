package swagger

import com.twilio.swagger.codegen.SwaggerUtil
import org.scalatest.{FunSuite, Matchers}
import scala.collection.immutable.{Seq => ISeq}
import scala.meta._

class EscapeTreeSpec extends FunSuite with Matchers {

  test("Assume special characters are not escaped") {
    // This test fails as of 1.6.0. If this changes in the future, SwaggerParser.escapeTermTree and friends can all be removed (:yey:)
    val q"val $x = 3" = q"val `dashy-thing` = 3"
    val x1@Pat.Var.Term(Term.Name("dashy-thing")) = x
    q"val $x1 = 3".toString shouldNot equal("val `dashy-thing` = 3") // shouldNot -> should (!) This is a bug in scala.meta!
  }

  test("Fiddly cases") {
    // If either of the following start failing, this is because scalameta has
    // changed the way Term.Names are interpolated. See notes in BacktickTest,
    // and re-evaluate whether ' ' should be considered an invalid character
    // (or if escapeTree can be removed altogether!)
    Term.Name("post /dashy-path").toString shouldEqual("`post /dashy-path`")
    SwaggerUtil.escapeTree(Term.Name("post /dashy-path")).toString shouldEqual("`post /dashy-path`")
  }

  List[(Tree, Tree)](
    (Ctor.Ref.Name("dashy-enum"), Ctor.Ref.Name("`dashy-enum`")),
    (Term.Name("dashy-class"), Term.Name("`dashy-class`")),
    (Term.Param(Nil, Term.Name("dashy-param"), Some(Type.Apply(Type.Name("Option"), ISeq(Type.Name("Long")))), Some(Term.Name("None"))), Term.Param(Nil, Term.Name("`dashy-param`"), Some(Type.Apply(Type.Name("Option"), ISeq(Type.Name("Long")))), Some(Term.Name("None")))),
    (Type.Name("dashy-class"), Type.Name("`dashy-class`"))
  ).foreach { case (x, y) =>
    test(s"${x.structure} should be escaped as ${y.structure}") {
      SwaggerUtil.escapeTree(x).toString shouldEqual(y.toString)
    }
  }
}
