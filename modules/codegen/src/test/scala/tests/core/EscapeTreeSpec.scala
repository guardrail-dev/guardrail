package tests.core

import org.scalatest.{ FunSuite, Matchers }

import scala.meta._

class EscapeTreeSpec extends FunSuite with Matchers {

  test("Assume special characters are not escaped") {
    val q"val $x = 3"                          = q"val `dashy-thing` = 3"
    val x1 @ Pat.Var(Term.Name("dashy-thing")) = x
    q"val $x1 = 3".syntax should equal("val `dashy-thing` = 3")
  }

  List[(Tree, String)](
    (Init(Type.Name("dashy-enum"), Name("what"), List()), "`dashy-enum`"),
    (Term.Name("dashy-class"), "`dashy-class`"),
    (
      Term.Param(Nil, Term.Name("dashy-param"), Some(Type.Apply(Type.Name("Option"), List(Type.Name("Long")))), Some(Term.Name("None"))),
      "`dashy-param`: Option[Long] = None"
    ),
    (Type.Name("dashy-class"), "`dashy-class`")
  ).foreach {
    case (x, y) =>
      test(s"${x.structure} should be escaped as ${y}") {
        x.syntax shouldEqual (y)
      }
  }
}
