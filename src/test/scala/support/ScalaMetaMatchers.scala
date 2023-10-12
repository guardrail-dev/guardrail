package support

import scala.meta._
import org.scalatest.matchers._

trait ScalaMetaMatchers {
  class StructureMatcher(right: Tree) extends Matcher[Tree] {
    def apply(left: Tree): MatchResult =
      MatchResult(
        left.syntax == right.syntax,
        s"""$left did not match $right""",
        s"""$left matched $right"""
      )
  }

  def matchStructure(right: Tree): StructureMatcher =
    new StructureMatcher(right)
}

object ScalaMetaMatchers extends ScalaMetaMatchers
