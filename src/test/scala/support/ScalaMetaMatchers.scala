package support

import scala.meta._
import org.scalatest.matchers._

trait ScalaMetaMatchers {
  class StructureMatcher(right: Term) extends Matcher[Term] {
    def apply(left: Term): MatchResult =
      MatchResult(
        left.structure == right.structure,
        s"""$left did not match structure $right""",
        s"""$left matched structure $right"""
      )
  }

  def matchStructure(right: Term): StructureMatcher =
    new StructureMatcher(right)
}

object ScalaMetaMatchers extends ScalaMetaMatchers
