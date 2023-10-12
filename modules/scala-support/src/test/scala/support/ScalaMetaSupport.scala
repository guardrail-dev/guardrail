package support

import org.scalactic.Equality
import org.scalatest.matchers._
import scala.meta._

trait ScalaMetaMatchers {
  implicit def TreeEquality[A <: Tree]: Equality[A] =
    new Equality[A] {
      def areEqual(a: A, b: Any): Boolean =
        b match {
          case x: Tree => a.structure == x.structure
          case _       => false
        }
    }

  class StructureMatcher(expectedStructure: Tree) extends Matcher[Tree] {
    def apply(left: Tree) =
      MatchResult(
        expectedStructure.syntax == left.syntax,
        s"""${expectedStructure.syntax} did not equal ${left.syntax}""",
        s"""Failure message"""
      )
  }

  def matchStructure(expectedStructure: Tree) = new StructureMatcher(expectedStructure)
}
