package dev.guardrail.generators.syntax

import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom
import scala.language.implicitConversions

class RichCollection[A, Repr](xs: IterableLike[A, Repr]) {
  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def distinctBy[B, That](f: A => B)(implicit cbf: CanBuildFrom[Repr, A, That]): That = {
    val builder = cbf(xs.repr)
    val i       = xs.iterator
    var set     = Set[B]()
    while (i.hasNext) {
      val o = i.next
      val b = f(o)
      if (!set(b)) {
        set += b
        builder += o
      }
    }
    builder.result
  }
}

trait SpecializedSyntax {
  implicit def toRichCollection[A, Repr](xs: IterableLike[A, Repr]): RichCollection[A, Repr] = new RichCollection(xs)
}
