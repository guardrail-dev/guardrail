package dev.guardrail
package generators

import dev.guardrail.languages.LA
import cats.data.StateT
import cats.syntax.all._

abstract class AbstractModule[L <: LA] {
  def popModule[A](section: String, first: (String, A), rest: (String, A)*): StateT[Target, Set[String], A] =
    StateT[Target, Set[String], A]({ modules =>
      modules.toList.flatTraverse { module =>
        (first :: rest.toList).flatTraverse({
          case (`module`, value) => (Set.empty[String], List(value))
          case _                 => (Set(module), Nil)
        })
      } match {
        case (_, Nil)             => Target.raiseError(MissingModule(section, (first :: rest.toList).map(_._1)))
        case (rest, value :: Nil) => Target.pure((rest, value))
        case (rest, a :: b :: _) =>
          Target.raiseError(ModuleConflict(section))
      }
    })
}
