package com.twilio.guardrail
package generators

import com.twilio.guardrail.languages.LA
import cats.data.StateT
import cats.implicits._

abstract class AbstractModule[L <: LA] {
  def popModule[A](section: String, first: (String, A), rest: (String, A)*): StateT[Target, Set[String], A] =
    StateT[Target, Set[String], A]({ modules =>
      modules.toList.flatTraverse { module =>
        (first :: rest.toList).flatTraverse({
          case (`module`, value) => (Set.empty[String], List(value))
          case _                 => (Set(module), Nil)
        })
      } match {
        case (rest, Nil)          => Target.raiseError(MissingModule(section))
        case (rest, value :: Nil) => Target.pure((rest, value))
        case (rest, a :: b :: _) =>
          Target.raiseError(ModuleConflict(section))
      }
    })
}
