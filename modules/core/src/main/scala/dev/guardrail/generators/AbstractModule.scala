package dev.guardrail.generators

import cats.data.{ NonEmptyList, StateT }
import cats.syntax.all._

import dev.guardrail._
import dev.guardrail.languages.LA

abstract class AbstractModule[L <: LA] {
  def popModule[A](section: String, first: (String, Target[A]), rest: (String, Target[A])*): StateT[Target, Set[String], A] =
    StateT[Target, Set[String], A] { modules =>
      modules.toList.flatTraverse { module =>
        (first :: rest.toList).flatTraverse {
          case (`module`, value) => (Set.empty[String], List(value))
          case _                 => (Set(module), Nil)
        }
      } match {
        case (_, Nil)             => Target.raiseError(MissingModule(section, (first :: rest.toList).map(_._1)))
        case (rest, value :: Nil) => value.map((rest, _))
        case (rest, a :: b :: _) =>
          Target.raiseError(ModuleConflict(section))
      }
    }

  def extract(modules: NonEmptyList[String]): Target[Framework[L, Target]]
}
