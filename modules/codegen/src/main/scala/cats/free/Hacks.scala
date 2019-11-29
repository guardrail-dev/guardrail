package cats.free

import cats.data.EitherK
import cats.free.Free.{ FlatMapped, Pure, Suspend }
import cats.implicits._

/* GuardrailFreeHacks
 *
 *  Motivation:
 *    Due to the complex nature of code generation codebases,
 *    combined with the inherent complexity of Free monads,
 *    it proved necessary to attempt to inject additional logging
 *    information into the Free-monadic call graph.
 *
 *    This may end up being the wrong approach to take in the end,
 *    at which point it can be safely removed, though for the time
 *    being the expected benefit is expressing a direct relationship between
 *    logs (via SwaggerTerms.log, Target.log, or anything else that uses
 *    StructuredLogger) and the functions that actually did the logging.
 *
 *  Implementation:
 *    By putting ourselves in the cats.free package, we can directly match
 *    on individual members of Free.
 *
 *    This lets us guess (with some confidence) when we're making some a
 *    function call vs simply the next line in a for comprehension.
 *
 *    By exposing the push/pop terms from StructuredLogger as parameters,
 *    we can influence the final interpreted tree by propagating through all
 *    other suspended functions throughout the Free members, without
 *    polluting the implementation with guardrail specifics.
 *
 *    Finally, as we're operating from inside the Free implementation,
 *    we can actually reflect on the name of the GADT in order to inject
 *    those as elements of structure in the StructuredLogger for free,
 *    including logging their parameters if the toString representation
 *    is shorter than the name threshold.
 *
 *  Limitations:
 *    - As mentioned earlier, reflecting on the GADT names means we're
 *      unable to differentiate identically named terms from different
 *      algebras. This turns out to not be that big of an issue, as we're
 *      only explicitly ignoring a handful, which are likely to not conflict
 *      with other algebras.
 *
 *    - Pure Scala functions that return Free are completely invisible to this
 *      technique, and must be explicitly marked with
 *      log.function("functionName").
 *      It will add a lot of noise in the codebase, but I currently don't see
 *      an alternate approach that gives us the level of debuggability that
 *      we are able to achieve with this approach.
 *      If it does turn out that there's a better approach, it would be much
 *      easier to remove than to write, as those function calls are simply
 *      wrappers and can be removed without altering anything about the
 *      actual execution.
 *
 */
object GuardrailFreeHacks {
  val limit: Int = 80

  trait TermName[F[_]] { def extract[A](term: F[A]): (String, String) }

  trait LowPriorityTermName {
    implicit def baseTermName[F[_]]: TermName[F] = new TermName[F] {
      def extract[A](term: F[A]) =
        (
          term.getClass.getSimpleName,
          term
            .toString()
            .linesIterator
            .filterNot(_.contains(": null"))
            .mkString("; ")
        )
    }
  }

  trait HighPriorityTermName extends LowPriorityTermName {
    implicit def extractEitherT[F[_], G[_]](implicit evF: TermName[F], evG: TermName[G]): TermName[EitherK[F, G, ?]] = new TermName[EitherK[F, G, ?]] {
      def extract[A](term: EitherK[F, G, A]) = term.run.fold(evF.extract _, evG.extract _)
    }
  }

  object TermName extends HighPriorityTermName {
    def apply[F[_], A](value: F[A])(implicit ev: TermName[F]): String = {
      val (name, full) = ev.extract(value)
      if (full.length > limit) name else full
    }
    def full[F[_], A](value: F[A])(implicit ev: TermName[F]): String     = ev.extract(value)._2
    def nameOnly[F[_], A](value: F[A])(implicit ev: TermName[F]): String = ev.extract(value)._1
  }

  def injectLogs[F[_]: TermName, A](
      value: Free[F, A],
      ignore: Set[String],
      push: String => Free[F, Unit],
      pop: Free[F, Unit],
      emit: Free[F, Unit]
  ): Free[F, A] = {
    def next[B]: Free[F, B] => Free[F, B] = {
      case Pure(a) =>
        Pure(a)
      case Suspend(a) =>
        Suspend(a)
      case FlatMapped(Suspend(a), f) if ignore contains TermName.nameOnly(a) =>
        FlatMapped(Suspend(a), f.map(next))
      case FlatMapped(Suspend(a), f) =>
        push(TermName(a)) *> FlatMapped(Suspend(a), f.map(x => emit *> pop *> next(x)))
      case FlatMapped(c, f) =>
        FlatMapped(next(c), f.map(next))
    }
    next(value)
  }
}
