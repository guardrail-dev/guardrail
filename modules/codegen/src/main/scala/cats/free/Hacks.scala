package cats.free

import cats.data.EitherK
import cats.free.Free.{ FlatMapped, Pure, Suspend }
import cats.implicits._

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
            .lines
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

  def injectLogs[F[_]: TermName, A](value: Free[F, A],
                                    ignore: Set[String],
                                    push: String => Free[F, Unit],
                                    pop: Free[F, Unit],
                                    emit: Free[F, Unit]): Free[F, A] = {
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
