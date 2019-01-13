package tests.scalatest

import cats.Functor
import cats.data.EitherT
import org.scalactic.source
import org.scalatest._
import org.scalatest.exceptions.{ StackDepthException, TestFailedException }
import scala.language.higherKinds
import scala.language.implicitConversions

trait EitherTValues {

  implicit def convertEitherTToValuable[F[_]: Functor, L, R](eitherT: EitherT[F, L, R]) = new EitherTValuable(eitherT)

  class EitherTValuable[F[_]: Functor, L, R](eitherT: EitherT[F, L, R]) {
    def leftValue(implicit pos: source.Position): F[L] =
      eitherT.fold(identity, { _ =>
        throw new TestFailedException((_: StackDepthException) => Option.empty[String], Option.empty[Throwable], pos)
      })

    def rightValue(implicit pos: source.Position): F[R] =
      eitherT.fold({ _ =>
        throw new TestFailedException((_: StackDepthException) => Option.empty[String], Option.empty[Throwable], pos)
      }, identity)
  }
}
