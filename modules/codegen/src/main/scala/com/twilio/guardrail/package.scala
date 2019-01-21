package com.twilio

import cats.{ Applicative, Id, MonadError }
import cats.data.{ EitherK, EitherT, NonEmptyList, ReaderT, WriterT }
import cats.implicits._
import com.twilio.guardrail.languages.{ LA, ScalaLanguage }
import com.twilio.guardrail.protocol.terms.client.ClientTerm
import com.twilio.guardrail.protocol.terms.protocol._
import com.twilio.guardrail.protocol.terms.server.ServerTerm
import com.twilio.guardrail.terms.framework.FrameworkTerm
import com.twilio.guardrail.terms.{ ScalaTerm, SwaggerTerm }

import com.twilio.swagger.core.StructuredLogger

package guardrail {
  case class CodegenDefinitions[L <: LA](clients: List[Client[L]], servers: List[Server[L]])

  sealed trait ErrorKind
  case object UserError extends ErrorKind
  case object InternalError extends ErrorKind

  object Target {
    val A                        = Applicative[Target]
    def pure[T](x: T): Target[T] = A.pure(x)
    @deprecated("Use raiseError instead", "v0.41.2")
    def error[T](x: String): Target[T]      = raiseError(x)
    def raiseError[T](x: String): Target[T] = EitherT.fromEither(Left((x, UserError)))
    def raiseException[T](x: String): Target[T] = EitherT.fromEither(Left((x, InternalError)))
    def fromOption[T](x: Option[T], default: => String): Target[T] =
      EitherT.fromOption(x, (default, UserError))
    def unsafeExtract[T](x: Target[T]): T =
      x.valueOr({ err =>
          throw new Exception(err.toString)
        })
        .value

    object log {
      def debug(name: String, names: String*)(message: String): Target[Unit] =
        EitherT.right(WriterT.tell(StructuredLogger.debug(NonEmptyList(name, names.toList), message)))
      def info(name: String, names: String*)(message: String): Target[Unit] =
        EitherT.right(WriterT.tell(StructuredLogger.info(NonEmptyList(name, names.toList), message)))
      def warning(name: String, names: String*)(message: String): Target[Unit] =
        EitherT.right(WriterT.tell(StructuredLogger.warning(NonEmptyList(name, names.toList), message)))
      def error(name: String, names: String*)(message: String): Target[Unit] =
        EitherT.right(WriterT.tell(StructuredLogger.error(NonEmptyList(name, names.toList), message)))
    }
  }

  object CoreTarget {
    def pure[T](x: T): CoreTarget[T] = x.pure[CoreTarget]
    def fromOption[T](x: Option[T], default: => Error): CoreTarget[T] =
      EitherT.fromOption(x, default)
    @deprecated("Use raiseError instead", "v0.41.2")
    def error[T](x: Error): CoreTarget[T]      = EitherT.fromEither(Left(x))
    def raiseError[T](x: Error): CoreTarget[T] = EitherT.fromEither(Left(x))
    def unsafeExtract[T](x: CoreTarget[T]): T =
      x.valueOr({ err =>
          throw new Exception(err.toString)
        })
        .value

    object log {
      def debug(name: String, names: String*)(message: String): CoreTarget[Unit] =
        EitherT.right(WriterT.tell(StructuredLogger.debug(NonEmptyList(name, names.toList), message)))
      def info(name: String, names: String*)(message: String): CoreTarget[Unit] =
        EitherT.right(WriterT.tell(StructuredLogger.info(NonEmptyList(name, names.toList), message)))
      def warning(name: String, names: String*)(message: String): CoreTarget[Unit] =
        EitherT.right(WriterT.tell(StructuredLogger.warning(NonEmptyList(name, names.toList), message)))
      def error(name: String, names: String*)(message: String): CoreTarget[Unit] =
        EitherT.right(WriterT.tell(StructuredLogger.error(NonEmptyList(name, names.toList), message)))
    }
  }
}

package object guardrail {
  type DefinitionPM[L <: LA, T]    = EitherK[ProtocolSupportTerm[L, ?], ModelProtocolTerm[L, ?], T]
  type DefinitionPME[L <: LA, T]   = EitherK[EnumProtocolTerm[L, ?], DefinitionPM[L, ?], T]
  type DefinitionPMEA[L <: LA, T]  = EitherK[ArrayProtocolTerm[L, ?], DefinitionPME[L, ?], T]
  type DefinitionPMEAP[L <: LA, T] = EitherK[PolyProtocolTerm[L, ?], DefinitionPMEA[L, ?], T]

  type ModelInterpreters[L <: LA, T] = DefinitionPMEAP[L, T]

  type FrameworkC[L <: LA, T]   = EitherK[ClientTerm[L, ?], ModelInterpreters[L, ?], T]
  type FrameworkCS[L <: LA, T]  = EitherK[ServerTerm[L, ?], FrameworkC[L, ?], T]
  type FrameworkCSF[L <: LA, T] = EitherK[FrameworkTerm[L, ?], FrameworkCS[L, ?], T]

  type ClientServerTerms[L <: LA, T] = FrameworkCSF[L, T]

  type Parser[L <: LA, T] = EitherK[SwaggerTerm[L, ?], ClientServerTerms[L, ?], T]

  type CodegenApplication[L <: LA, T] = EitherK[ScalaTerm[L, ?], Parser[L, ?], T]

  type Logger[T]     = WriterT[Id, StructuredLogger, T]
  type Target[A]     = EitherT[Logger, (String, ErrorKind), A]
  type CoreTarget[A] = EitherT[Logger, Error, A]
}
