package com.twilio

import cats.{ Applicative, Id }
import cats.data.{ EitherK, EitherT, NonEmptyList, ReaderT, WriterT }
import cats.instances.all._
import cats.syntax.applicative._
import cats.syntax.either._
import com.twilio.guardrail.generators.GeneratorSettings
import com.twilio.guardrail.languages.{ LA, ScalaLanguage }
import com.twilio.guardrail.protocol.terms.client.ClientTerm
import com.twilio.guardrail.protocol.terms.protocol._
import com.twilio.guardrail.protocol.terms.server.ServerTerm
import com.twilio.guardrail.terms.framework.FrameworkTerm
import com.twilio.guardrail.terms.{ ScalaTerm, SwaggerTerm }

import scala.meta._
import com.twilio.swagger.core.StructuredLogger

package guardrail {
  case class CodegenDefinitions[L <: LA](clients: List[Client[L]], servers: List[Server[L]])

  object Target {
    val A                        = Applicative[Target]
    def pure[T](x: T): Target[T] = A.pure(x)
    @deprecated("Use raiseError instead", "v0.41.2")
    def error[T](x: String): Target[T]      = raiseError(x)
    def raiseError[T](x: String): Target[T] = EitherT.fromEither(Left(x))
    def fromOption[T](x: Option[T], default: => String): Target[T] =
      EitherT.fromOption(x, default)
    def unsafeExtract[T](x: Target[T], generatorSettings: GeneratorSettings[ScalaLanguage]): T =
      x.valueOr({ err =>
          throw new Exception(err.toString)
        })
        .run(generatorSettings)
        .value
    def getGeneratorSettings: Target[GeneratorSettings[ScalaLanguage]] =
      EitherT.liftF(ReaderT.ask)

    object log {
      def debug(name: String, names: String*)(message: String): Target[Unit] =
        EitherT.right(ReaderT(_ => WriterT.tell(StructuredLogger.debug(NonEmptyList(name, names.toList), message))))
      def info(name: String, names: String*)(message: String): Target[Unit] =
        EitherT.right(ReaderT(_ => WriterT.tell(StructuredLogger.info(NonEmptyList(name, names.toList), message))))
      def warning(name: String, names: String*)(message: String): Target[Unit] =
        EitherT.right(ReaderT(_ => WriterT.tell(StructuredLogger.warning(NonEmptyList(name, names.toList), message))))
      def error(name: String, names: String*)(message: String): Target[Unit] =
        EitherT.right(ReaderT(_ => WriterT.tell(StructuredLogger.error(NonEmptyList(name, names.toList), message))))
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
  type DefinitionPM[T]     = EitherK[ProtocolSupportTerm[ScalaLanguage, ?], ModelProtocolTerm[ScalaLanguage, ?], T]
  type DefinitionPME[T]    = EitherK[EnumProtocolTerm[ScalaLanguage, ?], DefinitionPM, T]
  type DefinitionPMEA[T]   = EitherK[AliasProtocolTerm[ScalaLanguage, ?], DefinitionPME, T]
  type DefinitionPMEAA[T]  = EitherK[ArrayProtocolTerm[ScalaLanguage, ?], DefinitionPMEA, T]
  type DefinitionPMEAAP[T] = EitherK[PolyProtocolTerm[ScalaLanguage, ?], DefinitionPMEAA, T]

  type ModelInterpreters[T] = DefinitionPMEAAP[T]

  type FrameworkC[T]   = EitherK[ClientTerm[ScalaLanguage, ?], ModelInterpreters, T]
  type FrameworkCS[T]  = EitherK[ServerTerm[ScalaLanguage, ?], FrameworkC, T]
  type FrameworkCSF[T] = EitherK[FrameworkTerm[ScalaLanguage, ?], FrameworkCS, T]

  type ClientServerTerms[T] = FrameworkCSF[T]

  type Parser[T] = EitherK[SwaggerTerm[ScalaLanguage, ?], ClientServerTerms, T]

  type CodegenApplication[T] = EitherK[ScalaTerm[ScalaLanguage, ?], Parser, T]

  type Logger[T]     = WriterT[Id, StructuredLogger, T]
  type Settings[T]   = ReaderT[Logger, GeneratorSettings[ScalaLanguage], T]
  type Target[A]     = EitherT[Settings, String, A]
  type CoreTarget[A] = EitherT[Logger, Error, A]
}
