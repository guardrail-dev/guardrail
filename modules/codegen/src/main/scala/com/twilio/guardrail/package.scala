package com.twilio

import cats.{ Applicative, Id }
import cats.data.{ EitherK, EitherT, NonEmptyList, ReaderT, WriterT }
import cats.instances.all._
import cats.syntax.applicative._
import cats.syntax.either._
import com.twilio.guardrail.protocol.terms.client.ClientTerm
import com.twilio.guardrail.protocol.terms.protocol._
import com.twilio.guardrail.protocol.terms.server.ServerTerm
import com.twilio.guardrail.terms.{ ScalaTerm, SwaggerTerm }
import com.twilio.guardrail.terms.framework.FrameworkTerm
import com.twilio.guardrail.generators.GeneratorSettings

import scala.meta._
import com.twilio.swagger.core.StructuredLogger

package guardrail {
  case class CodegenDefinitions(clients: List[Client], servers: List[Server])

  object Target {
    val A                              = Applicative[Target]
    def pure[T](x: T): Target[T]       = A.pure(x)
    def error[T](x: String): Target[T] = EitherT.fromEither(Left(x))
    def fromOption[T](x: Option[T], default: => String): Target[T] =
      EitherT.fromOption(x, default)
    def unsafeExtract[T](x: Target[T], generatorSettings: GeneratorSettings): T =
      x.valueOr({ err =>
          throw new Exception(err.toString)
        })
        .run(generatorSettings)
        .value
    def getGeneratorSettings: Target[GeneratorSettings] =
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
    def error[T](x: Error): CoreTarget[T] = EitherT.fromEither(Left(x))
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
  type DefinitionPM[T]     = EitherK[ProtocolSupportTerm, ModelProtocolTerm, T]
  type DefinitionPME[T]    = EitherK[EnumProtocolTerm, DefinitionPM, T]
  type DefinitionPMEA[T]   = EitherK[AliasProtocolTerm, DefinitionPME, T]
  type DefinitionPMEAA[T]  = EitherK[ArrayProtocolTerm, DefinitionPMEA, T]
  type DefinitionPMEAAP[T] = EitherK[PolyProtocolTerm, DefinitionPMEAA, T]

  type ModelInterpreters[T] = DefinitionPMEAAP[T]

  type FrameworkC[T]   = EitherK[ClientTerm, ModelInterpreters, T]
  type FrameworkCS[T]  = EitherK[ServerTerm, FrameworkC, T]
  type FrameworkCSF[T] = EitherK[FrameworkTerm, FrameworkCS, T]

  type ClientServerTerms[T] = FrameworkCSF[T]

  type Parser[T] = EitherK[SwaggerTerm, ClientServerTerms, T]

  type CodegenApplication[T] = EitherK[ScalaTerm, Parser, T]

  type Logger[T]     = WriterT[Id, StructuredLogger, T]
  type Settings[T]   = ReaderT[Logger, GeneratorSettings, T]
  type Target[A]     = EitherT[Settings, String, A]
  type CoreTarget[A] = EitherT[Logger, Error, A]
}
