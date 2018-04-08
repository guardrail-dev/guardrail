package com.twilio

import cats.{Applicative, Id}
import cats.data.{EitherK, EitherT, NonEmptyList, WriterT}
import cats.instances.all._
import cats.syntax.applicative._
import cats.syntax.either._
import com.twilio.guardrail.terms.{ScalaTerm, SwaggerTerm}
import com.twilio.guardrail.terms.client.ClientTerm
import com.twilio.guardrail.terms.protocol.{AliasProtocolTerm, ArrayProtocolTerm, EnumProtocolTerm, ModelProtocolTerm, ProtocolSupportTerm}
import com.twilio.guardrail.terms.server.ServerTerm
import com.twilio.guardrail.terms.framework.FrameworkTerm
import scala.meta._
import com.twilio.swagger.core.StructuredLogger

package guardrail {
  case class CodegenDefinitions(clients: List[Client], servers: List[Server])

  object Target {
    val A = Applicative[Target]
    def pure[T](x: T): Target[T] = A.pure(x)
    def error[T](x: String): Target[T] = EitherT.left[T](x.pure[Logger])
    def fromOption[T](x: Option[T], default: => String): Target[T] = EitherT.fromOption(x, default)
    def unsafeExtract[T](x: Target[T]): T = x.valueOr({ err => throw new Exception(err.toString) }).value

    object log {
      def debug(name: String, names: String*)(message: String): Target[Unit] = EitherT.right(WriterT.tell(StructuredLogger.debug(NonEmptyList(name, names.toList), message)))
      def info(name: String, names: String*)(message: String): Target[Unit] = EitherT.right(WriterT.tell(StructuredLogger.info(NonEmptyList(name, names.toList), message)))
      def warning(name: String, names: String*)(message: String): Target[Unit] = EitherT.right(WriterT.tell(StructuredLogger.warning(NonEmptyList(name, names.toList), message)))
      def error(name: String, names: String*)(message: String): Target[Unit] = EitherT.right(WriterT.tell(StructuredLogger.error(NonEmptyList(name, names.toList), message)))
    }
  }

  object CoreTarget {
    def pure[T](x: T): CoreTarget[T] = x.pure[CoreTarget]
    def fromOption[T](x: Option[T], default: => Error): CoreTarget[T] = EitherT.fromOption(x, default)
    def error[T](x: Error): CoreTarget[T] = EitherT.left[T](x.pure[Logger])
    def unsafeExtract[T](x: CoreTarget[T]): T = x.valueOr({ err => throw new Exception(err.toString) }).value

    object log {
      def debug(name: String, names: String*)(message: String): CoreTarget[Unit] = EitherT.right(WriterT.tell(StructuredLogger.debug(NonEmptyList(name, names.toList), message)))
      def info(name: String, names: String*)(message: String): CoreTarget[Unit] = EitherT.right(WriterT.tell(StructuredLogger.info(NonEmptyList(name, names.toList), message)))
      def warning(name: String, names: String*)(message: String): CoreTarget[Unit] = EitherT.right(WriterT.tell(StructuredLogger.warning(NonEmptyList(name, names.toList), message)))
      def error(name: String, names: String*)(message: String): CoreTarget[Unit] = EitherT.right(WriterT.tell(StructuredLogger.error(NonEmptyList(name, names.toList), message)))
    }
  }
}

package object guardrail {
  type CodegenApplicationSP[T] = EitherK[ProtocolSupportTerm, ServerTerm, T]
  type CodegenApplicationMSP[T] = EitherK[ModelProtocolTerm, CodegenApplicationSP, T]
  type CodegenApplicationEMSP[T] = EitherK[EnumProtocolTerm, CodegenApplicationMSP, T]
  type CodegenApplicationCEMSP[T] = EitherK[ClientTerm, CodegenApplicationEMSP, T]
  type CodegenApplicationACEMSP[T] = EitherK[AliasProtocolTerm, CodegenApplicationCEMSP, T]
  type CodegenApplicationACEMSSP[T] = EitherK[ScalaTerm, CodegenApplicationACEMSP, T]
  type CodegenApplicationACEMSSPR[T] = EitherK[ArrayProtocolTerm, CodegenApplicationACEMSSP, T]
  type CodegenApplicationACEMSSPRS[T] = EitherK[SwaggerTerm, CodegenApplicationACEMSSPR, T]
  type CodegenApplicationACEMSSPRSF[T] = EitherK[FrameworkTerm, CodegenApplicationACEMSSPRS, T]
  type CodegenApplication[T] = CodegenApplicationACEMSSPRSF[T]

  type Logger[T] = WriterT[Id, StructuredLogger, T]
  type Target[A] = EitherT[Logger, String, A]
  type CoreTarget[A] = EitherT[Logger, Error, A]
}
