package com.twilio.swagger

import cats.{Applicative, Id}
import cats.data.{Coproduct, EitherT, NonEmptyList, WriterT}
import cats.instances.all._
import cats.syntax.applicative._
import cats.syntax.either._
import com.twilio.swagger.codegen.terms.{ScalaTerm, SwaggerTerm}
import com.twilio.swagger.codegen.terms.client.ClientTerm
import com.twilio.swagger.codegen.terms.protocol.{AliasProtocolTerm, ArrayProtocolTerm, EnumProtocolTerm, ModelProtocolTerm, ProtocolSupportTerm}
import com.twilio.swagger.codegen.terms.server.ServerTerm
import scala.meta._
import com.twilio.swagger.core.StructuredLogger

package codegen {
  case class CodegenDefinitions(clients: List[Client], servers: List[Server], frameworkImports: List[Import])

  object Target {
    val A = Applicative[Target]
    def pure[T](x: T): Target[T] = A.pure(x)
    def error[T](x: String): Target[T] = EitherT.left[Logger, String, T](x.pure[Logger])
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
    def error[T](x: Error): CoreTarget[T] = EitherT.left[Logger, Error, T](x.pure[Logger])
    def unsafeExtract[T](x: CoreTarget[T]): T = x.valueOr({ err => throw new Exception(err.toString) }).value

    object log {
      def debug(name: String, names: String*)(message: String): CoreTarget[Unit] = EitherT.right(WriterT.tell(StructuredLogger.debug(NonEmptyList(name, names.toList), message)))
      def info(name: String, names: String*)(message: String): CoreTarget[Unit] = EitherT.right(WriterT.tell(StructuredLogger.info(NonEmptyList(name, names.toList), message)))
      def warning(name: String, names: String*)(message: String): CoreTarget[Unit] = EitherT.right(WriterT.tell(StructuredLogger.warning(NonEmptyList(name, names.toList), message)))
      def error(name: String, names: String*)(message: String): CoreTarget[Unit] = EitherT.right(WriterT.tell(StructuredLogger.error(NonEmptyList(name, names.toList), message)))
    }
  }
}

package object codegen {
  type CodegenApplicationSP[T] = Coproduct[ProtocolSupportTerm, ServerTerm, T]
  type CodegenApplicationMSP[T] = Coproduct[ModelProtocolTerm, CodegenApplicationSP, T]
  type CodegenApplicationEMSP[T] = Coproduct[EnumProtocolTerm, CodegenApplicationMSP, T]
  type CodegenApplicationCEMSP[T] = Coproduct[ClientTerm, CodegenApplicationEMSP, T]
  type CodegenApplicationACEMSP[T] = Coproduct[AliasProtocolTerm, CodegenApplicationCEMSP, T]
  type CodegenApplicationACEMSSP[T] = Coproduct[ScalaTerm, CodegenApplicationACEMSP, T]
  type CodegenApplicationACEMSSPR[T] = Coproduct[ArrayProtocolTerm, CodegenApplicationACEMSSP, T]
  type CodegenApplicationACEMSSPRS[T] = Coproduct[SwaggerTerm, CodegenApplicationACEMSSPR, T]
  type CodegenApplication[T] = CodegenApplicationACEMSSPRS[T]

  type Logger[T] = WriterT[Id, StructuredLogger, T]
  type Target[A] = EitherT[Logger, String, A]
  type CoreTarget[A] = EitherT[Logger, Error, A]
}
