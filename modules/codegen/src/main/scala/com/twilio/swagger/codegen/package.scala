package com.twilio.swagger

import cats.{Applicative, Id}
import cats.data.{Coproduct, EitherT, WriterT}
import cats.instances.all._
import cats.syntax.applicative._
import cats.syntax.either._
import com.twilio.swagger.codegen.terms.ScalaTerm
import com.twilio.swagger.codegen.terms.client.ClientTerm
import com.twilio.swagger.codegen.terms.protocol.{AliasProtocolTerm, EnumProtocolTerm, ModelProtocolTerm, ProtocolSupportTerm}
import com.twilio.swagger.codegen.terms.server.ServerTerm
import scala.meta._

package codegen {
  case class CodegenDefinitions(clients: List[Client], servers: List[Server], frameworkImports: List[Import])

  object Target {
    type ErrorType = String
    type Logger[T] = WriterT[Id, List[String], T]
    type Type[A] = EitherT[Logger, ErrorType, A]
    val A = Applicative[Target]
    def pure[T](x: T): Target[T] = A.pure(x)
    def error[T](x: ErrorType): Target[T] = EitherT.left[Logger, ErrorType, T](x.pure[Logger])
    def fromOption[T](x: Option[T], default: => ErrorType): Target[T] = EitherT.fromOption(x, default)
    def unsafeExtract[T](x: Type[T]): T = x.valueOr({ err => throw new Exception(err.toString) }).value
  }

  object CoreTarget {
    type ErrorType = Error
    type Type[A] = EitherT[Id, ErrorType, A]
    val A = Applicative[CoreTarget]
    def pure[T](x: T): CoreTarget[T] = A.pure(x)
    def fromOption[T](x: Option[T], default: => ErrorType): CoreTarget[T] = EitherT.fromOption(x, default)
    def error[T](x: ErrorType): CoreTarget[T] = EitherT.left[Id, ErrorType, T](x)
    def unsafeExtract[T](x: Type[T]): T = x.valueOr({ err => throw new Exception(err.toString) })
  }
}

package object codegen {
  type CodegenApplicationSP[T] = Coproduct[ProtocolSupportTerm, ServerTerm, T]
  type CodegenApplicationMSP[T] = Coproduct[ModelProtocolTerm, CodegenApplicationSP, T]
  type CodegenApplicationEMSP[T] = Coproduct[EnumProtocolTerm, CodegenApplicationMSP, T]
  type CodegenApplicationCEMSP[T] = Coproduct[ClientTerm, CodegenApplicationEMSP, T]
  type CodegenApplicationACEMSP[T] = Coproduct[AliasProtocolTerm, CodegenApplicationCEMSP, T]
  type CodegenApplicationACEMSSP[T] = Coproduct[ScalaTerm, CodegenApplicationACEMSP, T]
  type CodegenApplication[T] = CodegenApplicationACEMSSP[T]

  type Target[A] = Target.Type[A]
  type CoreTarget[A] = CoreTarget.Type[A]
}
