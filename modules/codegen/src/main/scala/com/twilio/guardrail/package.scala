package com.twilio

import cats.{ Applicative, Id }
import cats.data.{ EitherK, IndexedStateT }
import cats.implicits._
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.protocol.terms.client.ClientTerm
import com.twilio.guardrail.protocol.terms.protocol._
import com.twilio.guardrail.protocol.terms.server.ServerTerm
import com.twilio.guardrail.terms.framework.FrameworkTerm
import com.twilio.guardrail.terms.{ ScalaTerm, SwaggerTerm }

import com.twilio.swagger.core.StructuredLogger

package guardrail {
  case class CodegenDefinitions[L <: LA](
      clients: List[Client[L]],
      servers: List[Server[L]],
      supportDefinitions: List[SupportDefinition[L]],
      frameworksImplicits: Option[(L#TermName, L#ObjectDefinition)]
  )

  trait LogAbstraction {
    type F[_]
    implicit def A: Applicative[F]
    def pushLogger(value: StructuredLogger): F[Unit]
    object log {
      def push(name: String): F[Unit] = pushLogger(StructuredLogger.push(name))
      def pop: F[Unit]                = pushLogger(StructuredLogger.pop)
      def function[A](name: String): F[A] => F[A] = { func =>
        (push(name) *> func) <* pop
      }
      def debug(message: String): F[Unit]   = pushLogger(StructuredLogger.debug(message))
      def info(message: String): F[Unit]    = pushLogger(StructuredLogger.info(message))
      def warning(message: String): F[Unit] = pushLogger(StructuredLogger.warning(message))
      def error(message: String): F[Unit]   = pushLogger(StructuredLogger.error(message))
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

  type Logger[T] = IndexedStateT[Id, StructuredLogger, StructuredLogger, T]
}
