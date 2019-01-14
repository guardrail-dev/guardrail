package com.twilio.guardrail
package generators

import java.util.Locale

import _root_.io.swagger.models._
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.implicits._
import com.twilio.guardrail.generators.syntax.Scala._
import com.twilio.guardrail.protocol.terms.client._
import com.twilio.guardrail.terms.RouteMeta
import com.twilio.guardrail.languages.ScalaLanguage

import scala.collection.JavaConverters._
import scala.meta._

object EndpointsClientGenerator {
  object ClientTermInterp extends FunctionK[ClientTerm[ScalaLanguage, ?], Target] {
    def apply[T](term: ClientTerm[ScalaLanguage, T]): Target[T] = term match {
      case GenerateClientOperation(className, route @ RouteMeta(pathStr, httpMethod, operation), methodName, tracing, parameters, responses) => ???
      case GetImports(tracing)                                                                                                               => ???
      case GetExtraImports(tracing)                                                                                                          => ???
      case ClientClsArgs(tracingName, schemes, host, tracing)                                                                                => ???
      case GenerateResponseDefinitions(operationId, operation, protocolElems)                                                                => ???
      case BuildStaticDefns(clientName, tracingName, schemes, host, ctorArgs, tracing)                                                       => ???
      case BuildClient(clientName, tracingName, schemes, host, basePath, ctorArgs, clientCalls, supportDefinitions, tracing)                 => ???
    }
  }
}
