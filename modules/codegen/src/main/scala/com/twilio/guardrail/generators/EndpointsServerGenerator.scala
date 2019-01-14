package com.twilio.guardrail
package generators

import _root_.io.swagger.models.{ HttpMethod, Operation }
import cats.arrow.FunctionK
import cats.data.{ NonEmptyList, OptionT }
import cats.instances.all._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.twilio.guardrail.SwaggerUtil
import com.twilio.guardrail.extract.{ ScalaPackage, ScalaTracingLabel, ServerRawResponse }
import com.twilio.guardrail.generators.syntax.Scala._
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.protocol.terms.server._
import com.twilio.guardrail.terms.RouteMeta
import scala.collection.JavaConverters._
import scala.meta._

object EndpointsServerGenerator {
  object ServerTermInterp extends FunctionK[ServerTerm[ScalaLanguage, ?], Target] {
    def apply[T](term: ServerTerm[ScalaLanguage, T]): Target[T] = term match {
      case GenerateResponseDefinitions(operationId, responses, protocolElems)                                                    => ???
      case BuildTracingFields(operation, resourceName, tracing)                                                                  => ???
      case GenerateRoutes(resourceName, basePath, routes, protocolElems)                                                         => ???
      case RenderHandler(handlerName, methodSigs, handlerDefinitions)                                                            => ???
      case GetExtraRouteParams(tracing)                                                                                          => ???
      case RenderClass(resourceName, handlerName, combinedRouteTerms, extraRouteParams, responseDefinitions, supportDefinitions) => ???
      case GetExtraImports(tracing)                                                                                              => ???
    }
  }
}
