package com.twilio.guardrail.generators.Java

import cats.data.NonEmptyList
import cats.~>
import com.github.javaparser.JavaParser
import com.github.javaparser.ast.Modifier
import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, MethodDeclaration}
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.protocol.terms.client._
import com.twilio.guardrail.terms.RouteMeta
import com.twilio.guardrail.{RenderedClientOperation, StaticDefns, Target}
import java.util
import scala.util.Random

object AsyncHttpClientClientGenerator {
  object ClientTermInterp extends (ClientTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ClientTerm[JavaLanguage, T]): Target[T] = term match {
      case GenerateClientOperation(_, _ @RouteMeta(pathStr, httpMethod, operation), methodName, tracing, parameters, responses) =>
        val dummyMethod = new MethodDeclaration(util.EnumSet.of(Modifier.PUBLIC), JavaParser.parseType("void"), new Random().alphanumeric.take(20).mkString)
        Target.pure(RenderedClientOperation[JavaLanguage](dummyMethod, List.empty))

      case GetImports(tracing) =>
        Target.pure(List.empty)

      case GetExtraImports(tracing) =>
        Target.pure(List.empty)

      case ClientClsArgs(tracingName, serverUrls, tracing) =>
        Target.pure(List.empty)

      case GenerateResponseDefinitions(operationId, responses, protocolElems) =>
        Target.pure(List.empty)

      case BuildStaticDefns(clientName, tracingName, serverUrls, ctorArgs, tracing) =>
        Target.pure(
          StaticDefns[JavaLanguage](
            className = clientName,
            extraImports = List.empty,
            definitions = List.empty
          )
        )

      case BuildClient(clientName, tracingName, serverUrls, basePath, ctorArgs, clientCalls, supportDefinitions, tracing) =>
        Target.pure(NonEmptyList(Right(new ClassOrInterfaceDeclaration(util.EnumSet.of(Modifier.PUBLIC), false, s"${clientName}Client")), Nil))
    }
  }
}
