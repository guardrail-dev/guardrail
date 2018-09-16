package com.twilio.guardrail
package generators

import _root_.io.swagger.models._
import cats.implicits._
import cats.syntax.either._
import cats.~>
import com.twilio.guardrail.extract.ScalaPackage
import com.twilio.guardrail.terms._
import scala.collection.JavaConverters._
import scala.meta._

object SwaggerGenerator {
  object SwaggerInterp extends (SwaggerTerm ~> Target) {
    def splitOperationParts(operationId: String): (List[String], String) = {
      val parts = operationId.split('.')
      (parts.drop(1).toList, parts.last)
    }

    def apply[T](term: SwaggerTerm[T]): Target[T] = term match {
      case ExtractOperations(paths) =>
        paths
          .map({
            case (pathStr, path) =>
              Target
                .fromOption(Option(path.getOperationMap), "No operations defined")
                .map { operationMap =>
                  operationMap.asScala.map {
                    case (httpMethod, operation) =>
                      RouteMeta(pathStr, httpMethod, operation)
                  }
                }
          })
          .sequence
          .map(_.flatten)

      case GetClassName(operation) =>
        for {
          _ <- Target.log.debug("AkkaHttpClientGenerator", "client")(s"getClassName(${operation})")

          pkg = ScalaPackage(operation)
            .map(_.split('.').toVector)
            .orElse({
              Option(operation.getTags).map { tags =>
                println(
                  s"Warning: Using `tags` to define package membership is deprecated in favor of the `x-scala-package` vendor extension"
                )
                tags.asScala
              }
            })
            .map(_.toList)
          opPkg = Option(operation.getOperationId())
            .map(splitOperationParts)
            .fold(List.empty[String])(_._1)
          className = pkg.map(_ ++ opPkg).getOrElse(opPkg)
        } yield className
    }
  }
}
