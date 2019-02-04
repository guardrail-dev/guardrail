package com.twilio.guardrail
package generators

import _root_.io.swagger.v3.oas.models._
import cats.implicits._
import cats.syntax.either._
import cats.~>
import com.twilio.guardrail.extract.ScalaPackage
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.shims._
import com.twilio.guardrail.terms._
import scala.collection.JavaConverters._

object SwaggerGenerator {
  object SwaggerInterp extends (SwaggerTerm[ScalaLanguage, ?] ~> Target) {
    def splitOperationParts(operationId: String): (List[String], String) = {
      val parts = operationId.split('.')
      (parts.drop(1).toList, parts.last)
    }

    def apply[T](term: SwaggerTerm[ScalaLanguage, T]): Target[T] = term match {
      case ExtractOperations(paths) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"extractOperations(${paths})")
          routes <- paths.traverse {
            case (pathStr, path) =>
              for {

                operationMap <- Target.fromOption(Option(path.readOperationsMap()), "No operations defined")
              } yield {
                operationMap.asScala.toList.map {
                  case (httpMethod, operation) =>
                    RouteMeta(pathStr, httpMethod, operation)
                }
              }
          }
        } yield routes.flatten

      case GetClassName(operation) =>
        for {
          _ <- Target.log.debug("SwaggerGenerator", "swagger")(s"getClassName(${operation})")

          pkg = ScalaPackage(operation)
            .map(_.split('.').toVector)
            .orElse({
              Option(operation.getTags).map { tags =>
                println(s"Warning: Using `tags` to define package membership is deprecated in favor of the `x-scala-package` vendor extension")
                tags.asScala
              }
            })
            .map(_.toList)
          opPkg = Option(operation.getOperationId())
            .map(splitOperationParts)
            .fold(List.empty[String])(_._1)
          className = pkg.map(_ ++ opPkg).getOrElse(opPkg)
        } yield className

      case GetParameterName(parameter) =>
        Target.fromOption(Option(parameter.getName()), "Parameter missing \"name\"")

      case GetBodyParameterSchema(parameter) =>
        Target.fromOption(Option(parameter.getSchema()), "Schema not specified")

      case GetHeaderParameterType(parameter) =>
        Target.fromOption(Option(parameter.getSchema.getType()), s"Missing type")

      case GetPathParameterType(parameter) =>
        Target.fromOption(Option(parameter.getSchema.getType()), s"Missing type")

      case GetQueryParameterType(parameter) =>
        Target.fromOption(Option(parameter.getSchema.getType()), s"Missing type")

      case GetCookieParameterType(parameter) =>
        Target.fromOption(Option(parameter.getSchema.getType()), s"Missing type")

      case GetFormParameterType(parameter) =>
        Target.fromOption(Option(parameter.getSchema.getType()), s"Missing type")

      case GetSerializableParameterType(parameter) =>
        Target.fromOption(Option(parameter.getSchema.getType()), s"Missing type")

      case GetRefParameterRef(parameter) =>
        Target.fromOption(Option(parameter.get$ref).flatMap(_.split("/").lastOption), "$ref not defined")

      case FallbackParameterHandler(parameter) =>
        Target.raiseError(s"Unsure how to handle ${parameter}")

      case GetOperationId(operation) =>
        Target.fromOption(Option(operation.getOperationId())
                            .map(splitOperationParts)
                            .map(_._2),
                          "Missing operationId")

      case GetResponses(operationId, operation) =>
        Target.fromOption(Option(operation.getResponses).map(_.asScala.toMap), s"No responses defined for ${operationId}")

      case GetSimpleRef(ref) =>
        Target.fromOption(Option(ref.get$ref).flatMap(_.split("/").lastOption), s"Unspecified $ref")

      case GetItems(arr) =>
        Target.fromOption(Option(arr.getItems()), "items.type unspecified")

      case GetType(model) =>
        val determinedType = Option(model.getType()).fold("No type definition")(s => s"type: $s")
        val className      = model.getClass.getName
        Target.fromOption(Option(model.getType()),
          s"""|Unknown type for the following structure (${determinedType}, class: ${className}):
              |  ${model.toString().lines.filterNot(_.contains(": null")).mkString("\n  ")}
              |""".stripMargin
        )

      case FallbackPropertyTypeHandler(prop) =>
        val determinedType = Option(prop.getType()).fold("No type definition")(s => s"type: $s")
        val className      = prop.getClass.getName
        Target.raiseError(
          s"""|Unknown type for the following structure (${determinedType}, class: ${className}):
              |  ${prop.toString().lines.filterNot(_.contains(": null")).mkString("\n  ")}
              |""".stripMargin
        )

      case ResolveType(name, protocolElems) =>
        Target.fromOption(protocolElems.find(_.name == name), s"Unable to resolve ${name}")

      case FallbackResolveElems(lazyElems) =>
        Target.raiseError(s"Unable to resolve: ${lazyElems.map(_.name)}")
    }
  }
}
