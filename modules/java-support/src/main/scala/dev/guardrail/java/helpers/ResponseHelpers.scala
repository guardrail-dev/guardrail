package dev.guardrail.javaext.helpers

import cats.data.NonEmptyList
import cats.syntax.all._
import dev.guardrail.core.{ LiteralRawType, Tracker }
import dev.guardrail.generators.LanguageParameters
import dev.guardrail.languages.LA
import dev.guardrail.terms.{ ApplicationJson, BinaryContent, ContentType, MultipartFormData, OctetStream, Response, TextContent, TextPlain, UrlencodedFormData }
import io.swagger.v3.oas.models.Operation

object ResponseHelpers {
  private val CONSUMES_PRIORITY = NonEmptyList.of(ApplicationJson.empty, TextPlain.empty, OctetStream.empty)
  private val PRODUCES_PRIORITY = NonEmptyList.of(ApplicationJson.empty, TextPlain.empty, OctetStream.empty)

  def getBestConsumes[L <: LA](operation: Tracker[Operation], contentTypes: List[ContentType], parameters: LanguageParameters[L]): Option[ContentType] =
    if (parameters.formParams.nonEmpty) {
      if (parameters.formParams.exists(_.isFile) || contentTypes.exists(ContentType.isSubtypeOf[MultipartFormData])) {
        Some(MultipartFormData.empty)
      } else {
        Some(UrlencodedFormData.empty)
      }
    } else {
      parameters.bodyParams.map { bodyParam =>
        CONSUMES_PRIORITY
          .collectFirstSome(ct => contentTypes.find(_ == ct))
          .orElse(contentTypes.collectFirst { case tc: TextContent => tc })
          .orElse(contentTypes.collectFirst { case bc: BinaryContent => bc })
          .getOrElse {
            val fallback =
              bodyParam.rawType match {
                case LiteralRawType(Some("object"), _) => ApplicationJson.empty
                case _                                 => TextPlain.empty
              }
            println(s"WARNING: no supported body param type at ${operation.showHistory}; falling back to $fallback")
            fallback
          }
      }
    }

  def getBestProduces[L <: LA](
      operation: Tracker[Operation],
      contentTypes: List[ContentType],
      response: Response[L],
      fallbackIsString: L#Type => Boolean
  ): Option[ContentType] =
    response.value
      .map(_._2)
      .flatMap { valueType =>
        PRODUCES_PRIORITY
          .collectFirstSome(ct => contentTypes.find(_ == ct))
          .orElse(contentTypes.collectFirst { case tc: TextContent => tc })
          .orElse(contentTypes.collectFirst { case bc: BinaryContent => bc })
          .orElse {
            val fallback    = if (fallbackIsString(valueType)) TextPlain.empty else ApplicationJson.empty
            val operationId = operation.downField("operationId", _.getOperationId).unwrapTracker.getOrElse("<no operationId>")
            println(
              s"WARNING: no supported body param type for operation '$operationId', response code ${response.statusCode}; falling back to ${fallback.value} (${operation.showHistory})"
            )
            Option(fallback)
          }
      }

  def removeEmpty(s: String): Option[String]       = Option(s.trim).filter(_.nonEmpty)
  def splitPathComponents(s: String): List[String] = s.split("/").flatMap(removeEmpty).toList

  def findPathPrefix(routePaths: List[String]): List[String] = {
    def getHeads(sss: List[List[String]]): (List[Option[String]], List[List[String]]) =
      (sss.map(_.headOption), sss.map(_.drop(1)))

    def checkMatch(matching: List[String], headsToCheck: List[Option[String]], restOfHeads: List[List[String]]): List[String] =
      headsToCheck match {
        case Nil => matching
        case x :: xs =>
          x.fold(matching) { first =>
            if (xs.forall(_.contains(first))) {
              val (nextHeads, nextRest) = getHeads(restOfHeads)
              checkMatch(matching :+ first, nextHeads, nextRest)
            } else {
              matching
            }
          }
      }

    val splitRoutePaths             = routePaths.map(splitPathComponents)
    val (initialHeads, initialRest) = getHeads(splitRoutePaths)
    checkMatch(List.empty, initialHeads, initialRest)
  }
}
