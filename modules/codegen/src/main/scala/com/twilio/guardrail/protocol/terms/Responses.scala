package com.twilio.guardrail.protocol.terms

import cats.implicits._
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, SwaggerTerms }
import com.twilio.guardrail.{ StrictProtocolElems, SwaggerUtil, monadForFrameworkTerms }
import io.swagger.v3.oas.models.Operation
import scala.collection.JavaConverters._

class Response[L <: LA](val statusCodeName: L#TermName, val statusCode: Int, val value: Option[(L#Type, Option[L#Term])], val headers: Headers[L]) {
  override def toString: String = s"Response($statusCodeName, $statusCode, $value, $headers)"
}
object Response {
  def unapply[L <: LA](value: Response[L]): Option[(L#TermName, Option[L#Type], Headers[L])] =
    Some((value.statusCodeName, value.value.map(_._1), value.headers))
}

class Responses[L <: LA](val value: List[Response[L]]) {
  override def toString: String = s"Responses($value)"
}
object Responses {
  def getResponses[L <: LA, F[_]](operationId: String, operation: Tracker[Operation], protocolElems: List[StrictProtocolElems[L]])(
      implicit Fw: FrameworkTerms[L, F],
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): F[Responses[L]] = Sw.log.function("getResponses") {
    import Cl._
    import Fw._
    import Sc._
    for {
      responses <- Sw.getResponses(operationId, operation)

      instances <- responses
        .traverse {
          case (key, resp) =>
            for {
              httpCode <- lookupStatusCode(key)
              (statusCode, statusCodeName) = httpCode
              valueTypes <- (for {
                (_, content) <- resp.downField("content", _.getContent()).indexedDistribute.value
                schema       <- content.downField("schema", _.getSchema()).indexedDistribute.toList
              } yield schema).traverse { prop =>
                for {
                  meta     <- SwaggerUtil.propMeta[L, F](prop)
                  resolved <- SwaggerUtil.ResolvedType.resolve[L, F](meta, protocolElems)
                  SwaggerUtil.Resolved(baseType, _, baseDefaultValue, _, _) = resolved

                } yield (baseType, baseDefaultValue)
              }
              headers <- Option(resp.get.getHeaders).map(_.asScala.toList).getOrElse(List.empty).traverse {
                case (name, header) =>
                  for {
                    argName    <- formatMethodArgName(s"${name}Header")
                    termName   <- pureTermName(argName)
                    typeName   <- pureTypeName("String").flatMap(widenTypeName)
                    resultType <- if (header.getRequired) typeName.pure[F] else liftOptionalType(typeName)
                  } yield new Header(name, header.getRequired, resultType, termName)
              }
            } yield new Response[L](statusCodeName, statusCode, valueTypes.headOption, new Headers(headers)) // FIXME: headOption
        }
    } yield new Responses[L](instances.toList)
  }

}
