package com.twilio.guardrail.protocol.terms

import cats.free.Free
import cats.instances.list._
import cats.syntax.traverse._
import com.twilio.guardrail.{ StrictProtocolElems, SwaggerUtil }
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.terms.{ ScalaTerms, SwaggerTerms }
import com.twilio.guardrail.terms.framework.FrameworkTerms
import io.swagger.v3.oas.models.Operation
import io.swagger.v3.oas.models.media.Schema
import scala.collection.JavaConverters._

class Response[L <: LA](val statusCodeName: L#TermName, val statusCode: Int, val value: Option[(L#Type, Option[L#Term])]) {
  override def toString: String = s"Response($statusCodeName, $statusCode, $value)"
}
object Response {
  def unapply[L <: LA](value: Response[L]): Option[(L#TermName, Option[L#Type])] = Some((value.statusCodeName, value.value.map(_._1)))
}

class Responses[L <: LA](val value: List[Response[L]]) {
  override def toString: String = s"Responses($value)"
}
object Responses {
  def getResponses[L <: LA, F[_]](operationId: String, operation: Operation, protocolElems: List[StrictProtocolElems[L]])(
      implicit Fw: FrameworkTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, Responses[L]] = Sw.log.function("getResponses") {
    import Fw._
    for {
      responses <- Sw.getResponses(operationId, operation)

      instances <- responses
        .foldLeft[List[Free[F, Response[L]]]](List.empty)({
          case (acc, (key, resp)) =>
            acc :+ (for {
              httpCode <- lookupStatusCode(key)
              (statusCode, statusCodeName) = httpCode
              valueTypes <- (for {
                content       <- Option(resp.getContent).toList
                contentValues <- Option(content.values()).toList.flatMap(_.asScala) // FIXME: values() ignores Content-Types in the keys
                schema        <- Option[Schema[_]](contentValues.getSchema).toList
              } yield schema).traverse { prop =>
                for {
                  meta     <- SwaggerUtil.propMeta[L, F](prop)
                  resolved <- SwaggerUtil.ResolvedType.resolve[L, F](meta, protocolElems)
                  SwaggerUtil.Resolved(baseType, _, baseDefaultValue, _, _) = resolved
                } yield (baseType, baseDefaultValue)
              }
            } yield new Response[L](statusCodeName, statusCode, valueTypes.headOption)) // FIXME: headOption
        })
        .sequence
    } yield new Responses[L](instances)
  }

}
