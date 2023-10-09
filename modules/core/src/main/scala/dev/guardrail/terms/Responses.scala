package dev.guardrail.terms

import cats.syntax.all._
import dev.guardrail.core
import dev.guardrail.core.Tracker
import dev.guardrail.core.resolvers.ModelResolver
import dev.guardrail.languages.LA
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.monadForFrameworkTerms
import dev.guardrail.terms.protocol.StrictProtocolElems
import io.swagger.v3.oas.models.Operation
import io.swagger.v3.oas.models.Components

class Response[L <: LA](
    val statusCodeName: L#TermName,
    val statusCode: Int,
    val value: Option[(ContentType, L#Type, Option[L#Term])],
    val headers: Headers[L]
) {
  override def toString: String = s"Response($statusCodeName, $statusCode, $value, $headers)"
}
object Response {
  def unapply[L <: LA](value: Response[L]): Some[(L#TermName, Option[L#Type], Headers[L])] =
    Some((value.statusCodeName, value.value.map(_._2), value.headers))
}

class Responses[L <: LA](val value: List[Response[L]]) {
  override def toString: String = s"Responses($value)"
}
object Responses {
  def getResponses[L <: LA, F[_]](
      operationId: String,
      operation: Tracker[Operation],
      protocolElems: List[StrictProtocolElems[L]],
      components: Tracker[Option[Components]]
  )(implicit
      Fw: FrameworkTerms[L, F],
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
        .traverse { case (key, resp) =>
          for {
            httpCode <- lookupStatusCode(key)
            (statusCode, statusCodeName) = httpCode
            valueTypes <- (for {
              (rawContentType, content) <- resp.downField("content", _.getContent()).indexedDistribute.value
              contentType               <- ContentType.unapply(rawContentType).toList
              schema                    <- content.downField("schema", _.getSchema()).indexedDistribute.toList
            } yield (contentType, schema)).traverse { case (contentType, prop) =>
              for {
                meta     <- ModelResolver.propMeta[L, F](prop, components)
                resolved <- core.ResolvedType.resolve[L, F](meta, protocolElems)
                core.Resolved(baseType, _, baseDefaultValue, _) = resolved // TODO: ReifiedRawType is just dropped, should it be considered?
              } yield (contentType, baseType, baseDefaultValue)
            }
            headers <- resp.downField("headers", _.getHeaders()).indexedDistribute.value.traverse { case (name, header) =>
              for {
                argName  <- formatMethodArgName(s"${name}Header")
                termName <- pureTermName(argName)
                typeName <- pureTypeName("String").flatMap(widenTypeName)
                required = header.downField("required", _.getRequired).unwrapTracker.getOrElse(false)
                resultType <- if (required) typeName.pure[F] else liftOptionalType(typeName)
              } yield new Header(name, required, resultType, termName)
            }
          } yield new Response[L](statusCodeName, statusCode, valueTypes.headOption, new Headers(headers)) // FIXME: headOption
        }
    } yield new Responses[L](instances.toList)
  }

}
