package dev.guardrail.terms

import cats.Monad
import cats.data.State
import cats.implicits._
import io.swagger.v3.oas.models.{ Components, Operation }
import io.swagger.v3.oas.models.PathItem.HttpMethod
import io.swagger.v3.oas.models.media._
import io.swagger.v3.oas.models.parameters.Parameter
import scala.jdk.CollectionConverters._

import dev.guardrail._
import dev.guardrail.core.implicits._
import dev.guardrail.core.{ Mappish, Tracker }
import dev.guardrail.generators.syntax._
import dev.guardrail.generators.{ LanguageParameter, LanguageParameters }
import dev.guardrail.languages.LA
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.protocol.StrictProtocolElems

@SuppressWarnings(Array("org.wartremover.warts.Null"))
case class RouteMeta(path: Tracker[String], method: HttpMethod, operation: Tracker[Operation], securityRequirements: Option[SecurityRequirements]) {
  override def toString(): String =
    s"RouteMeta(${path.unwrapTracker}, $method, ${operation.unwrapTracker.showNotNull} (${operation.showHistory}), $securityRequirements)"
  object MediaType {
    def unapply(value: MediaType): Some[(Option[Schema[_]], Option[Map[String, Encoding]], Option[Map[String, Object]])] = {
      val schema: Option[Schema[_]] = Option(value.getSchema)
      val encoding                  = Option(value.getEncoding()).map(_.asScala.toMap)
      val extensions                = Option(value.getExtensions()).map(_.asScala.toMap)
      Some((schema, encoding, extensions))
    }
  }

  private def extractPrimitiveFromRequestBody(
      fields: Mappish[List, String, Tracker[MediaType]],
      required: Tracker[Option[Boolean]]
  ): Option[Tracker[Parameter]] = {
    val formContentTypes = Set[ContentType](MultipartFormData.empty, UrlencodedFormData.empty)
    // FIXME: Just taking the head here isn't super great
    def unifyEntries: List[(String, Tracker[MediaType])] => Option[Tracker[Schema[_]]] =
      _.flatMap {
        case (ContentType(contentType), mediaType) =>
          mediaType
            .refine[Option[Tracker[Schema[_]]]] { case mt @ MediaType(None, _, _) if contentType == TextPlain => mt }(x =>
              Option(Tracker.cloneHistory(x, new StringSchema()))
            )
            .orRefine { case mt @ MediaType(schema, _, _) if !formContentTypes.contains(contentType) => schema }(_.indexedDistribute)
            .orRefineFallback(_ => None)
        case _ => None
      }.headOption
    for {
      schema <- unifyEntries(fields.value)
      tpe    <- schema.downField("type", _.getType()).indexedDistribute // TODO: Why is this here?
    } yield {

      val p = new Parameter

      if (schema.unwrapTracker.getFormat == "binary") {
        schema.unwrapTracker.setType("file")
        schema.unwrapTracker.setFormat(null)
      }

      p.setIn("body")
      p.setName("body")
      p.setSchema(schema.unwrapTracker)
      required.unwrapTracker.foreach(x => p.setRequired(x))

      schema
        .downField[Option[java.util.Map[String, Object]]]("extensions", _.getExtensions())
        .unwrapTracker
        .foreach(x => p.setExtensions(x))
      Tracker.cloneHistory(schema, p)
    }
  }

  // https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.0.md#fixed-fields-8
  // RequestBody can represent either a RequestBody object or $ref.
  // (these are both represented in the same RequestBody class)
  private def extractRefParamFromRequestBody(
      ref: Tracker[Option[String]],
      fields: Mappish[List, String, dev.guardrail.core.Tracker[io.swagger.v3.oas.models.media.MediaType]],
      extensions: Tracker[Option[java.util.Map[String, Object]]],
      required: Tracker[Option[Boolean]]
  ): Option[Tracker[Parameter]] = {
    val content = for {
      (_, mt) <- fields.value.headOption
      schema  <- mt.downField("schema", _.getSchema()).indexedCosequence
      ref     <- schema.downField("$ref", _.get$ref()).indexedCosequence
    } yield {
      val p = new Parameter

      if (schema.unwrapTracker.getFormat == "binary") {
        schema.unwrapTracker.setType("file")
        schema.unwrapTracker.setFormat(null)
      }

      p.setIn("body")
      p.setName("body")
      p.set$ref(ref.unwrapTracker)

      required.unwrapTracker.foreach(x => p.setRequired(x))

      extensions.unwrapTracker.foreach(x => p.setExtensions(x))
      Tracker.cloneHistory(ref, p)
    }

    val refParam = ref.cotraverse { x =>
      val p = new Parameter

      p.setIn("body")
      p.setName("body")
      p.set$ref(x.unwrapTracker)

      required.unwrapTracker.foreach(x => p.setRequired(x))

      extensions.unwrapTracker.foreach(x => p.setExtensions(x))

      Tracker.cloneHistory(ref, p)
    }

    content.orElse(refParam)
  }

  /** Temporary hack method to adapt to open-api v3 spec */
  private def extractParamsFromRequestBody(
      fields: Mappish[List, String, dev.guardrail.core.Tracker[io.swagger.v3.oas.models.media.MediaType]],
      required: Tracker[Option[Boolean]]
  ): List[Tracker[Parameter]] = {
    type HashCode            = Int
    type Count               = Int
    type ParameterCountState = (Count, Map[HashCode, Count])
    val contentTypes: List[ContentType] = fields.value.collect { case (ContentType(ct), _) => ct }
    val ((maxCount, instances), ps) = fields.value
      .flatMap { case (_, mt) =>
        for {
          mtSchema <- mt.downField("schema", _.getSchema()).indexedCosequence.toList
          requiredFields = mtSchema.downField("required", _.getRequired).unwrapTracker.toSet
          (name, schema) <- mtSchema.downField("properties", _.getProperties()).indexedCosequence.value
        } yield {
          val p = new Parameter

          if (schema.downField("format", _.getFormat).unwrapTracker.contains("binary")) {
            schema.unwrapTracker.setType("file")
            schema.unwrapTracker.setFormat(null)
          }

          p.setName(name)
          p.setIn("formData")
          p.setSchema(schema.unwrapTracker)

          val isRequired: Boolean = if (requiredFields.nonEmpty) {
            requiredFields.contains(name)
          } else {
            required.unwrapTracker.getOrElse(false)
          }

          p.setRequired(isRequired)
          p.setExtensions(schema.unwrapTracker.getExtensions)

          if (
            schema.downField("type", _.getType()).indexedCosequence.exists(_.unwrapTracker == "file") && contentTypes.exists(
              ContentType.isSubtypeOf[UrlencodedFormData]
            )
          ) {
            p.setRequired(false)
          }

          Tracker.cloneHistory(mt, p)
        }
      }
      .traverse[State[ParameterCountState, *], Tracker[Parameter]] { p =>
        State[ParameterCountState, Tracker[Parameter]] { case (maxCount, instances) =>
          val updated = instances.updated(p.unwrapTracker.hashCode, instances.getOrElse(p.unwrapTracker.hashCode, 0) + 1)
          ((Math.max(maxCount, updated.values.foldLeft(0)(Math.max)), updated), p)
        }
      }
      .runEmpty
      .value

    ps.distinctBy(_.unwrapTracker).map { p =>
      instances.get(p.hashCode).foreach { count =>
        // FIXME: Regardless of what the specification says, if a parameter does not appear across all media types, mark it as optional
        if (count != maxCount) {
          p.unwrapTracker.setRequired(false)
        }
      }
      p
    }
  }

  private val parameters: List[Tracker[Parameter]] =
    operation.downField("parameters", _.getParameters()).indexedDistribute ++
      operation
        .downField("requestBody", _.getRequestBody())
        .map(_.toList)
        .flatExtract { requestBody =>
          val content  = requestBody.downField("content", _.getContent()).indexedCosequence
          val required = requestBody.downField("required", _.getRequired())

          val refParam = extractRefParamFromRequestBody(
            requestBody.downField("$ref", _.get$ref()),
            content,
            requestBody.downField[Option[java.util.Map[String, Object]]]("extensions", _.getExtensions()),
            required
          )
          val params    = extractParamsFromRequestBody(content, required)
          val primitive = extractPrimitiveFromRequestBody(content, required)
          refParam.toList ++ params ++ primitive.toList
        }

  def getParameters[L <: LA, F[_]: Monad](
      components: Tracker[Option[Components]],
      protocolElems: List[StrictProtocolElems[L]]
  )(implicit Fw: FrameworkTerms[L, F], Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: SwaggerTerms[L, F]): F[LanguageParameters[L]] =
    for {
      a <- LanguageParameter.fromParameters[L, F](protocolElems, components).apply(parameters)
    } yield new LanguageParameters[L](a)
}
