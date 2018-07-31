package com.twilio.guardrail.generators.circe.protocol

import cats.data.EitherT
import cats.~>
import com.twilio.guardrail.{ Settings, Target }
import com.twilio.guardrail.protocol.terms.protocol._
import com.twilio.guardrail.swagger.SwaggerUtil
import io.swagger.models._

import scala.meta.Type
import scala.meta._
import cats.implicits._
import com.twilio.guardrail.generators.GeneratorSettings
import scala.collection.JavaConverters._

object ProtocolSupportTermInterp extends (ProtocolSupportTerm ~> Target) {
  def apply[T](term: ProtocolSupportTerm[T]): Target[T] = term match {
    case ExtractConcreteTypes(definitions) =>
      extractConcreteTypes(definitions)

    case ProtocolImports() =>
      Target.pure(
        List(
          q"import io.circe._",
          q"import io.circe.syntax._",
          q"import io.circe.generic.semiauto._"
        )
      )

    case PackageObjectImports() =>
      Target.pure(
        List(
          q"import java.time._",
          q"import io.circe.java8.{ time => j8time }"
        )
      )

    case PackageObjectContents() =>
      Target.pure(q"""
          val decodeLong = implicitly[Decoder[Long]]

          implicit def decodeInstant: Decoder[Instant] = j8time.decodeInstant.or(decodeLong.map(Instant.ofEpochMilli))
          implicit def decodeLocalDate: Decoder[LocalDate] = j8time.decodeLocalDateDefault.or(decodeInstant.map(_.atZone(ZoneOffset.UTC).toLocalDate))
          implicit def decodeOffsetDateTime: Decoder[OffsetDateTime] = j8time.decodeOffsetDateTimeDefault.or(decodeInstant.map(_.atZone(ZoneOffset.UTC).toOffsetDateTime))

          // Unused
          //implicit def decodeLocalDateTime: Decoder[Instant] = ???
          //implicit def decodeLocalTime: Decoder[Instant] = ???
          // implicit def decodeZonedDateTime: Decoder[Instant] = ???

          // Mirror
          implicit val encodeInstant = j8time.encodeInstant
          implicit val encodeLocalDateDefault = j8time.encodeLocalDateDefault
          implicit val encodeLocalDateTimeDefault = j8time.encodeLocalDateTimeDefault
          implicit val encodeLocalTimeDefault = j8time.encodeLocalTimeDefault
          implicit val encodeOffsetDateTimeDefault = j8time.encodeOffsetDateTimeDefault
          implicit val encodeZonedDateTimeDefault = j8time.encodeZonedDateTimeDefault
        """.stats)
  }

  // what does it do?
  private def extractConcreteTypes[T](definitions: List[(String, Model)]): EitherT[Settings, String, List[PropMeta]] =
    Target.getGeneratorSettings.flatMap { implicit gs: GeneratorSettings =>
      for {
        entries <- definitions.traverse {
          case (clsName, impl: ModelImpl) if Option(impl.getProperties).isDefined || Option(impl.getEnum).isDefined =>
            val resolvedType: SwaggerUtil.ResolvedType = SwaggerUtil.Resolved(Type.Name(clsName), None, None)
            Target.pure((clsName, resolvedType))

          case (clsName, comp: ComposedModel) =>
            //TODO start off here!!!!!
            val parentSimpleRef: Option[String] = comp.getInterfaces.asScala.headOption.map(_.getSimpleRef)

            val parentTerm = parentSimpleRef.map(n => Term.Name(n))

            val resolvedType: SwaggerUtil.ResolvedType = SwaggerUtil.Resolved(Type.Name(clsName), parentTerm, None)
            Target.pure((clsName, resolvedType))

          case (clsName, definition) =>
            SwaggerUtil
              .modelMetaType(definition)
              .map(x => (clsName, x))
        }
        result <- SwaggerUtil.ResolvedType.resolve_(entries)
      } yield
        result.map {
          case (clsName, SwaggerUtil.Resolved(tpe, parent, _)) =>
            PropMeta(clsName, tpe)
        }
    }
}
