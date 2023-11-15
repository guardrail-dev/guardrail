package dev.guardrail.terms.protocol

import cats.Monad
import cats.implicits._
import io.swagger.v3.oas.models.media.Schema

import dev.guardrail.core.Tracker
import dev.guardrail.languages.LA
import dev.guardrail.terms.{ LanguageTerms, OpenAPITerms }

case class Discriminator[L <: LA](propertyName: String, mapping: Map[String, StrictProtocolElems[L]])

object Discriminator {
  def fromSchema[L <: LA, F[_]: Monad](schema: Tracker[Schema[_]])(implicit Sc: LanguageTerms[L, F], Sw: OpenAPITerms[L, F]): F[Option[Discriminator[L]]] =
    Sw.log.function("Discriminator.fromSchema") {
      import Sc._
      schema
        .downField("discriminator", _.getDiscriminator)
        .indexedDistribute
        .flatMap(x => x.downField("propertyName", _.getPropertyName()).indexedDistribute.map((x, _)))
        .traverse { case (x, Tracker(_, propertyName)) =>
          val possibleMappings = x
            .downField("mapping", _.getMapping())
            .indexedDistribute
            .value
            .flatMap {
              case (k, s) if s.unwrapTracker.startsWith("#/") => s.map(_.split("/").lastOption.filter(_.nonEmpty)).indexedDistribute.map((k, _))
              case (k, s)                                     => Option((k, s))
            }
            .toList
          for {
            mapping <- possibleMappings.flatTraverse { case (key, name) =>
              parseType(name).map(_.map(tpe => (key, RandomType(name.unwrapTracker, tpe))).toList)
            }
          } yield Discriminator[L](propertyName, mapping.toMap)
        }
    }
}
