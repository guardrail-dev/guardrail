package dev.guardrail.terms

import cats.Order
import cats.data.{ NonEmptyList, NonEmptyMap }
import cats.implicits._
import io.swagger.v3.oas.models.security.SecurityRequirement
import scala.collection.JavaConverters._
import scala.collection.immutable.TreeMap

import dev.guardrail.terms.SecurityRequirements.SecurityScopes

object SecurityRequirements {
  type SecurityScopes = List[String]

  sealed trait Location
  case object Global extends Location
  case object Local  extends Location

  def apply(requirements: NonEmptyList[SecurityRequirement], optionalSchemes: List[String], location: Location): Option[SecurityRequirements] = {
    implicit val strOrder = Order.fromComparable[String]
    for {
      convertedReqs <- NonEmptyList.fromList(
        requirements.toList
          .flatMap(
            req =>
              NonEmptyMap.fromMap(
                TreeMap(req.asScala.mapValues(_.asScala.toList).toSeq: _*)
              )
          )
      )
    } yield SecurityRequirements(convertedReqs, optionalSchemes, location)
  }
}
case class SecurityRequirements(
    requirements: NonEmptyList[NonEmptyMap[String, SecurityScopes]],
    optionalSchemes: List[String],
    location: SecurityRequirements.Location
)

