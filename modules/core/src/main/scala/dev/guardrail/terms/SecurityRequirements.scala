package dev.guardrail.terms

import cats.Order
import cats.data.{ NonEmptyList, NonEmptyMap }
import cats.implicits._
import io.swagger.v3.oas.models.security.SecurityRequirement
import scala.jdk.CollectionConverters._
import scala.collection.immutable.TreeMap

import dev.guardrail.terms.SecurityRequirements.SecurityScopes
import dev.guardrail.core.Tracker
import dev.guardrail.core.implicits._

object SecurityRequirements {
  type SecurityScopes = List[String]

  sealed trait Location
  case object Global extends Location
  case object Local  extends Location

  def apply(requirements: NonEmptyList[Tracker[SecurityRequirement]], location: Location): Option[SecurityRequirements] = {
    implicit val strOrder = Order.fromComparable[String]
    val optional          = requirements.exists(_.unwrapTracker.isEmpty())

    for {
      convertedReqs <- NonEmptyList.fromList(
        requirements.toList
          .flatMap { requirement =>
            val nameAndScopes: Tracker[List[(String, List[String])]] = requirement.forceConvince.map(_.map(_.asScala.toList).value)
            nameAndScopes.map(reqs => NonEmptyMap.fromMap(TreeMap(reqs: _*))).indexedDistribute
          }
      )
    } yield SecurityRequirements(convertedReqs, optional, location)
  }
}
case class SecurityRequirements(
    requirements: NonEmptyList[Tracker[NonEmptyMap[String, SecurityScopes]]],
    optional: Boolean,
    location: SecurityRequirements.Location
)
