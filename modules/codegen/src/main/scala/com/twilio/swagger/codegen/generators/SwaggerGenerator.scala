package com.twilio.swagger.codegen
package generators

import _root_.io.swagger.models._
import cats.implicits._
import cats.syntax.either._
import cats.~>
import com.twilio.swagger.codegen.extract.ScalaPackage
import com.twilio.swagger.codegen.terms._
import scala.collection.JavaConverters._
import scala.meta._

object SwaggerGenerator {
  object SwaggerInterp extends (SwaggerTerm ~> Target) {
    def apply[T](term: SwaggerTerm[T]): Target[T] = ???
  }
}
