package com.twilio.guardrail
package generators

import io.swagger.models._
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.instances.all._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.twilio.guardrail.extract.ScalaPackage
import com.twilio.guardrail.terms.framework._
import java.util.Locale
import scala.collection.JavaConverters._
import scala.meta._
import com.twilio.guardrail.languages.ScalaLanguage

object EndpointsGenerator {
  object FrameworkInterp extends FunctionK[FrameworkTerm[ScalaLanguage, ?], Target] {
    def apply[T](term: FrameworkTerm[ScalaLanguage, T]): Target[T] = term match {
      case FileType(format)             => ???
      case ObjectType(format)           => ???
      case GetFrameworkImports(tracing) => ???
      case GetFrameworkImplicits()      => ???
      case LookupStatusCode(key)        => ???
    }
  }
}
