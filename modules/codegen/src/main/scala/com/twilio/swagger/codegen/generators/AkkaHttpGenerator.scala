package com.twilio.swagger.codegen
package generators

import _root_.io.swagger.models._
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.instances.all._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.twilio.swagger.codegen.extract.ScalaPackage
import com.twilio.swagger.codegen.terms.RouteMeta
import com.twilio.swagger.codegen.terms.framework._
import java.util.Locale
import scala.collection.JavaConverters._
import scala.meta._

object AkkaHttpGenerator {
  object FrameworkInterp extends FunctionK[FrameworkTerm, Target] {
    def apply[T](term: FrameworkTerm[T]): Target[T] = term match {
      case GetFrameworkImports(tracing) => Target.pure(List(
        q"import akka.http.scaladsl.model._"
      , q"import akka.http.scaladsl.model.headers.RawHeader"
      , q"import akka.http.scaladsl.unmarshalling.{Unmarshal, Unmarshaller, FromEntityUnmarshaller}"
      , q"import akka.http.scaladsl.marshalling.{Marshal, Marshaller, Marshalling, ToEntityMarshaller, ToResponseMarshaller}"
      , q"import akka.http.scaladsl.server.Directives._"
      , q"import akka.http.scaladsl.server.{Directive, Directive0, Route}"
      , q"import akka.http.scaladsl.util.FastFuture"
      , q"import akka.stream.Materializer"
      , q"import akka.stream.scaladsl.Source"
      , q"import akka.util.ByteString"
      , q"import cats.data.EitherT"
      , q"import scala.concurrent.{ExecutionContext, Future}"
      , q"import scala.language.implicitConversions"
      ))
    }
  }
}
