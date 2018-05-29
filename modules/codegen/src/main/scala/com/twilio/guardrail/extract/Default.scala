package com.twilio.guardrail.extract

import io.swagger.models.parameters._
import io.swagger.models.properties._

object Default {
  trait GetDefault[F] {
    def extract[T](from: F)(implicit T: Extractable[T]): Option[T]
  }

  object GetDefault {
    def build[F](f: F => Any): GetDefault[F] = {
      new GetDefault[F] {
        def extract[T](from: F)(implicit T: Extractable[T]): Option[T] = {
          T.extract(f(from)).toOption
        }
      }
    }

    implicit val getDefaultBooleanProperty: GetDefault[BooleanProperty] = build[BooleanProperty](_.getDefault)
    implicit val getDefaultDoubleProperty: GetDefault[DoubleProperty] = build[DoubleProperty](_.getDefault)
    implicit val getDefaultFloatProperty: GetDefault[FloatProperty] = build[FloatProperty](_.getDefault)
    implicit val getDefaultIntegerProperty: GetDefault[IntegerProperty] = build[IntegerProperty](_.getDefault)
    implicit val getDefaultLongProperty: GetDefault[LongProperty] = build[LongProperty](_.getDefault)
    implicit val getDefaultStringProperty: GetDefault[StringProperty] = build[StringProperty](_.getDefault)

    implicit val getDefaultCookieParameterParameter: GetDefault[CookieParameter] = build[CookieParameter](_.getDefault)
    implicit val getDefaultFormParameterParameter: GetDefault[FormParameter] = build[FormParameter](_.getDefault)
    implicit val getDefaultHeaderParameterParameter: GetDefault[HeaderParameter] = build[HeaderParameter](_.getDefault)
    implicit val getDefaultPathParameterParameter: GetDefault[PathParameter] = build[PathParameter](_.getDefault)
    implicit val getDefaultQueryParameterParameter: GetDefault[QueryParameter] = build[QueryParameter](_.getDefault)
  }

  case class DefaultAdapter[F](from: F) extends AnyVal {
    def extract[T: Extractable](implicit F: GetDefault[F]): Option[T] =
      F.extract(from)
  }

  def apply[F](from: F): DefaultAdapter[F] = DefaultAdapter(from)
}
