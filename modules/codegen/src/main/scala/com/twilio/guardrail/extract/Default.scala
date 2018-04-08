package com.twilio.guardrail.extract

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

    implicit val getDefaultBooleanProperty = build[io.swagger.models.properties.BooleanProperty](_.getDefault)
    implicit val getDefaultDoubleProperty = build[io.swagger.models.properties.DoubleProperty](_.getDefault)
    implicit val getDefaultFloatProperty = build[io.swagger.models.properties.FloatProperty](_.getDefault)
    implicit val getDefaultIntegerProperty = build[io.swagger.models.properties.IntegerProperty](_.getDefault)
    implicit val getDefaultLongProperty = build[io.swagger.models.properties.LongProperty](_.getDefault)
    implicit val getDefaultStringProperty = build[io.swagger.models.properties.StringProperty](_.getDefault)

    implicit val getDefaultCookieParameterParameter = build[io.swagger.models.parameters.CookieParameter](_.getDefault)
    implicit val getDefaultFormParameterParameter = build[io.swagger.models.parameters.FormParameter](_.getDefault)
    implicit val getDefaultHeaderParameterParameter = build[io.swagger.models.parameters.HeaderParameter](_.getDefault)
    implicit val getDefaultPathParameterParameter = build[io.swagger.models.parameters.PathParameter](_.getDefault)
    implicit val getDefaultQueryParameterParameter = build[io.swagger.models.parameters.QueryParameter](_.getDefault)
  }

  case class DefaultAdapter[F](from: F) extends AnyVal {
    def extract[T: Extractable](implicit F: GetDefault[F]): Option[T] =
      F.extract(from)
  }

  def apply[F](from: F): DefaultAdapter[F] = DefaultAdapter(from)
}
