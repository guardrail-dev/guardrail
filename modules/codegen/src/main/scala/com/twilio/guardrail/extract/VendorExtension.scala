package com.twilio.guardrail.extract

object VendorExtension {
  trait VendorExtensible[F] {
    def extract[T](from: F, key: String)(implicit T: Extractable[T]): Option[T]
  }

  object VendorExtensible {
    def build[F](f: F => String => Any): VendorExtensible[F] = {
      new VendorExtensible[F] {
        def extract[T](from: F, key: String)(implicit T: Extractable[T]): Option[T] = {
          T.extract(f(from)(key)).toOption
        }
      }
    }

    implicit val defaultVendorExtensibleModel = build[io.swagger.models.Model](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleOperation = build[io.swagger.models.Operation](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleParameter = build[io.swagger.models.parameters.Parameter](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensiblePath = build[io.swagger.models.Path](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleProperty = build[io.swagger.models.properties.Property](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleResponse = build[io.swagger.models.Response](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleSwagger = build[io.swagger.models.Swagger](m => key => m.getVendorExtensions.get(key))

    implicit val defaultVendorExtensibleArrayProperty = build[io.swagger.models.properties.ArrayProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleBaseIntegerProperty = build[io.swagger.models.properties.BaseIntegerProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleBooleanProperty = build[io.swagger.models.properties.BooleanProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleDateProperty = build[io.swagger.models.properties.DateProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleDateTimeProperty = build[io.swagger.models.properties.DateTimeProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleDecimalProperty = build[io.swagger.models.properties.DecimalProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleDoubleProperty = build[io.swagger.models.properties.DoubleProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleFloatProperty = build[io.swagger.models.properties.FloatProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleLongProperty = build[io.swagger.models.properties.LongProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleMapProperty = build[io.swagger.models.properties.MapProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleModelImpl = build[io.swagger.models.ModelImpl](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleObjectProperty = build[io.swagger.models.properties.ObjectProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleStringProperty = build[io.swagger.models.properties.StringProperty](m => key => m.getVendorExtensions.get(key))

    implicit val defaultVendorExtensibleCookieParameter = build[io.swagger.models.parameters.CookieParameter](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleFormParameter = build[io.swagger.models.parameters.FormParameter](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleHeaderParameter = build[io.swagger.models.parameters.HeaderParameter](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensiblePathParameter = build[io.swagger.models.parameters.PathParameter](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleQueryParameter = build[io.swagger.models.parameters.QueryParameter](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleSerializableParameter = build[io.swagger.models.parameters.SerializableParameter](m => key => m.getVendorExtensions.get(key))
  }

  case class VendorExtensibleAdapter[F](from: F) extends AnyVal {
    def extract[T: Extractable](key: String)(implicit F: VendorExtensible[F]): Option[T] =
      F.extract(from, key)
  }

  def apply[F](from: F): VendorExtensibleAdapter[F] = VendorExtensibleAdapter(from)
}
