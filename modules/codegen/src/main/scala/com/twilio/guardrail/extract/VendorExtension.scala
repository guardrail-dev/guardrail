package com.twilio.guardrail.extract

import io.swagger.models._
import io.swagger.models.parameters._
import io.swagger.models.properties._

object VendorExtension {
  trait VendorExtensible[F] {
    def extract[T](from: F, key: String)(implicit T: Extractable[T]): Option[T]
  }

  object VendorExtensible {
    def build[F](f: F => String => Any): VendorExtensible[F] =
      new VendorExtensible[F] {
        def extract[T](from: F, key: String)(implicit T: Extractable[T]): Option[T] =
          T.extract(f(from)(key)).toOption
      }

    implicit val defaultVendorExtensibleModel: VendorExtensible[Model] =
      build[Model](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleOperation: VendorExtensible[Operation] =
      build[Operation](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleParameter: VendorExtensible[Parameter] =
      build[Parameter](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensiblePath: VendorExtensible[Path] =
      build[Path](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleProperty: VendorExtensible[Property] =
      build[Property](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleResponse: VendorExtensible[Response] =
      build[Response](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleSwagger: VendorExtensible[Swagger] =
      build[Swagger](m => key => m.getVendorExtensions.get(key))

    implicit val defaultVendorExtensibleAbstractProperty: VendorExtensible[AbstractProperty] =
      build[AbstractProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleArrayProperty: VendorExtensible[ArrayProperty] =
      build[ArrayProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleBaseIntegerProperty: VendorExtensible[BaseIntegerProperty] =
      build[BaseIntegerProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleBooleanProperty: VendorExtensible[BooleanProperty] =
      build[BooleanProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleDateProperty: VendorExtensible[DateProperty] =
      build[DateProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleDateTimeProperty: VendorExtensible[DateTimeProperty] =
      build[DateTimeProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleDecimalProperty: VendorExtensible[DecimalProperty] =
      build[DecimalProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleDoubleProperty: VendorExtensible[DoubleProperty] =
      build[DoubleProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleFloatProperty: VendorExtensible[FloatProperty] =
      build[FloatProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleIntegerProperty: VendorExtensible[IntegerProperty] =
      build[IntegerProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleLongProperty: VendorExtensible[LongProperty] =
      build[LongProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleMapProperty: VendorExtensible[MapProperty] =
      build[MapProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleModelImpl: VendorExtensible[ModelImpl] =
      build[ModelImpl](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleObjectProperty: VendorExtensible[ObjectProperty] =
      build[ObjectProperty](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleStringProperty: VendorExtensible[StringProperty] =
      build[StringProperty](m => key => m.getVendorExtensions.get(key))

    implicit val defaultVendorExtensibleCookieParameter: VendorExtensible[CookieParameter] =
      build[CookieParameter](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleFormParameter: VendorExtensible[FormParameter] =
      build[FormParameter](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleHeaderParameter: VendorExtensible[HeaderParameter] =
      build[HeaderParameter](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensiblePathParameter: VendorExtensible[PathParameter] =
      build[PathParameter](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleQueryParameter: VendorExtensible[QueryParameter] =
      build[QueryParameter](m => key => m.getVendorExtensions.get(key))
    implicit val defaultVendorExtensibleSerializableParameter: VendorExtensible[SerializableParameter] =
      build[SerializableParameter](m => key => m.getVendorExtensions.get(key))
  }

  case class VendorExtensibleAdapter[F](from: F) extends AnyVal {
    def extract[T: Extractable](key: String)(implicit F: VendorExtensible[F]): Option[T] =
      F.extract(from, key)
  }

  def apply[F](from: F): VendorExtensibleAdapter[F] =
    VendorExtensibleAdapter(from)
}
