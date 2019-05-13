package com.twilio.guardrail.extract

import com.twilio.guardrail.terms.SecurityScheme
import io.swagger.v3.oas.models.media._
import io.swagger.v3.oas.models._
import io.swagger.v3.oas.models.parameters.{ CookieParameter, Parameter }
import io.swagger.v3.oas.models.responses.ApiResponse
import io.swagger.v3.oas.models.security.{ SecurityScheme => SwSecurityScheme }

object VendorExtension {
  trait VendorExtensible[-F] {
    def extract[T](from: F, key: String)(implicit T: Extractable[T]): Option[T]
  }

  object VendorExtensible {

    def build[F](f: F => String => Option[Any]): VendorExtensible[F] =
      new VendorExtensible[F] {
        def extract[T](from: F, key: String)(implicit T: Extractable[T]): Option[T] =
          f(from)(key).flatMap(T.extract(_).toOption)
      }

    implicit val defaultVendorExtensibleOperation: VendorExtensible[Operation] =
      build[Operation](m => key => Option(m.getExtensions).map(_.get(key)))

    implicit val defaultVendorExtensibleParameter: VendorExtensible[Parameter] =
      build[Parameter](m => key => Option(m.getExtensions).map(_.get(key)))

    implicit val defaultVendorExtensiblePath: VendorExtensible[PathItem] =
      build[PathItem](m => key => Option(m.getExtensions).map(_.get(key)))

    implicit val defaultVendorExtensibleResponse: VendorExtensible[ApiResponse] =
      build[ApiResponse](m => key => Option(m.getExtensions).map(_.get(key)))

    implicit val defaultVendorExtensibleSwagger: VendorExtensible[OpenAPI] =
      build[OpenAPI](m => key => Option(m.getExtensions).map(_.get(key)))

    implicit val defaultVendorExtensibleSchemaProperty: VendorExtensible[Schema[_]] =
      build[Schema[_]](m => key => Option(m.getExtensions).map(_.get(key)))

    implicit val defaultVendorExtensibleArrayProperty: VendorExtensible[ArraySchema] =
      build[ArraySchema](m => key => Option(m.getExtensions).map(_.get(key)))

    implicit val defaultVendorExtensibleBooleanProperty: VendorExtensible[BooleanSchema] =
      build[BooleanSchema](m => key => Option(m.getExtensions).map(_.get(key)))

    implicit val defaultVendorExtensibleDateProperty: VendorExtensible[DateSchema] =
      build[DateSchema](m => key => Option(m.getExtensions).map(_.get(key)))

    implicit val defaultVendorExtensibleDateTimeProperty: VendorExtensible[DateTimeSchema] =
      build[DateTimeSchema](m => key => Option(m.getExtensions).map(_.get(key)))

    implicit val defaultVendorExtensibleFloatProperty: VendorExtensible[NumberSchema] =
      build[NumberSchema](m => key => Option(m.getExtensions).map(_.get(key)))

    implicit val defaultVendorExtensibleIntegerProperty: VendorExtensible[IntegerSchema] =
      build[IntegerSchema](m => key => Option(m.getExtensions).map(_.get(key)))

    implicit val defaultVendorExtensibleMapProperty: VendorExtensible[MapSchema] =
      build[MapSchema](m => key => Option(m.getExtensions).map(_.get(key)))

    implicit val defaultVendorExtensibleObjectProperty: VendorExtensible[ObjectSchema] =
      build[ObjectSchema](m => key => Option(m.getExtensions).map(_.get(key)))

    implicit val defaultVendorExtensibleStringProperty: VendorExtensible[StringSchema] =
      build[StringSchema](m => key => Option(m.getExtensions).map(_.get(key)))

    implicit val defaultVendorExtensibleCookieParameter: VendorExtensible[CookieParameter] =
      build[CookieParameter](m => key => Option(m.getExtensions).map(_.get(key)))

    implicit val defaultVendorExtensibleSwSecurityScheme: VendorExtensible[SwSecurityScheme] =
      build[SwSecurityScheme](m => key => Option(m.getExtensions).map(_.get(key)))

    implicit val defaultVendorExtensibleSecurityScheme: VendorExtensible[SecurityScheme] =
      build[SecurityScheme](m => key => Option(m.extensions).flatMap(_.get(key)))
  }

  case class VendorExtensibleAdapter[F](from: F) extends AnyVal {
    def extract[T: Extractable](key: String)(implicit F: VendorExtensible[F]): Option[T] =
      F.extract(from, key)
  }

  def apply[F](from: F): VendorExtensibleAdapter[F] =
    VendorExtensibleAdapter(from)
}
