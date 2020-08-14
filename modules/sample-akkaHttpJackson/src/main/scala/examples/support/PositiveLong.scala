package examples.support

import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.core.`type`.TypeReference
import examples.client.akkaHttpJackson.Implicits
import examples.client.akkaHttpJackson.{JacksonImplicits => CJacksonImplicits}
import examples.server.akkaHttpJackson.{JacksonImplicits => SJacksonImplicits}
import javax.validation.constraints.{Min, NotNull}
import scala.annotation.meta.{field, param}

class PositiveLong private (@(NotNull @param @field) @Min(0) val value: Long)
object PositiveLong {
  def apply(value: Long): Option[PositiveLong] = if (value >= 0) Some(new PositiveLong(value)) else None
  @JsonCreator
  def applyUnsafe(value: Long): PositiveLong = apply(value).getOrElse({ throw new IllegalArgumentException(s"$value is not positive") })
  @JsonCreator
  def apply(str: String): PositiveLong = new PositiveLong(str.toLong)

  implicit val showable: Implicits.Show[PositiveLong] = Implicits.Show.build[PositiveLong](_.value.toString())
  implicit val guardrailCEncodePositiveLong: CJacksonImplicits.GuardrailEncoder[PositiveLong] = CJacksonImplicits.GuardrailEncoder.instance
  implicit val guardrailCDecodePositiveLong: CJacksonImplicits.GuardrailDecoder[PositiveLong] = CJacksonImplicits.GuardrailDecoder.instance(new TypeReference[PositiveLong] {})
  implicit val guardrailCValidatePositiveLog: CJacksonImplicits.GuardrailValidator[PositiveLong] = CJacksonImplicits.GuardrailValidator.instance
  implicit val guardrailSEncodePositiveLong: SJacksonImplicits.GuardrailEncoder[PositiveLong] = SJacksonImplicits.GuardrailEncoder.instance
  implicit val guardrailSDecodePositiveLong: SJacksonImplicits.GuardrailDecoder[PositiveLong] = SJacksonImplicits.GuardrailDecoder.instance(new TypeReference[PositiveLong] {})
  implicit val guardrailSValidatePositiveLog: SJacksonImplicits.GuardrailValidator[PositiveLong] = SJacksonImplicits.GuardrailValidator.instance
}
