package examples.support

import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.core.`type`.TypeReference
import examples.server.dropwizardScala.{JacksonImplicits => SJacksonImplicits}

class PositiveLong private (val value: Long)
object PositiveLong {
  def apply(value: Long): Option[PositiveLong] = if (value >= 0) Some(new PositiveLong(value)) else None
  @JsonCreator
  def applyUnsafe(value: Long): PositiveLong = apply(value).getOrElse({ throw new IllegalArgumentException(s"$value is not positive") })
  @JsonCreator
  def apply(str: String): PositiveLong = new PositiveLong(str.toLong)

  implicit val guardrailSEncodePositiveLong: SJacksonImplicits.GuardrailEncoder[PositiveLong] = SJacksonImplicits.GuardrailEncoder.instance
  implicit val guardrailSDecodePositiveLong: SJacksonImplicits.GuardrailDecoder[PositiveLong] = SJacksonImplicits.GuardrailDecoder.instance(new TypeReference[PositiveLong] {})
}
