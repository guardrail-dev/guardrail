package examples.support

import examples.client.http4s.Implicits
import io.circe.Decoder

class PositiveLong private (val value: Long) extends AnyVal
object PositiveLong {
  def apply(value: Long): Option[PositiveLong] = if (value >= 0) Some(new PositiveLong(value)) else None
  implicit val showable                        = Implicits.Show.build[PositiveLong](_.value.toString())
  implicit val decodePositiveLong              = Decoder.decodeLong.emap(num => PositiveLong.apply(num).toRight(s"${num} is not positive"))
}
