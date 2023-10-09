package examples.support

import examples.client.akkaHttp.Implicits
import io.circe.Decoder

class PositiveLong private (val value: Long) extends AnyVal
object PositiveLong {
  def apply(value: Long): Option[PositiveLong]           = if (value >= 0) Some(new PositiveLong(value)) else None
  implicit val showable: Implicits.Show[PositiveLong]    = Implicits.Show.build[PositiveLong](_.value.toString())
  implicit val decodePositiveLong: Decoder[PositiveLong] = Decoder.decodeLong.emap(num => PositiveLong.apply(num).toRight(s"${num} is not positive"))
}
