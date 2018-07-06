package support

import clients.akkaHttp.{ Implicits => AkkaImplicits }
import clients.http4s.{ Implicits => Http4sImplicits }
import io.circe.Decoder

class PositiveLong private (val value: Long) extends AnyVal
object PositiveLong {
  def apply(value: Long): Option[PositiveLong] = if (value >= 0) Some(new PositiveLong(value)) else None
  implicit val akkaShowable = AkkaImplicits.Show.build[PositiveLong](_.value.toString())
  implicit val http4sShowable = Http4sImplicits.Show.build[PositiveLong](_.value.toString())
  implicit val decodePositiveLong = Decoder.decodeLong.emap(num => PositiveLong.apply(num).toRight(s"${num} is not positive"))
}
