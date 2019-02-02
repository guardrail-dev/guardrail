package com.twilio.guardrail.extract
import com.twilio.guardrail.{ EmptyIsEmpty, EmptyIsNull, EmptyToNullBehaviour }

import scala.util.Try

trait Extractable[T] {
  def extract(v: Any): Try[T]
}

object Extractable {
  def build[T](f: PartialFunction[Any, T]): Extractable[T] =
    (v: Any) => Try(f(v))

  implicit val defaultExtractableBoolean: Extractable[Boolean] =
    build[Boolean]({ case x: Boolean => x })
  implicit val defaultExtractableDouble: Extractable[Double] = build[Double]({
    case x: Float  => x.toDouble
    case x: Double => x
  })
  implicit val defaultExtractableFloat: Extractable[Float] = build[Float]({
    case x: Float  => x
    case x: Double => x.toFloat // FIXME: Will likely trim
  })
  implicit val defaultExtractableInt: Extractable[Int] = build[Int]({
    case x: Int                                            => x
    case x: Long if x <= Int.MaxValue && x >= Int.MinValue => x.toInt // FIXME: Warn that default value is being dropped
  })
  implicit val defaultExtractableLong: Extractable[Long] = build[Long]({
    case x: Int  => x
    case x: Long => x
  })
  implicit val defaultExtractableString: Extractable[String] = build[String]({
    case x: String => x
  })
  implicit val defaultExtractableEmptyToNullBehaviour: Extractable[EmptyToNullBehaviour] =
    build[EmptyToNullBehaviour]({
      case x: Boolean if x  => EmptyIsNull
      case x: Boolean if !x => EmptyIsEmpty
    })
}
