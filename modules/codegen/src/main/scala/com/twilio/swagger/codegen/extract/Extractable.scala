package com.twilio.swagger.codegen.extract

import scala.util.Try


trait Extractable[T] {
  def extract(v: Any): Try[T]
}

object Extractable {
  def build[T](f: PartialFunction[Any, T]): Extractable[T] = {
    new Extractable[T] {
      def extract(v: Any): Try[T] = Try(f(v))
    }
  }

  implicit val defaultExtractableBoolean = build[Boolean]({ case x: Boolean => x })
  implicit val defaultExtractableDouble = build[Double]({ case x: Double => x })
  implicit val defaultExtractableFloat = build[Float]({ case x: Float => x })
  implicit val defaultExtractableInt = build[Int]({
    case x: Int => x
    case x: Long if x <= Int.MaxValue && x >= Int.MinValue => x.toInt
  })
  implicit val defaultExtractableLong = build[Long]({ case x: Long => x })
  implicit val defaultExtractableString = build[String]({ case x: String => x })
}
