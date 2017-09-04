package com.twilio.swagger.codegen

import cats.data.NonEmptyList

object NelShim {
  implicit class RichNel[T](xs: NonEmptyList[T]) {
    def last: T = xs.tail.lastOption.getOrElse(xs.head)
  }
}
