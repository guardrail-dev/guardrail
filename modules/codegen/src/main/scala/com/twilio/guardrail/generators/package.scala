package com.twilio.guardrail.generators.syntax
import com.twilio.guardrail.generators._
import _root_.scala.meta._
import cats.data.NonEmptyList

object scala {
  implicit class RichRawParameterName(parameter: RawParameterName) {
    def toLit: Lit.String = Lit.String(parameter.value)
  }

  implicit class ExtendedUnzip[T1, T2, T3, T4, T5, T6, T7](xs: NonEmptyList[(T1, T2, T3, T4, T5, T6, T7)]) {
    def unzip7: (List[T1], List[T2], List[T3], List[T4], List[T5], List[T6], List[T7]) =
      xs.foldLeft(
        (List.empty[T1], List.empty[T2], List.empty[T3], List.empty[T4], List.empty[T5], List.empty[T6], List.empty[T7])
      ) {
        case ((v1a, v2a, v3a, v4a, v5a, v6a, v7a), (v1, v2, v3, v4, v5, v6, v7)) =>
          (v1a :+ v1, v2a :+ v2, v3a :+ v3, v4a :+ v4, v5a :+ v5, v6a :+ v6, v7a :+ v7)
      }
  }
}
