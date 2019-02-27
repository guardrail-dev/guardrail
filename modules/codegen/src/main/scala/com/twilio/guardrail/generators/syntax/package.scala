package com.twilio.guardrail.generators

import java.util.Locale

package object syntax {
  private val toPascalRegexes = List(
    "[\\._-]([a-z])".r, // dotted, snake, or dashed case
    "\\s+([a-zA-Z])".r, // spaces
    "^([a-z])".r // initial letter
  )

  implicit class RichString(val s: String) extends AnyVal {
    def toPascalCase: String =
      toPascalRegexes.foldLeft(s)(
        (accum, regex) => regex.replaceAllIn(accum, m => m.group(1).toUpperCase(Locale.US))
      )

    def toCamelCase: String = {
      val fromSnakeOrDashed =
        "[_-]([a-z])".r.replaceAllIn(s, m => m.group(1).toUpperCase(Locale.US))
      "^([A-Z])".r
        .replaceAllIn(fromSnakeOrDashed, m => m.group(1).toLowerCase(Locale.US))
    }
  }
}
