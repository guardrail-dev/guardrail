package com.twilio.guardrail
import java.nio.charset.{ Charset, StandardCharsets }
import java.nio.file.Path

package object generators {
  val utf8: Charset                             = StandardCharsets.UTF_8
  val resolveFile: Path => List[String] => Path = root => _.foldLeft(root)(_.resolve(_))
}
