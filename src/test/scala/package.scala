package com.twilio

import scala.meta._
import org.scalatest._

package object swagger {
  def scalaMetaFromResource(path: String): Source = {
    scala.io.Source.fromInputStream(getClass.getResourceAsStream(path))
        .getLines
        .mkString("\n")
        .parse[Source]
        .get
  }
}
