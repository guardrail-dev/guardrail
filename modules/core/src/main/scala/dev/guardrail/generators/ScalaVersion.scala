package dev.guardrail.generators

sealed abstract class ScalaVersion(val value: String) extends Product with Serializable {
  def isScala3: Boolean = this == ScalaVersion.Scala3
  def isScala2: Boolean = !isScala3
}

object ScalaVersion {
  case object Scala212 extends ScalaVersion("2.12")
  case object Scala213 extends ScalaVersion("2.13")
  case object Scala3   extends ScalaVersion("3")

  def fromString(version: String): Either[String, ScalaVersion] = version.trim.toLowerCase match {
    case "2.12" | "2.12.x"       => Right(Scala212)
    case "2.13" | "2.13.x"       => Right(Scala213)
    case "3" | "3.x"             => Right(Scala3)
    case s if s.startsWith("3.") => Right(Scala3)
    case other                   => Left(s"Unsupported Scala version: $other. Supported: 2.12, 2.13, 3")
  }

  val default: ScalaVersion = Scala213
}
