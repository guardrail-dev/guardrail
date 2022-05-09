package dev.guardrail.generators.java.springMvc

sealed abstract class SpringMvcVersion(val value: String)
object SpringMvcVersion extends SpringMvcVersion("spring-mvc") {
  def unapply(version: String): Option[SpringMvcVersion] = version match {
    case `value` => Some(SpringMvcVersion)
    case _       => None
  }
}
