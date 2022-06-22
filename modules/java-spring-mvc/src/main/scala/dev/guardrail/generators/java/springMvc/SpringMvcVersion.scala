package dev.guardrail.generators.java.springMvc

sealed abstract class SpringMvcVersion(val value: String)
object SpringMvcVersion extends SpringMvcVersion("spring-mvc") {
  val mapping: Map[String, SpringMvcVersion] = Map(
    value -> SpringMvcVersion
  )
}
