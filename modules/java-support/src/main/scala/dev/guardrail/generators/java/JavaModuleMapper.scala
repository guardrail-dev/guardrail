package dev.guardrail.generators.java

import dev.guardrail.generators.spi.ModuleMapperLoader
import dev.guardrail.Target
import scala.reflect.runtime.universe.typeTag

class JavaModuleMapper extends ModuleMapperLoader {
  type L = JavaLanguage
  def reified = typeTag[Target[JavaLanguage]]
  def apply(frameworkName: String): Option[Set[String]] = frameworkName match {
    case "dropwizard" => Some(Set("dropwizard", "jackson", "java-stdlib", "async-http-client"))
    case "spring-mvc" => Some(Set("spring-mvc", "jackson", "java-stdlib", "async-http-client"))
    case _            => None
  }
}
