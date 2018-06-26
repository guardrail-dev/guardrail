package com.twilio.guardrail
package terms.framework
import com.twilio.guardrail.generators.GeneratorSettings

import scala.meta._

sealed trait FrameworkTerm[T]
case class GetFrameworkImports(tracing: Boolean)                       extends FrameworkTerm[List[Import]]
case class GetFrameworkImplicits(generatorSettings: GeneratorSettings) extends FrameworkTerm[Defn.Object]
case class GetGeneratorSettings()                                      extends FrameworkTerm[GeneratorSettings]
