package dev.guardrail.sbt.interop

object CompanionFunctions {
  import scala.meta._

  def buildApply(typeName: Type.Name, parameters: List[Term.Param]): Defn.Def = {
    val assignedParameters = parameters.map { case param"${term@Term.Name(_)}: $_" =>
      q"$term = $term"
    }

    q"""
      def apply(..${parameters}): ${typeName} = new ${typeName}(..$assignedParameters)
    """
  }
}
