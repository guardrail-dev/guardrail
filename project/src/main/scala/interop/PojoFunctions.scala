package dev.guardrail.sbt.interop

object PojoFunctions {
  import _root_.scala.meta._

  // Build up the full AST for a useful toString implementation
  def buildToString(className: String, parameters: List[Term.Param]): Defn.Def = {
    val toStringTerm = {
      val fields: List[Term.ApplyInfix] = parameters.map {
        case param"${term@Term.Name(field)}: $_" =>
          q"${Lit.String(field + " = ")} + ${term}.toString()"
      }

      val commaSeparated = fields.foldLeft[Option[Term]](None) {
        case (None, term) => Some(term)
        case (Some(acc), term) => Some(q"$acc + ${Lit.String(", ")} + ${term}")
      }

      val start = Lit.String(className + "(")
      val end = q""" ")" """
      commaSeparated.fold(q"$start + $end") { inner =>
        q"$start + $inner + $end"
      }
    }

    q"""
      override def toString(): String = ${toStringTerm}
    """
  }

  def buildWithMethods(
    typeName: Type.Name,
    termName: Term.Name,
    parameters: List[Term.Param],
  ): List[Defn.Def] = {
    parameters.zipWithIndex.map { case (p@param"${term@Term.Name(name)}: $tpe", idx) =>
      val withName = Term.Name(s"with${name.capitalize}")
      val (firstAssigns, lastAssigns) = {
        val assigns = parameters.map { case param"${term@Term.Name(_)}: $_" =>
          q"$term = self.$term"
        }
        val (firstAssigns, rest) = assigns.splitAt(idx)
        val lastAssigns = rest.drop(1)
        (firstAssigns, lastAssigns)
      }
      val allAssigns = (firstAssigns :+ q"${term} = ${term}") ++ lastAssigns
      q"""
        def ${withName}(${p}): ${typeName} = ${termName}(..${allAssigns})
      """
    }
  }
}
