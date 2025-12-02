package dev.guardrail.generators.scala

import scala.meta._

import dev.guardrail.generators.ScalaVersion

/** Utilities for generating Scala 2 or Scala 3 compatible code based on target version.
  *
  * Scala 3 uses `using` clauses instead of `implicit` parameter clauses, and `given` instances instead of `implicit val`/`implicit def`. This module provides
  * helpers to generate the appropriate syntax based on the target Scala version.
  */
object Scala3Compat {

  /** Create an implicit or using parameter clause based on Scala version.
    *
    * For Scala 2: `(implicit ec: ExecutionContext)` For Scala 3: `(using ec: ExecutionContext)`
    *
    * @param params
    *   The parameters for the clause
    * @param scalaVersion
    *   The target Scala version
    * @return
    *   A Term.ParamClause with the appropriate modifier
    */
  def implicitsClause(params: List[Term.Param], scalaVersion: ScalaVersion): Term.ParamClause = {
    val modOpt = if (scalaVersion.isScala3) Some(Mod.Using()) else Some(Mod.Implicit())
    Term.ParamClause(params, modOpt)
  }

  /** Create an implicit val or given definition for a simple value.
    *
    * For Scala 2: `implicit val name: Type = rhs` For Scala 3: `given name: Type = rhs`
    *
    * @param name
    *   The name of the definition
    * @param tpe
    *   The type of the definition
    * @param rhs
    *   The right-hand side expression
    * @param scalaVersion
    *   The target Scala version
    * @return
    *   A definition statement
    */
  def implicitVal(
      name: Term.Name,
      tpe: Type,
      rhs: Term,
      scalaVersion: ScalaVersion
  ): Defn =
    if (scalaVersion.isScala3) {
      // For Scala 3: given name: Type = rhs
      // Use the Defn.GivenAlias which is simpler for value-like givens
      Defn.GivenAlias(
        Nil,  // mods
        name, // name
        None, // paramClauseGroup (no type params or using params)
        tpe,  // decltpe
        rhs   // body
      )
    } else {
      q"implicit val ${Pat.Var(name)}: $tpe = $rhs"
    }

  /** Create an implicit val or given definition using a pattern (for more complex cases).
    *
    * For Scala 2: {{{implicit val pattern: Type = rhs}}} For Scala 3: {{{given name: Type = rhs}}}
    *
    * Note: For Scala 3, we extract the name from the pattern if possible.
    *
    * @param pattern
    *   The pattern for the val (typically Pat.Var)
    * @param tpe
    *   The type of the definition
    * @param rhs
    *   The right-hand side expression
    * @param scalaVersion
    *   The target Scala version
    * @return
    *   A definition statement
    */
  def implicitValWithPattern(
      pattern: Pat,
      tpe: Type,
      rhs: Term,
      scalaVersion: ScalaVersion
  ): Defn =
    pattern match {
      case Pat.Var(name) => implicitVal(name, tpe, rhs, scalaVersion)
      case _             =>
        // For complex patterns, fall back to Scala 2 style (Scala 3 doesn't support patterns in given)
        q"implicit val $pattern: $tpe = $rhs"
    }

  /** Create an implicit def or given definition with type parameters and/or implicit parameters.
    *
    * For Scala 2: `implicit def name[A](implicit ev: Show[A]): Encoder[A] = ...` For Scala 3: `given name[A](using ev: Show[A]): Encoder[A] = ...`
    *
    * @param name
    *   The name of the definition
    * @param typeParams
    *   Type parameters
    * @param implicitParams
    *   Implicit/using parameters
    * @param returnType
    *   The return type
    * @param body
    *   The implementation body
    * @param scalaVersion
    *   The target Scala version
    * @return
    *   A definition statement
    */
  def implicitDef(
      name: Term.Name,
      typeParams: List[Type.Param],
      implicitParams: List[Term.Param],
      returnType: Type,
      body: Term,
      scalaVersion: ScalaVersion
  ): Defn =
    if (scalaVersion.isScala3) {
      val usingClause = if (implicitParams.nonEmpty) {
        List(Term.ParamClause(implicitParams, Some(Mod.Using())))
      } else {
        Nil
      }
      val paramClauseGroup = if (typeParams.nonEmpty || usingClause.nonEmpty) {
        val tparamClause = Type.ParamClause(typeParams)
        Some(Member.ParamClauseGroup(tparamClause, usingClause))
      } else {
        None
      }
      Defn.GivenAlias(
        Nil,              // mods
        name,             // name
        paramClauseGroup, // paramClauseGroup
        returnType,       // decltpe
        body              // body
      )
    } else {
      val paramClause = if (implicitParams.nonEmpty) {
        List(Term.ParamClause(implicitParams, Some(Mod.Implicit())))
      } else {
        Nil
      }
      if (typeParams.nonEmpty) {
        q"implicit def $name[..$typeParams](...$paramClause): $returnType = $body"
      } else if (paramClause.nonEmpty) {
        q"implicit def $name(...$paramClause): $returnType = $body"
      } else {
        q"implicit def $name: $returnType = $body"
      }
    }

  /** Helper to create implicit/given with explicit Defn.Val syntax for Scala 2 compatibility.
    *
    * This is useful when you need to generate code that will be compared in tests, as it produces more predictable AST structure.
    *
    * @param mods
    *   Additional modifiers (will have implicit prepended for Scala 2)
    * @param name
    *   The name of the val
    * @param tpe
    *   The declared type
    * @param rhs
    *   The right-hand side expression
    * @param scalaVersion
    *   The target Scala version
    * @return
    *   A definition statement
    */
  def implicitValExplicit(
      mods: List[Mod],
      name: Term.Name,
      tpe: Type,
      rhs: Term,
      scalaVersion: ScalaVersion
  ): Defn =
    if (scalaVersion.isScala3) {
      Defn.GivenAlias(
        mods,
        name,
        None,
        tpe,
        rhs
      )
    } else {
      Defn.Val(
        Mod.Implicit() +: mods,
        List(Pat.Var(name)),
        Some(tpe),
        rhs
      )
    }
}
