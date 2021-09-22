package dev.guardrail.generators.scala

import cats.Monad
import dev.guardrail.core
import dev.guardrail.terms.CollectionsLibTerms
import dev.guardrail.Target
import scala.meta._

object ScalaCollectionsGenerator {
  object ScalaCollectionsInterp extends CollectionsLibTerms[ScalaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances

    def vendorPrefixes(): Target[List[String]] = Target.pure(List("x-scala", "x-jvm"))

    def liftOptionalType(value: Type): Target[Type] = Target.pure(t"Option[$value]")
    def liftOptionalTerm(value: Term): Target[Term] = Target.pure(q"Option($value)")
    def liftSomeTerm(value: Term): Target[Term]     = Target.pure(q"Some($value)")
    def emptyOptionalTerm(): Target[Term]           = Target.pure(q"None")

    def arrayType(format: Option[String]): Target[Type] = Target.pure(t"Iterable[String]")
    def liftVectorType(value: Type, customTpe: Option[Type]): Target[Type] =
      Target.pure(t"${customTpe.getOrElse(t"Vector")}[$value]")
    def liftVectorTerm(value: Term): Target[Term] = Target.pure(q"Vector($value)")
    def emptyArray(): Target[Term]                = Target.pure(q"Vector.empty")
    def embedArray(tpe: core.LazyResolvedType[ScalaLanguage], containerTpe: Option[Type]): Target[core.LazyResolvedType[ScalaLanguage]] = tpe match {
      case core.Deferred(tpe) =>
        Target.pure(core.DeferredArray[ScalaLanguage](tpe, containerTpe))
      case core.DeferredArray(_, _) =>
        Target.raiseUserError("FIXME: Got an Array of Arrays, currently not supported")
      case core.DeferredMap(_, _) =>
        Target.raiseUserError("FIXME: Got an Array of Maps, currently not supported")
    }

    def liftMapType(value: Type, customTpe: Option[Type]): Target[Type] =
      Target.pure(t"${customTpe.getOrElse(t"Map")}[String, $value]")
    def emptyMap(): Target[Term] = Target.pure(q"Map.empty")
    def embedMap(tpe: core.LazyResolvedType[ScalaLanguage], containerTpe: Option[Type]): Target[core.LazyResolvedType[ScalaLanguage]] = tpe match {
      case core.Deferred(inner) =>
        Target.pure(core.DeferredMap[ScalaLanguage](inner, containerTpe))
      case core.DeferredMap(_, _) =>
        Target.raiseUserError("FIXME: Got a map of maps, currently not supported")
      case core.DeferredArray(_, _) =>
        Target.raiseUserError("FIXME: Got a map of arrays, currently not supported")
    }
  }
}
