package dev.guardrail.generators.collections

import cats.Monad
import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, Type }
import com.github.javaparser.ast.{ Node, NodeList }
import com.github.javaparser.ast.expr.ObjectCreationExpr
import dev.guardrail.core.LazyResolvedType
import dev.guardrail.Target
import dev.guardrail.core
import dev.guardrail.generators.syntax.Java._
import dev.guardrail.languages.JavaLanguage
import dev.guardrail.terms.CollectionsLibTerms
@SuppressWarnings(Array("org.wartremover.warts.Null"))
object JavaCollectionsGenerator {

  class JavaCollectionsInterp extends CollectionsLibTerms[JavaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances

    def vendorPrefixes(): Target[List[String]] = Target.pure(List("x-java", "x-jvm"))

    def liftOptionalType(value: Type): Target[Type] =
      safeParseClassOrInterfaceType(s"java.util.Optional").map(_.setTypeArguments(new NodeList(value)))

    def liftOptionalTerm(value: Node): Target[Node] =
      buildMethodCall("java.util.Optional.ofNullable", Some(value))

    def liftSomeTerm(value: Node): Target[Node] =
      buildMethodCall("java.util.Optional.of", Some(value))

    def emptyOptionalTerm(): Target[Node] = buildMethodCall("java.util.Optional.empty")

    def arrayType(format: Option[String]): Target[Type] =
      safeParseClassOrInterfaceType("java.util.List").map(_.setTypeArguments(new NodeList[Type](STRING_TYPE)))

    def liftVectorType(value: Type, customTpe: Option[Type]): Target[Type] =
      customTpe
        .fold[Target[ClassOrInterfaceType]](safeParseClassOrInterfaceType("java.util.List").map(identity))({
          case t: ClassOrInterfaceType =>
            Target.pure(t)
          case x =>
            Target.raiseUserError(s"Unsure how to map $x")
        })
        .map(_.setTypeArguments(new NodeList(value)))

    def liftVectorTerm(value: Node): Target[Node] =
      buildMethodCall("java.util.Collections.singletonList", Some(value))

    def emptyArray(): Target[Node] =
      for {
        cls <- safeParseClassOrInterfaceType("java.util.ArrayList")
      } yield new ObjectCreationExpr(null, cls.setTypeArguments(new NodeList[Type]), new NodeList())

    def embedArray(tpe: LazyResolvedType[JavaLanguage], containerTpe: Option[Type]): Target[LazyResolvedType[JavaLanguage]] =
      tpe match {
        case core.Deferred(tpe) =>
          Target.pure(core.DeferredArray[JavaLanguage](tpe, containerTpe))
        case core.DeferredArray(_, _) =>
          Target.raiseUserError("FIXME: Got an Array of Arrays, currently not supported")
        case core.DeferredMap(_, _) =>
          Target.raiseUserError("FIXME: Got an Array of Maps, currently not supported")
      }

    def liftMapType(value: Type, customTpe: Option[Type]): Target[Type] =
      customTpe
        .fold[Target[ClassOrInterfaceType]](safeParseClassOrInterfaceType("java.util.Map").map(identity))({
          case t: ClassOrInterfaceType =>
            Target.pure(t)
          case x =>
            Target.raiseUserError(s"Unsure how to map $x")
        })
        .map(_.setTypeArguments(STRING_TYPE, value))

    def emptyMap(): Target[Node] =
      Target.pure(
        new ObjectCreationExpr(null, StaticJavaParser.parseClassOrInterfaceType("java.util.HashMap").setTypeArguments(new NodeList[Type]), new NodeList())
      )

    def embedMap(tpe: LazyResolvedType[JavaLanguage], containerTpe: Option[Type]): Target[LazyResolvedType[JavaLanguage]] =
      tpe match {
        case core.Deferred(inner) =>
          Target.pure(core.DeferredMap[JavaLanguage](inner, containerTpe))
        case core.DeferredMap(_, _) =>
          Target.raiseUserError("FIXME: Got a map of maps, currently not supported")
        case core.DeferredArray(_, _) =>
          Target.raiseUserError("FIXME: Got a map of arrays, currently not supported")
      }
  }
}
