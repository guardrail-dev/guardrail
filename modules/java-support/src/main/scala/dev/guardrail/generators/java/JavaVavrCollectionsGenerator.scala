package dev.guardrail.generators.java

import com.github.javaparser.ast.{ Node, NodeList }
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, Type }
import scala.reflect.runtime.universe.typeTag

import dev.guardrail.Target
import dev.guardrail.core
import dev.guardrail.generators.java.syntax.{ buildMethodCall, _ }
import dev.guardrail.terms.CollectionsLibTerms
import dev.guardrail.generators.spi.{ CollectionsGeneratorLoader, ModuleLoadResult }

class JavaVavrCollectionsGeneratorLoader extends CollectionsGeneratorLoader {
  type L = JavaLanguage
  def reified = typeTag[Target[JavaLanguage]]
  val apply   = ModuleLoadResult.forProduct1(CollectionsGeneratorLoader.label -> Seq(JavaVavrCollectionsGenerator.mapping))(identity _)
}

object JavaVavrCollectionsGenerator {
  def apply(): CollectionsLibTerms[JavaLanguage, Target] =
    new JavaVavrCollectionsGenerator
  val mapping: Map[String, JavaVavrCollectionsGenerator] = Map(
    "java-vavr" -> new JavaVavrCollectionsGenerator
  )
}

class JavaVavrCollectionsGenerator private extends CollectionsLibTerms[JavaLanguage, Target] {
  private val baseInterp = JavaCollectionsGenerator()

  override def vendorPrefixes(): Target[List[String]] = baseInterp.vendorPrefixes().map("x-java-vavr" +: _)

  override def liftOptionalType(value: Type): Target[Type] =
    safeParseClassOrInterfaceType("io.vavr.control.Option").map(_.setTypeArguments(new NodeList(value)))

  override def liftOptionalTerm(value: Node): Target[Node] =
    buildMethodCall("io.vavr.control.Option.of", Some(value))

  override def liftSomeTerm(value: Node): Target[Node] =
    for {
      wrappedValue <- buildMethodCall("java.util.Objects.requireNonNull", Some(value))
      term         <- buildMethodCall("io.vavr.control.Option.some", Some(wrappedValue))
    } yield term

  override def emptyOptionalTerm(): Target[Node] = buildMethodCall("io.vavr.control.Option.none")

  override def arrayType(format: Option[String]): Target[Type] =
    safeParseClassOrInterfaceType("io.vavr.collection.Vector").map(_.setTypeArguments(new NodeList[Type](STRING_TYPE)))

  override def liftVectorType(value: Type, customTpe: Option[Type]): Target[Type] =
    customTpe
      .fold[Target[ClassOrInterfaceType]](safeParseClassOrInterfaceType("io.vavr.collection.Vector").map(identity)) {
        case t: ClassOrInterfaceType =>
          Target.pure(t)
        case x =>
          Target.raiseUserError(s"Unsure how to map $x")
      }
      .map(_.setTypeArguments(new NodeList(value)))

  override def liftVectorTerm(value: Node): Target[Node] =
    buildMethodCall("io.vavr.collection.Vector.of", Some(value))

  override def emptyArray(): Target[Node] = buildMethodCall("io.vavr.collection.Vector.empty")

  override def embedArray(tpe: core.LazyResolvedType[JavaLanguage], containerTpe: Option[Type]): Target[core.LazyResolvedType[JavaLanguage]] =
    tpe match {
      case core.Deferred(tpe) =>
        Target.pure(core.DeferredArray[JavaLanguage](tpe, containerTpe))
      case core.DeferredArray(_, _) =>
        Target.raiseUserError("FIXME: Got an Array of Arrays, currently not supported")
      case core.DeferredMap(_, _) =>
        Target.raiseUserError("FIXME: Got an Array of Maps, currently not supported")
    }

  override def liftMapType(value: Type, customTpe: Option[Type]): Target[Type] =
    customTpe
      .fold[Target[ClassOrInterfaceType]](safeParseClassOrInterfaceType("io.vavr.collection.Map").map(identity)) {
        case t: ClassOrInterfaceType =>
          Target.pure(t)
        case x =>
          Target.raiseUserError(s"Unsure how to map $x")
      }
      .map(_.setTypeArguments(STRING_TYPE, value))

  override def emptyMap(): Target[Node] =
    buildMethodCall("io.vavr.collection.HashMap.empty")

  override def embedMap(tpe: core.LazyResolvedType[JavaLanguage], containerTpe: Option[Type]): Target[core.LazyResolvedType[JavaLanguage]] =
    tpe match {
      case core.Deferred(inner) =>
        Target.pure(core.DeferredMap[JavaLanguage](inner, containerTpe))
      case core.DeferredMap(_, _) =>
        Target.raiseUserError("FIXME: Got a map of maps, currently not supported")
      case core.DeferredArray(_, _) =>
        Target.raiseUserError("FIXME: Got a map of arrays, currently not supported")
    }
}
