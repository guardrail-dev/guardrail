package dev.guardrail.generators.spi

import java.util.ServiceLoader
import scala.jdk.CollectionConverters._
import scala.reflect.runtime.universe.TypeTag

import dev.guardrail.{ MissingDependency, Target }
import dev.guardrail.languages.LA
import dev.guardrail.terms.CollectionsLibTerms

trait CollectionsGeneratorLoader {
  type L <: LA
  def reified: TypeTag[Target[L]]
  def apply(parameters: Set[String]): Option[CollectionsLibTerms[L, Target]]
}

object CollectionsGeneratorLoader {
  def collectionsLoader: ServiceLoader[CollectionsGeneratorLoader] = ServiceLoader.load(classOf[CollectionsGeneratorLoader])
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def load[L <: LA](params: Set[String], error: MissingDependency)(implicit tt: TypeTag[Target[L]]): Target[CollectionsLibTerms[L, Target]] = {
    val found = collectionsLoader
      .iterator()
      .asScala
      .filter(_.reified == tt)
      .flatMap(_.apply(params).asInstanceOf[Option[CollectionsLibTerms[L, Target]]])
      .toSeq
      .headOption

    Target.fromOption(found, error)
  }
}
