package dev.guardrail.generators.spi

import dev.guardrail.languages.LA
import dev.guardrail.terms.LanguageTerms
import dev.guardrail.{ MissingDependency, Target }

import java.util.ServiceLoader
import scala.jdk.CollectionConverters._
import scala.reflect.runtime.universe.TypeTag

trait LanguageLoader {
  type L <: LA
  def reified: TypeTag[Target[L]]
  def apply(parameters: Set[String]): Option[LanguageTerms[L, Target]]
}

object LanguageLoader {
  def frameworkLoader: ServiceLoader[LanguageLoader] = ServiceLoader.load(classOf[LanguageLoader])
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def load[L <: LA](params: Set[String], error: MissingDependency)(implicit tt: TypeTag[Target[L]]): Target[LanguageTerms[L, Target]] = {
    val found = frameworkLoader
      .iterator()
      .asScala
      .filter(_.reified == tt)
      .flatMap(_.apply(params).asInstanceOf[Option[LanguageTerms[L, Target]]])
      .toSeq
      .headOption

    Target.fromOption(found, error)
  }
}
