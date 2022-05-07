package dev.guardrail.generators.spi

import dev.guardrail.{ MissingDependency, Target }
import dev.guardrail.languages.LA
import dev.guardrail.terms.framework.FrameworkTerms
import java.util.ServiceLoader
import scala.jdk.CollectionConverters._
import scala.reflect.runtime.universe.TypeTag

trait FrameworkGeneratorLoader {
  type L <: LA
  def reified: TypeTag[Target[L]]
  def apply(parameters: Set[String]): Option[FrameworkTerms[L, Target]]
}

object FrameworkGeneratorLoader {
  def frameworkLoader: ServiceLoader[FrameworkGeneratorLoader] = ServiceLoader.load(classOf[FrameworkGeneratorLoader])
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def load[L <: LA](params: Set[String], error: MissingDependency)(implicit tt: TypeTag[Target[L]]): Target[FrameworkTerms[L, Target]] = {
    val found = frameworkLoader
      .iterator()
      .asScala
      .filter(_.reified == tt)
      .flatMap(_.apply(params).asInstanceOf[Option[FrameworkTerms[L, Target]]])
      .toSeq
      .headOption

    Target.fromOption(found, error)
  }
}
