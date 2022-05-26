package dev.guardrail.generators.spi

import dev.guardrail.{ MissingDependency, Target }
import dev.guardrail.languages.LA
import java.util.ServiceLoader
import scala.jdk.CollectionConverters._
import scala.reflect.runtime.universe.TypeTag

trait ModuleMapperLoader {
  type L <: LA
  def reified: TypeTag[Target[L]]
  def apply(frameworkName: String): Option[Set[String]]
}

object ModuleMapperLoader {
  def moduleMapperLoader: ServiceLoader[ModuleMapperLoader] = ServiceLoader.load(classOf[ModuleMapperLoader])
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def load[L <: LA](frameworkName: String, error: MissingDependency)(implicit tt: TypeTag[Target[L]]): Target[Set[String]] = {
    val found = moduleMapperLoader
      .iterator()
      .asScala
      .filter(_.reified.tpe =:= tt.tpe)
      .flatMap(_.apply(frameworkName).asInstanceOf[Option[Set[String]]])
      .toSeq
      .headOption

    Target.fromOption(found, error)
  }
}
