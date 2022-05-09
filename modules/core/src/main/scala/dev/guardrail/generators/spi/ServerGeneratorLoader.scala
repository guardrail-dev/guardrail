package dev.guardrail.generators.spi

import dev.guardrail.{ MissingDependency, Target }
import dev.guardrail.languages.LA
import dev.guardrail.terms.server.ServerTerms
import java.util.ServiceLoader
import scala.jdk.CollectionConverters._
import scala.reflect.runtime.universe.TypeTag

trait ServerGeneratorLoader {
  type L <: LA
  def reified: TypeTag[Target[L]]
  def apply(parameters: Set[String]): Option[ServerTerms[L, Target]]
}

object ServerGeneratorLoader {
  def serverLoader: ServiceLoader[ServerGeneratorLoader] = ServiceLoader.load(classOf[ServerGeneratorLoader])
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def load[L <: LA](params: Set[String], error: MissingDependency)(implicit tt: TypeTag[Target[L]]): Target[ServerTerms[L, Target]] = {
    val found = serverLoader
      .iterator()
      .asScala
      .filter(_.reified == tt)
      .flatMap(_.apply(params).asInstanceOf[Option[ServerTerms[L, Target]]])
      .toSeq
      .headOption

    Target.fromOption(found, error)
  }
}
