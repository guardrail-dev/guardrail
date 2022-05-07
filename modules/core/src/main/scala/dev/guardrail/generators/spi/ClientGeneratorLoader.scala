package dev.guardrail.generators.spi

import dev.guardrail.{ MissingDependency, Target }
import dev.guardrail.languages.LA
import dev.guardrail.terms.client.ClientTerms
import java.util.ServiceLoader
import scala.jdk.CollectionConverters._
import scala.reflect.runtime.universe.TypeTag

trait ClientGeneratorLoader {
  type L <: LA
  def reified: TypeTag[Target[L]]
  def apply(parameters: Set[String]): Option[ClientTerms[L, Target]]
}

object ClientGeneratorLoader {
  def clientLoader: ServiceLoader[ClientGeneratorLoader] = ServiceLoader.load(classOf[ClientGeneratorLoader])
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def load[L <: LA](params: Set[String], error: MissingDependency)(implicit tt: TypeTag[Target[L]]): Target[ClientTerms[L, Target]] = {
    val found = clientLoader
      .iterator()
      .asScala
      .filter(_.reified == tt)
      .flatMap(_.apply(params).asInstanceOf[Option[ClientTerms[L, Target]]])
      .toSeq
      .headOption

    Target.fromOption(found, error)
  }
}
