package dev.guardrail.generators.spi

import dev.guardrail.{ MissingDependency, Target }
import dev.guardrail.languages.LA
import dev.guardrail.terms.ProtocolTerms
import java.util.ServiceLoader
import scala.jdk.CollectionConverters._
import scala.reflect.runtime.universe.TypeTag

trait ProtocolGeneratorLoader {
  type L <: LA
  def reified: TypeTag[Target[L]]
  def apply(parameters: Set[String]): Option[ProtocolTerms[L, Target]]
}

object ProtocolGeneratorLoader {
  def protocolLoader: ServiceLoader[ProtocolGeneratorLoader] = ServiceLoader.load(classOf[ProtocolGeneratorLoader])
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def load[L <: LA](params: Set[String], error: MissingDependency)(implicit tt: TypeTag[Target[L]]): Target[ProtocolTerms[L, Target]] = {
    val found = protocolLoader
      .iterator()
      .asScala
      .filter(_.reified == tt)
      .flatMap(_.apply(params).asInstanceOf[Option[ProtocolTerms[L, Target]]])
      .toSeq
      .headOption

    Target.fromOption(found, error)
  }
}
