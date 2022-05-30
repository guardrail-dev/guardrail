package dev.guardrail.generators.spi

import dev.guardrail.Target
import dev.guardrail.generators.Framework
import dev.guardrail.generators.SwaggerGenerator
import dev.guardrail.languages.LA
import dev.guardrail.{ MissingDependency, Target }
import java.util.ServiceLoader
import scala.jdk.CollectionConverters._
import scala.reflect.runtime.universe.TypeTag

trait FrameworkLoader {
  type L <: LA
  def reified: TypeTag[Target[L]]

  def apply(modules: Set[String]): Option[Framework[L, Target]] =
    (for {
      client      <- ClientGeneratorLoader.load[L](modules, MissingDependency(modules.mkString(", ")))(reified)
      server      <- ServerGeneratorLoader.load[L](modules, MissingDependency(modules.mkString(", ")))(reified)
      framework   <- FrameworkGeneratorLoader.load[L](modules, MissingDependency(modules.mkString(", ")))(reified)
      collections <- CollectionsGeneratorLoader.load[L](modules, MissingDependency(modules.mkString(", ")))(reified)
      protocol    <- ProtocolGeneratorLoader.load[L](modules, MissingDependency(modules.mkString(", ")))(reified)
      language    <- LanguageLoader.load[L](modules, MissingDependency(modules.mkString(", ")))(reified)
    } yield new Framework[L, Target] {
      override implicit def ClientInterp         = client
      override implicit def FrameworkInterp      = framework
      override implicit def ProtocolInterp       = protocol
      override implicit def ServerInterp         = server
      override implicit def SwaggerInterp        = SwaggerGenerator[L]()
      override implicit def LanguageInterp       = language
      override implicit def CollectionsLibInterp = collections
    }).map(Option.apply _)
      .valueOr { err =>
        Console.err.println(err); None
      }
}

object FrameworkLoader {
  def loader: ServiceLoader[FrameworkLoader] = ServiceLoader.load(classOf[FrameworkLoader])
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def load[L <: LA](params: Set[String], error: MissingDependency)(implicit tt: TypeTag[Target[L]]): Target[Framework[L, Target]] = {
    val found = loader
      .iterator()
      .asScala
      .filter(_.reified.tpe =:= tt.tpe)
      .flatMap(_.apply(params).asInstanceOf[Option[Framework[L, Target]]])
      .toSeq
      .headOption

    Target.fromOption(found, error)
  }
}
