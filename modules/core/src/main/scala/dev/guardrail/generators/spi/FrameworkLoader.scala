package dev.guardrail.generators.spi

import dev.guardrail.generators.Framework
import dev.guardrail.generators.SwaggerGenerator
import dev.guardrail.{ MissingDependency, Target }
import java.util.ServiceLoader

trait FrameworkLoader extends AbstractGeneratorLoader[Framework, Set[String]] {
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

object FrameworkLoader extends AbstractGeneratorLoaderCompanion[Framework, Set[String], FrameworkLoader] {
  @deprecated("Deprecated in favor of an abstract 'loader' member", "0.71.2")
  def frameworkLoader: ServiceLoader[FrameworkLoader] = loader
}
