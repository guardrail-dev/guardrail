package dev.guardrail.generators.spi

import cats.implicits._
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
    (
      ClientGeneratorLoader.load[L](modules)(reified),
      ServerGeneratorLoader.load[L](modules)(reified),
      FrameworkGeneratorLoader.load[L](modules)(reified),
      CollectionsGeneratorLoader.load[L](modules)(reified),
      ProtocolGeneratorLoader.load[L](modules)(reified),
      LanguageLoader.load[L](modules)(reified)
    ).mapN((client, server, framework, collections, protocol, language) =>
      new Framework[L, Target] {
        override implicit def ClientInterp         = client
        override implicit def FrameworkInterp      = framework
        override implicit def ProtocolInterp       = protocol
        override implicit def ServerInterp         = server
        override implicit def SwaggerInterp        = SwaggerGenerator[L]()
        override implicit def LanguageInterp       = language
        override implicit def CollectionsLibInterp = collections
      }
    ) match {
      case fail: ModuleLoadFailed =>
        Console.err.println(fail)
        None
      case succ: ModuleLoadSuccess[Framework[L, Target]] =>
        Some(succ.result)
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
