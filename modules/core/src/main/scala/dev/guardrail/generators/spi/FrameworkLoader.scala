package dev.guardrail.generators.spi

import cats.data.NonEmptyList
import cats.implicits._
import dev.guardrail.{ ModuleConflict, Target, UnspecifiedModules, UnusedModules }
import dev.guardrail.generators.Framework
import dev.guardrail.generators.OpenAPIGenerator
import dev.guardrail.languages.LA
import dev.guardrail.terms
import java.util.ServiceLoader
import scala.jdk.CollectionConverters._
import scala.reflect.runtime.universe.TypeTag

trait FrameworkLoader {
  type L <: LA
  def reified: TypeTag[Target[L]]

  def apply(modules: Set[String]): ModuleLoadResult[Framework[L, Target]] =
    (
      ClientGeneratorLoader.load[L](modules)(reified),
      ServerGeneratorLoader.load[L](modules)(reified),
      FrameworkGeneratorLoader.load[L](modules)(reified),
      CollectionsGeneratorLoader.load[L](modules)(reified),
      ProtocolGeneratorLoader.load[L](modules)(reified),
      LanguageLoader.load[L](modules)(reified)
    ).mapN((client, server, framework, collections, protocol, language) =>
      new Framework[L, Target] {
        override implicit def ClientInterp: terms.client.ClientTerms[L, Target]          = client
        override implicit def FrameworkInterp: terms.framework.FrameworkTerms[L, Target] = framework
        override implicit def ProtocolInterp: terms.ProtocolTerms[L, Target]             = protocol
        override implicit def ServerInterp: terms.server.ServerTerms[L, Target]          = server
        override implicit def SwaggerInterp: terms.OpenAPITerms[L, Target]               = OpenAPIGenerator[L]()
        override implicit def LanguageInterp: terms.LanguageTerms[L, Target]             = language
        override implicit def CollectionsLibInterp: terms.CollectionsLibTerms[L, Target] = collections
      }
    )
}

object FrameworkLoader {
  def loader: ServiceLoader[FrameworkLoader] = ServiceLoader.load(classOf[FrameworkLoader])
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def load[L <: LA](params: Set[String])(implicit tt: TypeTag[Target[L]]): Target[Framework[L, Target]] = {
    val found = loader
      .iterator()
      .asScala
      .filter(_.reified.tpe =:= tt.tpe)
      .map(_.apply(params).asInstanceOf[ModuleLoadResult[Framework[L, Target]]])
      .toList

    NonEmptyList
      .fromList(found)
      .fold[Target[Framework[L, Target]]](
        Target.raiseException(s"No framework loaders found for ${tt}! please report this to https://github.com/guardrail-dev/guardrail")
      )(_.reduce match {
        case fail: ModuleLoadFailed =>
          Target.raiseError(UnspecifiedModules(fail.choices))
        case conflict: ModuleLoadConflict =>
          Target.raiseError(ModuleConflict(conflict.section))
        case succ: ModuleLoadSuccess[Framework[L, Target]] =>
          NonEmptyList.fromList(params.diff(succ.modulesConsumed).toList).fold(Target.pure(succ.result)) { unused =>
            Target.raiseError(UnusedModules(unused))
          }
      })
  }
}
