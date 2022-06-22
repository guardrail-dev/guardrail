package dev.guardrail.generators.spi

import cats.data.NonEmptyList
import dev.guardrail.languages.LA
import dev.guardrail.Target
import java.util.ServiceLoader
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

trait AbstractGeneratorLoader[A[_ <: LA, _[_]]] {
  type L <: LA
  def reified: TypeTag[Target[L]]
  def apply: Set[String] => ModuleLoadResult[A[L, Target]]
}

abstract class AbstractGeneratorLoaderCompanion[A[_ <: LA, _[_]], B <: AbstractGeneratorLoader[A]](implicit ct: ClassTag[B]) {
  val label: String
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def loader: ServiceLoader[B] = ServiceLoader.load(ct.runtimeClass.asInstanceOf[Class[B]])
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def load[L <: LA](params: Set[String])(implicit tt: TypeTag[Target[L]]): ModuleLoadResult[A[L, Target]] = {
    val found = loader
      .iterator()
      .asScala
      .filter(_.reified.tpe =:= tt.tpe)
      .toList
      .map(_.apply(params).asInstanceOf[ModuleLoadResult[A[L, Target]]])

    // TODO: Can we alert if there are multiple matches for the same slot?
    NonEmptyList.fromList(found).fold[ModuleLoadResult[A[L, Target]]](new ModuleLoadFailed(Set.empty, Set(label)))(_.reduce)
  }
}
