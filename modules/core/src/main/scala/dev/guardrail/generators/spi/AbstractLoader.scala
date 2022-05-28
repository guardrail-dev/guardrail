package dev.guardrail.generators.spi

import dev.guardrail.languages.LA
import dev.guardrail.{ MissingDependency, Target }
import java.util.ServiceLoader
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

trait AbstractGeneratorLoader[A[_ <: LA, _[_]], P] {
  type L <: LA
  def reified: TypeTag[Target[L]]
  def apply(parameters: P): Option[A[L, Target]]
}

abstract class AbstractGeneratorLoaderCompanion[A[_ <: LA, _[_]], P, B <: AbstractGeneratorLoader[A, P]](implicit ct: ClassTag[B]) {
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def loader: ServiceLoader[B] = ServiceLoader.load(ct.runtimeClass.asInstanceOf[Class[B]])
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def load[L <: LA](params: P, error: MissingDependency)(implicit tt: TypeTag[Target[L]]): Target[A[L, Target]] = {
    val found = loader
      .iterator()
      .asScala
      .filter(_.reified.tpe =:= tt.tpe)
      .flatMap(_.apply(params).asInstanceOf[Option[A[L, Target]]])
      .toSeq
      .headOption

    Target.fromOption(found, error)
  }
}
