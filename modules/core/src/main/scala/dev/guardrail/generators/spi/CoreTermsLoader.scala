package dev.guardrail.generators.spi

import cats.data.NonEmptyList
import dev.guardrail.languages.LA
import dev.guardrail.terms.CoreTerms
import dev.guardrail.{ Args, Common, MissingDependency, ReadSwagger, Target, WriteTree }
import java.util.ServiceLoader
import scala.jdk.CollectionConverters._

trait CoreTermsLoader {
  def runM[L <: LA](parameters: NonEmptyList[Args])(implicit ct: CoreTerms[L, Target]) = Common.runM[L, Target](parameters)
  def apply(language: String, parameters: NonEmptyList[Args]): Option[Function0[Target[NonEmptyList[ReadSwagger[Target[List[WriteTree]]]]]]]
}

object CoreTermsLoader {
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def loader: ServiceLoader[CoreTermsLoader] = ServiceLoader.load(classOf[CoreTermsLoader])
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def load(language: String, parameters: NonEmptyList[Args], error: MissingDependency): Target[NonEmptyList[ReadSwagger[Target[List[WriteTree]]]]] = {
    val found = loader
      .iterator()
      .asScala
      .flatMap(_.apply(language, parameters))
      .toSeq
      .headOption

    Target.fromOption(found, error).flatMap(_.apply())
  }
}
