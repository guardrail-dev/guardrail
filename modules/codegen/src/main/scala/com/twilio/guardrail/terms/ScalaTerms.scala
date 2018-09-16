package com.twilio.guardrail
package terms

import cats.InjectK
import cats.free.Free

import scala.meta._

class ScalaTerms[F[_]](implicit I: InjectK[ScalaTerm, F]) {
  def renderImplicits(
      pkgName: List[String],
      frameworkImports: List[Import],
      jsonImports: List[Import],
      customImports: List[Import]
  ): Free[F, Source] =
    Free.inject[ScalaTerm, F](RenderImplicits(pkgName, frameworkImports, jsonImports, customImports))
  def renderFrameworkImplicits(
      pkgName: List[String],
      frameworkImports: List[Import],
      jsonImports: List[Import],
      frameworkImplicits: Defn.Object
  ): Free[F, Source] =
    Free.inject[ScalaTerm, F](RenderFrameworkImplicits(pkgName, frameworkImports, jsonImports, frameworkImplicits))
}
object ScalaTerms {
  implicit def scalaTerm[F[_]](implicit I: InjectK[ScalaTerm, F]): ScalaTerms[F] = new ScalaTerms[F]
}
