package com.twilio.guardrail
package terms

import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.languages.LA

class ScalaTerms[L <: LA, F[_]](implicit I: InjectK[ScalaTerm[L, ?], F]) {
  def renderImplicits(pkgName: List[String],
                      frameworkImports: List[L#Import],
                      jsonImports: List[L#Import],
                      customImports: List[L#Import]): Free[F, L#FileContents] =
    Free.inject[ScalaTerm[L, ?], F](RenderImplicits(pkgName, frameworkImports, jsonImports, customImports))
  def renderFrameworkImplicits(pkgName: List[String],
                               frameworkImports: List[L#Import],
                               jsonImports: List[L#Import],
                               frameworkImplicits: L#ObjectDefinition): Free[F, L#FileContents] =
    Free.inject[ScalaTerm[L, ?], F](RenderFrameworkImplicits(pkgName, frameworkImports, jsonImports, frameworkImplicits))
}
object ScalaTerms {
  implicit def scalaTerm[L <: LA, F[_]](implicit I: InjectK[ScalaTerm[L, ?], F]): ScalaTerms[L, F] = new ScalaTerms[L, F]
}
