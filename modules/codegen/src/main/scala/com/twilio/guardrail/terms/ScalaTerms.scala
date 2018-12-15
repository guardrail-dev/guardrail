package com.twilio.guardrail
package terms

import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.languages.LA
import java.nio.file.Path

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

  def writePackageObject(dtoPackagePath: Path,
                         dtoComponents: List[String],
                         customImports: List[L#Import],
                         packageObjectImports: List[L#Import],
                         protocolImports: List[L#Import],
                         packageObjectContents: List[L#ValueDefinition],
                         extraTypes: List[L#Statement]): Free[F, WriteTree] =
    Free.inject[ScalaTerm[L, ?], F](
      WritePackageObject(dtoPackagePath, dtoComponents, customImports, packageObjectImports, protocolImports, packageObjectContents, extraTypes)
    )
}
object ScalaTerms {
  implicit def scalaTerm[L <: LA, F[_]](implicit I: InjectK[ScalaTerm[L, ?], F]): ScalaTerms[L, F] = new ScalaTerms[L, F]
}
