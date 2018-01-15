package com.twilio.swagger.codegen
package terms

import cats.free.{Free, Inject}
import scala.meta._

class ScalaTerms[F[_]](implicit I: Inject[ScalaTerm, F]) {
  def renderImplicits(pkgName: List[String], frameworkImports: List[Import], jsonImports: List[Import], customImports: List[Import]): Free[F, Source] =
    Free.inject[ScalaTerm, F](RenderImplicits(pkgName, frameworkImports, jsonImports, customImports))
  def renderFrameworkImplicits(pkgName: List[String], frameworkImports: List[Import], jsonImports: List[Import], frameworkImplicits: Defn.Object): Free[F, Source] =
    Free.inject[ScalaTerm, F](RenderFrameworkImplicits(pkgName, frameworkImports, jsonImports, frameworkImplicits))
}
object ScalaTerms {
  implicit def scalaTerm[F[_]](implicit I: Inject[ScalaTerm, F]): ScalaTerms[F] = new ScalaTerms[F]
}
