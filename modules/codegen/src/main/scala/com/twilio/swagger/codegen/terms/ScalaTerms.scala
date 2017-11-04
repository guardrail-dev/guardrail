package com.twilio.swagger.codegen
package terms

import cats.free.{Free, Inject}
import scala.collection.immutable.Seq
import scala.meta._

class ScalaTerms[F[_]](implicit I: Inject[ScalaTerm, F]) {
  def renderImplicits(pkgName: Seq[String], frameworkImports: Seq[Import], jsonImports: Seq[Import], customImports: Seq[Import]): Free[F, Source] =
    Free.inject[ScalaTerm, F](RenderImplicits(pkgName, frameworkImports, jsonImports, customImports))
}
object ScalaTerms {
  implicit def scalaTerm[F[_]](implicit I: Inject[ScalaTerm, F]): ScalaTerms[F] = new ScalaTerms[F]
}
