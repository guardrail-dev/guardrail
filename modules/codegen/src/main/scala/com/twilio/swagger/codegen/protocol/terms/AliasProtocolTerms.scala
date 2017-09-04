package com.twilio.swagger.codegen
package terms.protocol

import cats.free.{Free, Inject}
import scala.meta._

class AliasProtocolTerms[F[_]](implicit I: Inject[AliasProtocolTerm, F]) {
  def renderAlias(clsName: String, tpe: Type): Free[F, Defn] =
    Free.inject[AliasProtocolTerm, F](RenderAlias(clsName, tpe))
  def renderAliasCompanion(clsName: String): Free[F, Defn.Object] =
    Free.inject[AliasProtocolTerm, F](RenderAliasCompanion(clsName))
}
object AliasProtocolTerms {
  implicit def aliasProtocolTerm[F[_]](implicit I: Inject[AliasProtocolTerm, F]): AliasProtocolTerms[F] = new AliasProtocolTerms[F]
}
