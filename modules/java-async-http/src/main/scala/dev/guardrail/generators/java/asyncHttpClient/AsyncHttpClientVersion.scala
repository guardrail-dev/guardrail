package dev.guardrail.generators.java.asyncHttpClient

sealed abstract class AsyncHttpClientVersion(val value: String)
object AsyncHttpClientVersion extends AsyncHttpClientVersion("async-http-client") {
  def unapply(version: String): Option[AsyncHttpClientVersion] = version match {
    case `value` => Some(AsyncHttpClientVersion)
    case _       => None
  }
}
