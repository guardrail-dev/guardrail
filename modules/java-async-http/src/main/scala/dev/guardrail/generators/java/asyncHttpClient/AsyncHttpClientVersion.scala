package dev.guardrail.generators.java.asyncHttpClient

sealed abstract class AsyncHttpClientVersion(val value: String)
object AsyncHttpClientVersion extends AsyncHttpClientVersion("async-http-client") {
  val mapping: Map[String, AsyncHttpClientVersion] = Map(
    value -> AsyncHttpClientVersion
  )
}
