package core.Dropwizard

import com.github.tomakehurst.wiremock.client.WireMock._
import helpers.WireMockSupport
import org.scalatest.{ FreeSpec, Matchers }
import tests.contentTypes.textPlain.client.dropwizard.foo.FooClient

class AsyncHttpClientContentHeaderTest extends FreeSpec with Matchers with WireMockSupport {
  private val MOCK_REQUEST_BODY  = "abcd"
  private val MOCK_RESPONSE_BODY = "efgh"

  "AsyncHttpClient with text/plain" - {
    "in both req/resp bodies has Content-Type and Accept headers set" in {
      wireMockServer.stubFor(
        post(urlPathEqualTo("/baz"))
          .withHeader("Content-Type", equalTo("text/plain; charset=utf-8"))
          .withHeader("Accept", equalTo("text/plain; q=1.0, */*; q=0.1"))
          .withRequestBody(equalTo(MOCK_REQUEST_BODY))
          .willReturn(
            aResponse()
              .withStatus(201)
              .withHeader("Content-Type", "text/plain; charset=utf-8")
              .withBody(MOCK_RESPONSE_BODY)
          )
      )

      val client = new FooClient.Builder().withBaseUrl(wireMockBaseUrl).build()
      client
        .doBaz()
        .withBody(MOCK_REQUEST_BODY)
        .call()
        .toCompletableFuture
        .get()
        .fold(
          { body =>
            body shouldBe MOCK_RESPONSE_BODY; ()
          }, { () =>
            fail("Should have gotten 2xx response"); ()
          }
        )
    }

    "in req body has Content-Type header set" in {
      wireMockServer.stubFor(
        post(urlPathEqualTo("/foo"))
          .withHeader("Content-Type", equalTo("text/plain; charset=utf-8"))
          .withRequestBody(equalTo(MOCK_REQUEST_BODY))
          .willReturn(
            aResponse()
              .withStatus(201)
          )
      )
      val client = new FooClient.Builder().withBaseUrl(wireMockBaseUrl).build()
      client
        .doFoo(MOCK_REQUEST_BODY)
        .call()
        .toCompletableFuture
        .get()
        .fold(
          () => (), { () =>
            fail("Should have gotten 201 response"); ()
          }
        )
    }
  }
}
