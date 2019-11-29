package helpers

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.core.WireMockConfiguration._
import java.net.URI
import org.scalatest.{ BeforeAndAfterAll, Suite }

trait WireMockSupport extends BeforeAndAfterAll { self: Suite =>
  protected val wireMockServer       = new WireMockServer(wireMockConfig().bindAddress("localhost").dynamicPort())
  protected def wireMockBaseUrl: URI = new URI(s"http://localhost:${wireMockServer.port()}")

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    wireMockServer.start()
  }

  override protected def afterAll(): Unit = {
    wireMockServer.stop()
    super.afterAll()
  }
}
