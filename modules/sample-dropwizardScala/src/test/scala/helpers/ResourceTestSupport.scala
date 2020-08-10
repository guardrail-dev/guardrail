package helpers

import com.fasterxml.jackson.databind.ObjectMapper
import io.dropwizard.testing.common.Resource
import java.util.function.Consumer
import javax.validation.Validator
import javax.ws.rs.client.{ Client, WebTarget }
import org.glassfish.jersey.client.ClientConfig
import org.glassfish.jersey.test.{ JerseyTest, TestProperties }
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.util.control.NonFatal

object ResourceTestSupport {
  class Builder extends Resource.Builder[Builder] {
    def build(): Resource = buildResource()
  }
  object Builder {
    def apply(): Builder = new Builder
  }
}

trait ResourceTestSupport extends AnyFreeSpec with BeforeAndAfterAll {
  def buildResource: Resource

  protected lazy val resource: Resource = buildResource

  def validator: Validator                       = resource.getValidator
  def objectMapper: ObjectMapper                 = resource.getObjectMapper
  def clientConfigurator: Consumer[ClientConfig] = resource.getClientConfigurator
  def target(path: String): WebTarget            = resource.target(path)
  def client: Client                             = resource.client
  def jerseyTest: JerseyTest                     = resource.getJerseyTest

  override protected def beforeAll(): Unit = {
    System.setProperty(TestProperties.CONTAINER_PORT, "0")
    try resource.before()
    catch {
      case NonFatal(e) =>
        resource.after()
        throw e
    }
  }

  override protected def afterAll(): Unit = resource.after()
}
