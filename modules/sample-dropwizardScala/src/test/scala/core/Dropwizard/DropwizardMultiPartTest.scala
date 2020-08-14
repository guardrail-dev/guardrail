package core.Dropwizard

import cats.implicits._
import helpers.ScalaHelpers._
import helpers.{ ResourceTestSupport, ScalaHelpers }
import io.dropwizard.testing.common.Resource
import java.time.{ OffsetDateTime, ZoneOffset }
import javax.ws.rs.client.Entity
import multipartFormData.server.dropwizardScala.definitions.Foo
import multipartFormData.server.dropwizardScala.foo.{ FooHandler, FooResource }
import org.glassfish.jersey.media.multipart.{ FormDataMultiPart, MultiPartFeature }
import org.glassfish.jersey.test.grizzly.GrizzlyTestContainerFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class DropwizardMultiPartTest extends AnyFreeSpec with ResourceTestSupport with Matchers {
  private val fooHandler = new FooHandler {
    override def doFoo(
        respond: FooResource.DoFooResponse.type
    )(id: Long, date: OffsetDateTime, optionalDate: Option[OffsetDateTime]): Future[FooResource.DoFooResponse] =
      respond.Created(Foo(id, date, optionalDate)).pure[Future]
  }

  override def buildResource: Resource =
    ResourceTestSupport
      .Builder()
      .setTestContainerFactory(new GrizzlyTestContainerFactory)
      .setMapper(ScalaHelpers.createObjectMapper())
      .addScalaProviders()
      .addProvider(new MultiPartFeature)
      .addResource(new FooResource(fooHandler))
      .build()

  "Multi-part form data with jsr310 date-times works" in {
    val now: OffsetDateTime = OffsetDateTime.now.withOffsetSameInstant(ZoneOffset.UTC)
    val multiPart: FormDataMultiPart = new FormDataMultiPart()
      .field("id", "42")
      .field("date", now.toString)
      .field("optional_date", now.toString)
    val response = resource
      .target("/foo")
      .register(new MultiPartFeature, -1)
      .request
      .post(Entity.entity(multiPart, multiPart.getMediaType))
      .readEntity(classOf[Foo])
    response.id mustBe 42
    response.date mustBe now
    response.optionalDate mustBe Some(now)
  }
}
