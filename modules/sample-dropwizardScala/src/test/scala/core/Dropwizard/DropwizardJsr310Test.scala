package core.Dropwizard

import cats.implicits._
import dateTime.server.dropwizardScala.dateTime.{ DateTimeHandler, DateTimeResource }
import helpers.ScalaHelpers.ResourceTestSupportBuilderExtensions
import helpers.{ ResourceTestSupport, ScalaHelpers }
import io.dropwizard.testing.common.Resource
import java.time.{ LocalDate, OffsetDateTime }
import org.glassfish.jersey.test.grizzly.GrizzlyTestContainerFactory
import org.mockito.ArgumentMatchersSugar
import org.mockito.scalatest.IdiomaticMockito
import org.scalatest.BeforeAndAfter
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object DropwizardJsr310Test {
  class DateTimeHandlerImpl extends DateTimeHandler {
    override def getSomething(respond: DateTimeResource.GetSomethingResponse.type)(
        dateTime: OffsetDateTime,
        optionalDateTime: Option[OffsetDateTime],
        date: LocalDate,
        optionalDate: Option[LocalDate]
    ): Future[DateTimeResource.GetSomethingResponse] =
      respond.NoContent.pure[Future]
  }
}

class DropwizardJsr310Test extends AnyFreeSpec with ResourceTestSupport with BeforeAndAfter with IdiomaticMockito with ArgumentMatchersSugar with Matchers {
  import DropwizardJsr310Test._

  private val handler = spy(new DateTimeHandlerImpl)

  override def buildResource: Resource =
    ResourceTestSupport
      .Builder()
      .setTestContainerFactory(new GrizzlyTestContainerFactory)
      .setMapper(ScalaHelpers.createObjectMapper())
      .addScalaProviders()
      .addResource(new DateTimeResource(handler))
      .build()

  before {
    reset(handler)
  }

  "All date-times should be present" in {
    resource
      .target("/foo")
      .queryParam("dateTime", OffsetDateTime.now.toString)
      .queryParam("optionalDateTime", OffsetDateTime.now.toString)
      .queryParam("date", LocalDate.now.toString)
      .queryParam("optionalDate", LocalDate.now.toString)
      .request
      .get
      .getStatus mustBe 204
    handler.getSomething(any)(any, any, any, any) was called
  }

  "Optional date-times can be missing" in {
    resource
      .target("/foo")
      .queryParam("dateTime", OffsetDateTime.now.toString)
      .queryParam("date", LocalDate.now.toString)
      .request
      .get
      .getStatus mustBe 204
    handler.getSomething(any)(any, eqTo(None), any, eqTo(None)) was called
  }

  "All date-times missing should return 400" in {
    resource
      .target("/foo")
      .request
      .get
      .getStatus mustBe 400
    handler.getSomething(any)(any, any, any, any) wasNever called
  }

  "Unparseable date-times should return 400" in {
    resource
      .target("/foo")
      .queryParam("dateTime", "oasdpajdsapojdapsdja")
      .queryParam("date", "asoidajdo290ewjdk")
      .request
      .get
      .getStatus mustBe 400
    handler.getSomething(any)(any, any, any, any) wasNever called
  }
}
