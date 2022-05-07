package core.issues

import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.unmarshalling.Unmarshaller
import com.fasterxml.jackson.core.io.JsonEOFException
import core.TestImplicits
import issues.issue148.server.akkaHttpJackson.Resource
import javax.validation.ValidationException
import org.scalatest.EitherValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.SpanSugar._
import scala.concurrent.Future

/** Changes
  *
  *   - Server request body validation
  *   - Client responses
  *     - No content vs Partial content vs Invalid content
  *   - Polymorphic discriminator error messages
  */
class Issue148Suite extends AnyFunSuite with TestImplicits with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest {
  override implicit val patienceConfig: PatienceConfig = PatienceConfig(10.seconds, 1.second)

  test("akka-http server request body validation") {
    import issues.issue148.server.akkaHttpJackson.Handler
    import issues.issue148.server.akkaHttpJackson.definitions._
    val route = Resource.routes(new Handler {
      override def createFoo(
          respond: Resource.CreateFooResponse.type
      )(body: Foo, xHeader: Boolean, xOptionalHeader: Option[Boolean]): Future[Resource.CreateFooResponse] =
        Future.successful(respond.OK(body))
      override def getFoo(respond: Resource.GetFooResponse.type)(): Future[Resource.GetFooResponse] =
        Future.successful(respond.OK(Bar("bar")))
      override def updateFoo(respond: Resource.UpdateFooResponse.type)(name: Boolean, bar: Option[Boolean]): Future[Resource.UpdateFooResponse] =
        Future.successful(respond.Accepted(Bar("bar")))
    })

    /* Correct mime type
     * Missing header
     */
    Post("/test") ~> route ~> check {
      rejection match {
        case MissingHeaderRejection("x-header") => ()
      }
    }

    /* Correct mime type
     * Valid "x-header" value
     * Missing content
     */
    Post("/test")
      .withHeaders(RawHeader("x-header", "false"))
      .withEntity(ContentTypes.`application/json`, "") ~> route ~> check {
      rejection match {
        case RequestEntityExpectedRejection => ()
      }
    }

    /* Correct mime type
     * Valid "x-header" value
     * Invalid JSON
     */
    Post("/test")
      .withHeaders(RawHeader("x-header", "false"))
      .withEntity(ContentTypes.`application/json`, "{") ~> route ~> check {
      rejection match {
        case ex: MalformedRequestContentRejection => ex.message should startWith("Unexpected end-of-input")
      }
    }

    /* Correct mime type
     * Valid "x-header" value
     * Valid JSON
     * Missing discriminator
     */
    Post("/test")
      .withHeaders(RawHeader("x-header", "false"))
      .withEntity(ContentTypes.`application/json`, "{}") ~> route ~> check {
      rejection match {
        case ex: ValidationRejection => ex.message should include("missing type id property 'type'")
      }
    }

    /* Correct mime type
     * Valid "x-header" value
     * Valid JSON
     * Invalid discriminator
     */
    Post("/test")
      .withHeaders(RawHeader("x-header", "false"))
      .withEntity(ContentTypes.`application/json`, """{"type": "blep"}""") ~> route ~> check {
      rejection match {
        case ex: ValidationRejection => ex.message should startWith("Could not resolve type id 'blep' as a subtype")
      }
    }

    /* Correct mime type
     * Valid "x-header" value
     * Valid JSON
     * Valid discriminator
     * Missing "name" field
     */
    Post("/test")
      .withHeaders(RawHeader("x-header", "false"))
      .withEntity(ContentTypes.`application/json`, """{"type": "Bar"}""") ~> route ~> check {
      rejection match {
        case ex: MalformedRequestContentRejection => ex.message shouldBe "Validation of Bar failed: name: must not be null"
      }
    }

    val validEntity = """{"type": "Bar", "name": "bar"}"""

    /* Correct mime type
     * Valid JSON
     * Valid discriminator
     * Valid "name" field
     * Invalid "x-header" value
     */
    Post("/test")
      .withEntity(ContentTypes.`application/json`, validEntity)
      .withHeaders(RawHeader("x-header", "foo")) ~> route ~> check {
      rejection match {
        case MalformedHeaderRejection("x-header", message, _) => message shouldBe "'foo' is not a valid Boolean value"
      }
    }

    /* Correct mime type
     * Valid JSON
     * Valid discriminator
     * Valid "name" field
     * Valid "x-header" value
     * Invalid "x-optional-header" value
     */
    Post("/test")
      .withEntity(ContentTypes.`application/json`, validEntity)
      .withHeaders(RawHeader("x-header", "false"), RawHeader("x-optional-header", "foo")) ~> route ~> check {
      rejection match {
        case MalformedHeaderRejection("x-optional-header", message, _) => message shouldBe "'foo' is not a valid Boolean value"
      }
    }

    /* Correct entity mime type
     * Invalid mime type for "foo" body part
     */
    Put("/test")
      .withEntity(
        Multipart
          .FormData(
            Multipart.FormData.BodyPart.Strict("foo", "blep")
          )
          .toEntity
      ) ~> route ~> check {
      rejection match {
        case MalformedFormFieldRejection("foo", message, _) => message should startWith("Unrecognized token 'blep'")
      }
    }

    /* Correct entity mime type
     * Valid mime type for "foo" body part
     * Invalid content for "foo" body part
     */
    Put("/test")
      .withEntity(
        Multipart
          .FormData(
            Multipart.FormData.BodyPart.Strict("foo", HttpEntity(ContentTypes.`application/json`, "blep"))
          )
          .toEntity
      ) ~> route ~> check {
      rejection match {
        case MalformedFormFieldRejection("foo", message, _) => message should startWith("Unrecognized token 'blep'")
      }
    }

    /* Correct entity mime type
     * Valid mime type for "foo" body part
     * Valid content for "foo" body part
     * Invalid mime type for "bar" body part
     */
    Put("/test")
      .withEntity(
        Multipart
          .FormData(
            Multipart.FormData.BodyPart.Strict("foo", HttpEntity(ContentTypes.`application/json`, "false")),
            Multipart.FormData.BodyPart.Strict("bar", "blep")
          )
          .toEntity
      ) ~> route ~> check {
      rejection match {
        case MalformedFormFieldRejection("bar", message, _) => message should startWith("Unrecognized token 'blep'")
      }
    }

    /* Correct entity mime type
     * Valid mime type for "foo" body part
     * Valid content for "foo" body part
     * Valid mime type for "bar" body part
     * Invalid content for "bar" body part
     */
    Put("/test")
      .withEntity(
        Multipart
          .FormData(
            Multipart.FormData.BodyPart.Strict("foo", HttpEntity(ContentTypes.`application/json`, "false")),
            Multipart.FormData.BodyPart.Strict("bar", HttpEntity(ContentTypes.`application/json`, "blep"))
          )
          .toEntity
      ) ~> route ~> check {
      rejection match {
        case MalformedFormFieldRejection("bar", message, _) => message should startWith("Unrecognized token 'blep'")
      }
    }
  }

  test("akka-http client response body validation") {
    import issues.issue148.client.akkaHttpJackson.Client

    def jsonResponse(str: String): HttpRequest => Future[HttpResponse] =
      _ => Future.successful(HttpResponse(200).withEntity(ContentTypes.`application/json`, str))

    /* Correct mime type
     * Missing content
     */
    Client.httpClient(jsonResponse(""), "http://localhost:80").getFoo().value.futureValue match {
      case Left(Left(Unmarshaller.NoContentException)) => ()
      case ex                                          => failTest(s"Unknown: ${ex}")
    }

    /* Correct mime type
     * Invalid JSON
     */
    Client.httpClient(jsonResponse("{"), "http://localhost:80").getFoo().value.futureValue match {
      case Left(Left(e: JsonEOFException)) => e.getMessage should startWith("Unexpected end-of-input")
      case ex                              => failTest(s"Unknown: ${ex}")
    }

    /* Correct mime type
     * Valid JSON
     * Missing discriminator
     */
    Client.httpClient(jsonResponse("{}"), "http://localhost:80").getFoo().value.futureValue match {
      case Left(Left(e: IllegalArgumentException)) =>
        e.getMessage should startWith(
          "Could not resolve subtype of [simple type, class issues.issue148.client.akkaHttpJackson.definitions.Foo]: missing type id property 'type'"
        )
      case ex => failTest(s"Unknown: ${ex}")
    }

    /* Correct mime type
     * Valid JSON
     * Invalid discriminator
     */
    Client.httpClient(jsonResponse("""{"type": "blep"}"""), "http://localhost:80").getFoo().value.futureValue match {
      case Left(Left(e: IllegalArgumentException)) => e.getMessage should startWith("Could not resolve type id 'blep' as a subtype")
      case ex                                      => failTest(s"Unknown: ${ex}")
    }

    /* Correct mime type
     * Valid JSON
     * Valid discriminator
     * Missing "name" field
     */
    Client.httpClient(jsonResponse("""{"type": "Bar"}"""), "http://localhost:80").getFoo().value.futureValue match {
      case Left(Left(e: ValidationException)) => e.getMessage shouldBe "Validation of Bar failed: name: must not be null"
      case ex                                 => failTest(s"Unknown: ${ex}")
    }
  }
}
