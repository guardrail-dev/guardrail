package core.issues

import io.circe._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.SpanSugar._
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Changes
  *
  * - Server request body validation
  * - Client responses
  *   - No content vs Partial content vs Invalid content
  * - Polymorphic discriminator error messages
  */
class Issue148Suite extends AnyFunSuite with Matchers with EitherValues with ScalaFutures {
  override implicit val patienceConfig = PatienceConfig(10 seconds, 1 second)

  test("http4s server request body validation") {
    import cats.effect.IO
    import issues.issue148.server.http4s.definitions._
    import issues.issue148.server.http4s._
    import org.http4s._
    import org.http4s.client.Client
    import org.http4s.headers._
    import org.http4s.implicits._
    import org.http4s.multipart._

    val route = new Resource[IO]().routes(new Handler[IO] {
      override def createFoo(respond: CreateFooResponse.type)(body: Foo, xHeader: Boolean, xOptionalHeader: Option[Boolean]): IO[CreateFooResponse] =
        IO.pure(respond.Ok(body))
      override def getFoo(respond: GetFooResponse.type)(): IO[GetFooResponse] =
        IO.pure(respond.Ok(Bar("bar")))
      override def updateFoo(respond: UpdateFooResponse.type)(name: Boolean, bar: Option[Boolean]): IO[UpdateFooResponse] =
        IO.pure(respond.Accepted(Bar("bar")))
    })

    val client = Client.fromHttpApp[IO](route.orNotFound)
    def failedResponseBody(req: Request[IO]): String =
      client
        .run(req)
        .use({
          case Status.BadRequest(resp) =>
            resp.as[String]
          case Status.UnprocessableEntity(resp) =>
            resp.as[String]
        })
        .unsafeRunSync()

    def makeJsonRequest(body: String): Request[IO] =
      Request[IO](method = Method.POST, uri = Uri.unsafeFromString("/test"))
        .withEntity(body)(
          EntityEncoder[IO, String]
            .withContentType(`Content-Type`(MediaType.application.json).withCharset(DefaultCharset))
        )

    def makeFormRequest(body: Multipart[IO]): Request[IO] =
      Request[IO](method = Method.PUT, uri = Uri.unsafeFromString("/test"))
        .withEntity(body)

    /* Correct mime type
     * Missing content
     */
    failedResponseBody(makeJsonRequest("")) should equal("The request body was malformed.")

    /* Correct mime type
     * Invalid JSON
     */
    failedResponseBody(makeJsonRequest("{")) should equal("The request body was malformed.")

    /* Correct mime type
     * Valid JSON
     * Missing discriminator
     */
    failedResponseBody(makeJsonRequest("{}")) should equal("The request body was invalid.")

    /* Correct mime type
     * Valid JSON
     * Invalid discriminator
     */
    failedResponseBody(makeJsonRequest("""{"type": "blep"}""")) should equal("The request body was invalid.")

    /* Correct mime type
     * Valid JSON
     * Valid discriminator
     * Missing "name" field
     */
    failedResponseBody(makeJsonRequest("""{"type": "Bar"}""")) should equal("The request body was invalid.")

    val validEntity = """{"type": "Bar", "name": "bar"}"""

    /* Correct mime type
     * Valid JSON
     * Valid discriminator
     * Valid "name" field
     * Invalid "x-header" value
     */
    failedResponseBody(
      makeJsonRequest(validEntity).withHeaders(
        Headers(
          Header("x-header", "foo")
        )
      )
    ) should equal("Invalid data")

    /* Correct mime type
     * Valid JSON
     * Valid discriminator
     * Valid "name" field
     * Valid "x-header" value
     * Invalid "x-optional-header" value
     */
    // TODO: https://github.com/guardrail-dev/guardrail/issues/155
    // `x-header` is currently never parsed correctly due to #155
    // Once this is fixed, this test will still fail due to `x-optional-header`,
    // but this is intentional for this test case.
    failedResponseBody(
      makeJsonRequest(validEntity).withHeaders(
        Headers(
          Header("x-header", "false"),
          Header("x-optional-header", "foo")
        )
      )
    ) should equal("Invalid data")

    /* Correct entity mime type
     * Invalid mime type for "foo" body part
     */
    // TODO: https://github.com/guardrail-dev/guardrail/issues/155
    failedResponseBody(
      makeFormRequest(
        Multipart(
          Vector(
            Part.formData[IO]("foo", "blep")
          )
        )
      )
    ) should equal("The request body was malformed.")

    /* Correct entity mime type
     * Valid mime type for "foo" body part
     * Invalid content for "foo" body part
     */
    // TODO: https://github.com/guardrail-dev/guardrail/issues/155
    failedResponseBody(
      makeFormRequest(
        Multipart(
          Vector(
            Part.formData[IO]("foo", "blep", Header("Content-Type", "application/json"))
          )
        )
      )
    ) should equal("The request body was malformed.")

    /* Correct entity mime type
     * Valid mime type for "foo" body part
     * Valid content for "foo" body part
     * Invalid mime type for "bar" body part
     */
    // TODO: https://github.com/guardrail-dev/guardrail/issues/155
    failedResponseBody(
      makeFormRequest(
        Multipart(
          Vector(
            Part.formData[IO]("foo", "false", Header("Content-Type", "application/json")),
            Part.formData[IO]("bar", "blep")
          )
        )
      )
    ) should equal("The request body was malformed.")

    /* Correct entity mime type
     * Valid mime type for "foo" body part
     * Valid content for "foo" body part
     * Valid mime type for "bar" body part
     * Invalid content for "bar" body part
     */
    // TODO: https://github.com/guardrail-dev/guardrail/issues/155
    failedResponseBody(
      makeFormRequest(
        Multipart(
          Vector(
            Part.formData[IO]("foo", "false", Header("Content-Type", "application/json")),
            Part.formData[IO]("bar", "blep", Header("Content-Type", "application/json"))
          )
        )
      )
    ) should equal("The request body was malformed.")
  }

  test("http4s client response body validation") {
    import cats.data.Kleisli
    import cats.effect.IO
    import issues.issue148.client.http4s.Client
    import org.http4s._
    import org.http4s.client.{ Client => Http4sClient }
    import org.http4s.headers._

    def jsonResponse(str: String): Http4sClient[IO] =
      Http4sClient.fromHttpApp[IO](
        Kleisli.pure(
          Response[IO](Status.Ok)
            .withEntity(str)(
              EntityEncoder[IO, String]
                .withContentType(`Content-Type`(MediaType.application.json).withCharset(DefaultCharset))
            )
        )
      )

    /* Correct mime type
     * Missing content
     */
    Client.httpClient(jsonResponse(""), "http://localhost:80").getFoo().attempt.unsafeRunSync() match {
      case Left(MalformedMessageBodyFailure(details, cause)) => details should equal("Invalid JSON: empty body")
      case ex                                                => fail(s"Unknown: ${ex}")
    }

    /* Correct mime type
     * Invalid JSON
     */
    Client.httpClient(jsonResponse("{"), "http://localhost:80").getFoo().attempt.unsafeRunSync() match {
      case Left(MalformedMessageBodyFailure(details, cause)) => details should equal("Invalid JSON")
      case ex                                                => fail(s"Unknown: ${ex}")
    }

    /* Correct mime type
     * Valid JSON
     * Missing discriminator
     */
    Client.httpClient(jsonResponse("{}"), "http://localhost:80").getFoo().attempt.unsafeRunSync() match {
      case Left(InvalidMessageBodyFailure(_, Some(DecodingFailure(message, history)))) =>
        message shouldBe "Attempt to decode value on failed cursor"
        history should equal(List(CursorOp.DownField("type")))
      case ex => fail(s"Unknown: ${ex}")
    }

    /* Correct mime type
     * Valid JSON
     * Invalid discriminator
     */
    Client.httpClient(jsonResponse("""{"type": "blep"}"""), "http://localhost:80").getFoo().attempt.unsafeRunSync() match {
      case Left(InvalidMessageBodyFailure(_, Some(DecodingFailure(message, history)))) =>
        message shouldBe "Unknown value blep (valid: Bar)"
        history should equal(List(CursorOp.DownField("type")))
      case ex => fail(s"Unknown: ${ex}")
    }

    /* Correct mime type
     * Valid JSON
     * Valid discriminator
     * Missing "name" field
     */
    Client.httpClient(jsonResponse("""{"type": "Bar"}"""), "http://localhost:80").getFoo().attempt.unsafeRunSync() match {
      case Left(InvalidMessageBodyFailure(_, Some(DecodingFailure(message, history)))) =>
        message shouldBe "Attempt to decode value on failed cursor"
        history should equal(List(CursorOp.DownField("name")))
      case ex => fail(s"Unknown: ${ex}")
    }
  }
}
