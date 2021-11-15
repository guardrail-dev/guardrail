package core.Http4s

import java.security.MessageDigest
import java.util.Locale.US

import cats.effect.IO
import cats.effect.IO._
import fs2.Stream
import javax.xml.bind.DatatypeConverter.printHexBinary
import io.circe.Json
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.implicits._
import org.http4s.dsl.io._
import org.http4s.{ HttpRoutes, Method, Request, Uri }
import org.scalatest.exceptions.TestFailedException
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import _root_.enumerations.client.http4s.foo.{ DoFooResponse => ClientDoFooResponse, FooClient }
import _root_.enumerations.client.http4s.{ definitions => cdefs }
import _root_.enumerations.server.http4s.foo._
import _root_.enumerations.server.http4s.foo.FooResource._
import _root_.enumerations.server.http4s.{ definitions => sdefs }

class Http4sNonStringEnumerationTest extends AnyFunSuite with Matchers with EitherValues {

  def cond[A](pred: Boolean, message: String): IO[Unit] =
    if (pred) {
      IO.pure(())
    } else {
      IO.raiseError(new Throwable(message))
    }

  test("round-trip: Ensure enumerations are unpacked correctly") {
    val expectedInt      = cdefs.IntEnum.IntEnum1
    val expectedLong     = cdefs.LongEnum.LongEnum2
    val expectedString   = cdefs.StringEnum.ILikeSpaces
    val expectedBody     = cdefs.IntEnum.IntEnum3
    val expectedResponse = cdefs.StringEnum.ILikeSpaces

    val httpService = new FooResource[IO]().routes(new FooHandler[IO] {
      def doFoo(
          respond: DoFooResponse.type
      )(intEnum: sdefs.IntEnum, longEnum: Option[sdefs.LongEnum], stringEnum: Option[sdefs.StringEnum], body: sdefs.IntEnum): IO[DoFooResponse] =
        for {
          _ <- cond(intEnum.value == expectedInt.value, "intEnum value did not match")
          _ <- cond(longEnum.exists(_.value == expectedLong.value), "longEnum value did not match")
          _ <- cond(stringEnum.exists(_.value == expectedString.value), "stringEnum value did not match")
          _ <- cond(body.value == expectedBody.value, "body value did not match")
        } yield respond.Created(sdefs.StringEnum.ILikeSpaces)
    })

    val fooClient = FooClient.httpClient(Client.fromHttpApp(httpService.orNotFound), "http://localhost:1234")

    fooClient.doFoo(expectedInt, Some(expectedLong), Some(expectedString), expectedBody).attempt.unsafeRunSync() should be(
      Right(ClientDoFooResponse.Created(expectedResponse))
    )
  }

  test("raw client: Ensure enumerations are unpacked correctly") {
    val expectedInt      = cdefs.IntEnum.IntEnum1
    val expectedLong     = cdefs.LongEnum.LongEnum2
    val expectedString   = cdefs.StringEnum.ILikeSpaces
    val expectedBody     = cdefs.IntEnum.IntEnum3
    val expectedResponse = cdefs.StringEnum.ILikeSpaces

    val httpService = new FooResource[IO]().routes(new FooHandler[IO] {
      def doFoo(
          respond: DoFooResponse.type
      )(intEnum: sdefs.IntEnum, longEnum: Option[sdefs.LongEnum], stringEnum: Option[sdefs.StringEnum], body: sdefs.IntEnum): IO[DoFooResponse] =
        for {
          _ <- cond(intEnum.value == expectedInt.value, "intEnum value did not match")
          _ <- cond(longEnum.exists(_.value == expectedLong.value), "longEnum value did not match")
          _ <- cond(stringEnum.exists(_.value == expectedString.value), "stringEnum value did not match")
          _ <- cond(body.value == expectedBody.value, "body value did not match")
        } yield respond.Created(sdefs.StringEnum.ILikeSpaces)
    })

    val client = Client.fromHttpApp(httpService.orNotFound)

    val uri = Uri().withPath(Uri.Path.unsafeFromString("/foo/1")).withQueryParam("longEnum", "2").withQueryParam("stringEnum", "i like spaces")
    client.expect[Json](Request[IO](method = Method.POST, uri = uri).withEntity("3")).attempt.unsafeRunSync() should be(
      Right(Json.fromString(expectedResponse.value))
    )
  }

  object LongParamMatcher   extends OptionalQueryParamDecoderMatcher[Long]("longEnum")
  object StringParamMatcher extends OptionalQueryParamDecoderMatcher[String]("stringEnum")

  test("raw server: Ensure enumerations are unpacked correctly") {
    val expectedInt      = cdefs.IntEnum.IntEnum1
    val expectedLong     = cdefs.LongEnum.LongEnum2
    val expectedString   = cdefs.StringEnum.ILikeSpaces
    val expectedBody     = cdefs.IntEnum.IntEnum3
    val expectedResponse = cdefs.StringEnum.ILikeSpaces

    val httpService = HttpRoutes.of[IO] {
      case req @ Method.POST -> Root / "foo" / intEnum :? LongParamMatcher(longEnum) :? StringParamMatcher(stringEnum) =>
        for {
          body <- req.as[Json]
          _    <- cond(intEnum == expectedInt.value.toString(), "intEnum value did not match")
          _    <- cond(longEnum.exists(_ == expectedLong.value), "longEnum value did not match")
          _    <- cond(stringEnum.exists(_ == expectedString.value), "stringEnum value did not match")
          _    <- cond(body == Json.fromInt(expectedBody.value), "body value did not match")
          resp <- Created(Json.fromString(expectedResponse.value))
        } yield resp
    }

    val fooClient = FooClient.httpClient(Client.fromHttpApp(httpService.orNotFound), "http://localhost:1234")

    fooClient.doFoo(expectedInt, Some(expectedLong), Some(expectedString), expectedBody).attempt.unsafeRunSync() should be(
      Right(ClientDoFooResponse.Created(expectedResponse))
    )
  }
}
