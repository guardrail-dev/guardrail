package generators.Http4s.RoundTrip

import cats.effect.IO
import org.http4s.client.{ Client => Http4sClient }
import org.http4s.implicits._
import org.scalatest.{ EitherValues, FlatSpec, Matchers }
import tests.customTypes.customHeader.client.http4s.{ definitions => cdefs, Client }
import tests.customTypes.customHeader.server.http4s.Implicits.Formatter
import tests.customTypes.customHeader.server.http4s.{ definitions => sdefs, GetFooResponse, Handler, Resource }

class Http4sCustomHeadersTest extends FlatSpec with Matchers with EitherValues {

  it should "encode custom headers" in {
    Formatter.show(sdefs.Bar.V1) shouldBe "v1"
    Formatter.show(sdefs.Bar.ILikeSpaces) shouldBe "i like spaces"
  }

  it should "round-trip encoded values" in {
    val client = Client.httpClient(
      Http4sClient.fromHttpApp(
        new Resource[IO]()
          .routes(new Handler[IO] {
            override def getFoo(
                respond: GetFooResponse.type
            )(header: String, longHeader: Long, customHeader: sdefs.Bar, customOptionHeader: Option[sdefs.Bar], missingCustomOptionHeader: Option[sdefs.Bar])
                : IO[GetFooResponse] =
              (header, longHeader, customHeader, customOptionHeader, missingCustomOptionHeader) match {
                case ("foo", 5L, sdefs.Bar.V1, Some(sdefs.Bar.V2), None) => IO.pure(respond.Ok)
                case _                                                   => IO.pure(respond.BadRequest)
              }
          })
          .orNotFound
      )
    )

    client.getFoo("foo", 5L, cdefs.Bar.V1, Some(cdefs.Bar.V2), None).attempt.unsafeRunSync().right.value
  }

}
