package core.issues

import cats.implicits._
import org.http4s.implicits._
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.SpanSugar._
import scala.concurrent.Future
import cats.effect.Async
import cats.effect.implicits._
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.http4s.client.{ Client => Http4sClient }

class Issue455Suite extends AnyFunSuite with Matchers with EitherValues with ScalaFutures {
  override implicit val patienceConfig: PatienceConfig = PatienceConfig(10 seconds, 1 second)

  test("Circe NPE: https://github.com/circe/circe/issues/561") {
    val route = {
      import issues.issue455.server.http4s.{ Handler, Resource }
      import issues.issue455.server.http4s.Resource.BooResponse
      import issues.issue455.server.http4s.definitions.RecursiveData
      new Resource[IO].routes(new Handler[IO] {
        val recData = RecursiveData(3, "three", Some(RecursiveData(2, "two", Some(RecursiveData(1, "one", None)))))
        def boo(respond: BooResponse.type)(body: RecursiveData): IO[BooResponse] = IO.pure(respond.Ok(recData))
      })
    }
    {
      import issues.issue455.client.http4s.Client
      import issues.issue455.client.http4s.definitions.RecursiveData
      val recData = RecursiveData(3, "three", Some(RecursiveData(2, "two", Some(RecursiveData(1, "one", None)))))
      val client  = Client.httpClient(Http4sClient.fromHttpApp[IO](route.orNotFound))
      val resp    = client.boo(recData).unsafeToFuture().futureValue
      resp.fold(handleOk = {
        case `recData` => ()
        case data      => fail(s"${data} != ${recData}")
      })
    }
  }
}
