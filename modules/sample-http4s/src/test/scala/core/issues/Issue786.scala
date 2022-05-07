package core.issues

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.http4s.{ Request, Status, Uri }
import org.http4s.client.{ Client => Http4sClient }
import org.http4s.blaze.client._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import io.circe._
import issues.issue786.server.http4s.definitions.Dog
import issues.issue786.server.http4s.definitions.Turtle

class Issue786Suite extends AnyFunSuite with Matchers {
  test("Ensure that the discriminator type is included in the wire-encoded value") {
    import issues.issue786.server.http4s.{ Handler, Resource }

    val route = new Resource[IO]().routes(new Handler[IO] {
      val dog    = Dog("bones", "black")
      val turtle = Turtle("turtle food")
      def getAnimals(respond: Resource.GetAnimalsResponse.type)(): IO[Resource.GetAnimalsResponse] =
        IO.pure(respond.Ok(Vector(dog, turtle)))
      def getDogs(respond: Resource.GetDogsResponse.type)(): IO[Resource.GetDogsResponse] =
        IO.pure(respond.Ok(Vector(dog)))
    })

    val client = Http4sClient.fromHttpApp[IO](route.orNotFound)

    val req = Request[IO](uri = Uri.unsafeFromString("/animals/dogs"))

    client
      .run(req)
      .use { case Status.Ok(resp) =>
        IO.pure {
          resp.status should equal(Status.Ok)
          resp.bodyText.compile.string.unsafeRunSync() should equal("""[{"food":"bones","colour":"black","animalType":"Dog"}]""")
          ()
        }
      }
      .unsafeRunSync()
  }
}
