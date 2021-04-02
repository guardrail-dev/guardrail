package core.issues

import dev.guardrail.generators.Scala.Http4s
import dev.guardrail.{ Context, Server, Servers }
import support.SwaggerSpecRunner
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Issue225 extends AnyFunSuite with Matchers with SwaggerSpecRunner {

  import scala.meta._

  val swagger: String =
    s"""
       |swagger: '2.0'
       |host: petstore.swagger.io
       |paths:
       |  /{response}:
       |    parameters:
       |    - in: path
       |      name: response
       |      type: string
       |    get:
       |      operationId: getRoot
       |      responses:
       |        200:
       |         description: description
       |""".stripMargin

  test("Ensure mapRoute is generated") {
    val (_, _, Servers(Server(_, _, genHandler, genResource :: _) :: Nil, Nil)) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    val handler  = q"""
      trait Handler[F[_]] {
        def getRoot(respond: GetRootResponse.type)(response: String): F[GetRootResponse]
      }
    """
    val resource = q"""
      class Resource[F[_]](mapRoute: (String, Request[F], F[Response[F]]) => F[Response[F]] = (_: String, _: Request[F], r: F[Response[F]]) => r)(implicit F: Async[F]) extends Http4sDsl[F] with CirceInstances {
        def routes(handler: Handler[F]): HttpRoutes[F] = HttpRoutes.of {
          {
            case req @ GET -> Root / response => 
              mapRoute("getRoot", req, {
                handler.getRoot(GetRootResponse)(response) flatMap {
                  case GetRootResponse.Ok =>
                    F.pure(Response[F](status = org.http4s.Status.Ok))
                } 
              })
          }
        }
      }
    """

    compare(genHandler, handler)

    // Cause structure is slightly different but source code is the same the value converted to string and then parsed
    compare(genResource.toString().parse[Stat].get, resource)
  }

  private def compare(actual: Tree, expected: Tree): Unit =
    actual.structure shouldEqual expected.structure
}
