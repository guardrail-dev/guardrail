package core.issues

import dev.guardrail.{ Context, Server, Servers }
import dev.guardrail.generators.Scala.Http4s
import support.SwaggerSpecRunner
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Issue416 extends AnyFunSuite with Matchers with SwaggerSpecRunner {

  import scala.meta._

  val swagger: String =
    s"""
       |swagger: '2.0'
       |host: petstore.swagger.io
       |paths:
       |  /:
       |    get:
       |      parameters:
       |      - in: body
       |        name: response
       |        schema:
       |           $$ref: "#/definitions/SinkConfiguration"
       |      operationId: GetRoot
       |      responses:
       |        200:
       |         description: description
       |
       |definitions:
       |  SinkConfiguration:
       |    type: object
       |    required:
       |      - name
       |    properties:
       |      name:
       |        type: string
       |""".stripMargin

  test("Ensure mapRoute is generated") {
    val (_, _, Servers(Server(_, _, genHandler, genResource :: _) :: Nil, _)) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    val resource = q"""
      class Resource[F[_]](mapRoute: (String, Request[F], F[Response[F]]) => F[Response[F]] = (_: String, _: Request[F], r: F[Response[F]]) => r)(implicit F: Async[F]) extends Http4sDsl[F] with CirceInstances {
        private[this] val getRootDecoder: EntityDecoder[F, Option[SinkConfiguration]] = jsonOf[F, Option[SinkConfiguration]]
        def routes(handler: Handler[F]): HttpRoutes[F] = HttpRoutes.of {
          {
            case req @ GET -> Root =>
              mapRoute("getRoot", req, {
                req.decodeWith(getRootDecoder, strict = false) { body =>
                  handler.getRoot(GetRootResponse)(body) flatMap ({
                    case GetRootResponse.Ok =>
                      F.pure(Response[F](status = org.http4s.Status.Ok))
                  })
                }
              })
          }
        }
      }
    """
    // Cause structure is slightly different but source code is the same the value converted to string and then parsed
    compare(genResource.toString().parse[Stat].get, resource)
  }

  private def compare(actual: Tree, expected: Tree): Unit =
    actual.structure shouldEqual expected.structure
}
