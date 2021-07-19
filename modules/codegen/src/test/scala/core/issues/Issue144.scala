package core.issues

import dev.guardrail.generators.Scala.Http4s
import dev.guardrail.{ Context, Server, Servers }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import support.SwaggerSpecRunner

class Issue144 extends AnyFunSuite with Matchers with SwaggerSpecRunner {

  import scala.meta._

  val swagger: String =
    s"""
       |swagger: '2.0'
       |host: petstore.swagger.io
       |paths:
       |  /:
       |    get:
       |      parameters:
       |        - name: epoch_seconds
       |          in: body
       |          required: true
       |          schema:
       |            type: integer
       |            format: int64
       |      operationId: GetEpochSeconds
       |      responses:
       |        200:
       |         description: description
       |""".stripMargin

  test("Ensure mapRoute is generated") {
    val (_, _, Servers(Server(_, _, _, genResource :: _) :: Nil, _)) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    val resource =
      q"""
      class Resource[F[_]](mapRoute: (String, Request[F], F[Response[F]]) => F[Response[F]] = (_: String, _: Request[F], r: F[Response[F]]) => r)(implicit F: Async[F]) extends Http4sDsl[F] with CirceInstances {
        private[this] val getEpochSecondsDecoder: EntityDecoder[F, Long] = jsonOf[F, Long]
        def routes(handler: Handler[F]): HttpRoutes[F] = HttpRoutes.of {
          {
            case req @ GET -> Root =>
              mapRoute("getEpochSeconds", req, {
                req.decodeWith(getEpochSecondsDecoder, strict = false) { body =>
                  handler.getEpochSeconds(GetEpochSecondsResponse)(body) flatMap ({
                    case GetEpochSecondsResponse.Ok =>
                      F.pure(Response[F](status = org.http4s.Status.Ok))
                  })
                }
              })
          }
        }
      }
    """

    // Cause structure is slightly different but source code is the same the value converted to string and then parsed
    genResource.toString().parse[Stat].get.structure shouldEqual resource.structure
  }
}
