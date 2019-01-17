package tests.generators.http4s

import com.twilio.guardrail.generators.Http4s
import com.twilio.guardrail.{ Context, Server, Servers }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner

class Http4sServerTest extends FunSuite with Matchers with SwaggerSpecRunner {

  import scala.meta._

  val swagger: String =
    s"""
       |swagger: '2.0'
       |host: petstore.swagger.io
       |paths:
       |  "/":
       |    get:
       |      x-scala-package: store
       |      operationId: getRoot
       |      responses:
       |        200:
       |         description: description
       |  "/foo":
       |    get:
       |      x-scala-package: store
       |      operationId: getFoo
       |      responses:
       |        200:
       |         description: description
       |  "/foo/":
       |    get:
       |      x-scala-package: store
       |      operationId: getFooDir
       |      responses:
       |        200:
       |         description: description
       |""".stripMargin

  test("Ensure routes are generated") {
    val (_, _, Servers(Server(_, _, genHandler :: genResource :: _) :: Nil)) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    val handler  = q"""
      trait StoreHandler[F[_]] {
        def getRoot(respond: GetRootResponse.type)(): F[GetRootResponse]
        def getFoo(respond: GetFooResponse.type)(): F[GetFooResponse]
        def getFooDir(respond: GetFooDirResponse.type)(): F[GetFooDirResponse]
      }
    """
    val resource = q"""
      class StoreResource[F[_]]()(implicit E: Effect[F]) extends Http4sDsl[F] {
        def routes(handler: StoreHandler[F]): HttpRoutes[F] = HttpRoutes.of {
          {
            case req @ GET -> Root =>
              handler.getRoot(GetRootResponse)() flatMap {
                case GetRootResponse.Ok =>
                  Ok()
              }
            case req @ GET -> Root / "foo" =>
              handler.getFoo(GetFooResponse)() flatMap {
                case GetFooResponse.Ok =>
                  Ok()
              }
            case req @ GET -> Root / "foo" / "" =>
              handler.getFooDir(GetFooDirResponse)() flatMap {
                case GetFooDirResponse.Ok =>
                  Ok()
              }
          }
        }
      }
    """

    genHandler.structure shouldEqual handler.structure

    // Cause structure is slightly different but source code is the same the value converted to string and then parsed
    genResource.toString().parse[Stat].get.structure shouldEqual resource.structure
  }
}
