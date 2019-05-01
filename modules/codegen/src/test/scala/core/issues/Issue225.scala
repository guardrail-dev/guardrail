package core.issues

import com.twilio.guardrail.generators.Http4s
import com.twilio.guardrail.{ Context, Server, Servers }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner

class Issue225 extends FunSuite with Matchers with SwaggerSpecRunner {

  import scala.meta._

  val swagger: String =
    s"""
       |swagger: '2.0'
       |host: petstore.swagger.io
       |paths:
       |  /{response0}:
       |    parameters:
       |    - in: path
       |      name: response0
       |      type: string
       |    get:
       |      operationId: getRoot
       |      responses:
       |        200:
       |         description: description
       |""".stripMargin

  test("Ensure handlerWrapper is generated and conflicting name is resolved correctly (response0 param conflicts with generated val name)") {
    val (_, _, Servers(Server(_, _, genHandler, genResource :: _) :: Nil, Nil)) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    val handler  = q"""
      trait Handler[F[_]] {
        def getRoot(respond: GetRootResponse.type)(response0: String): F[GetRootResponse]
      }
    """
    val resource = q"""
      class Resource[F[_]](mapRoute: (String, Request[F], F[Response[F]]) => F[Response[F]] = (_: String, _: Request[F], r: F[Response[F]]) => r)(implicit F: Async[F]) extends Http4sDsl[F] {
        def routes(handler: Handler[F]): HttpRoutes[F] = HttpRoutes.of {
          {
            case req @ GET -> Root / response0 => 
              val response1 = {
                handler.getRoot(GetRootResponse)(response0) flatMap {
                  case GetRootResponse.Ok =>
                    Ok()
                } 
              }
              mapRoute("getRoot", req, response1)
          }
        }
      }
    """

    compare(genHandler, handler)

    // Cause structure is slightly different but source code is the same the value converted to string and then parsed
    compare(genResource.toString().parse[Stat].get, resource)
  }
  
  private def compare(actual: Tree, expected: Tree): Unit = {
    println(s"actual: ${actual.syntax}")
    println(s"expected: ${expected.syntax}")
    actual.structure shouldEqual expected.structure
  }
}
