package core.issues

import support.SwaggerSpecRunner

import dev.guardrail.Context
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.http4s.Http4sVersion
import dev.guardrail.generators.{ Server, Servers }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Issue225 extends AnyFunSuite with Matchers with SwaggerSpecRunner {

  import scala.meta._

  val spec: String =
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

  def testVersion(version: Http4sVersion): Unit =
    test(s"$version - Ensure mapRoute is generated") {
      val (_, _, Servers(Server(_, _, genHandler, genResource :: _) :: Nil, Nil)) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, version.value)

      val handler = q"""
      trait Handler[F[_]] {
        def getRoot(respond: Resource.GetRootResponse.type)(response: String): F[Resource.GetRootResponse]
      }
    """
      val resource = q"""
      class Resource[F[_]](mapRoute: (String, Request[F], F[Response[F]]) => F[Response[F]] = (_: String, _: Request[F], r: F[Response[F]]) => r)(implicit F: Async[F]) extends Http4sDsl[F] with CirceInstances {
        import Resource._
        def routes(handler: Handler[F]): HttpRoutes[F] = HttpRoutes.of {
          {
            case req @ GET -> Root / response =>
              mapRoute("getRoot", req, {
                handler.getRoot(GetRootResponse)(response) flatMap ({
                  case GetRootResponse.Ok =>
                    F.pure(Response[F](status = org.http4s.Status.Ok))
                })
              })
          }
        }
      }
    """

      genHandler.structure shouldEqual handler.structure

      // Cause structure is slightly different but source code is the same the value converted to string and then parsed
      genResource.toString().parse[Stat].get.structure shouldEqual resource.structure
    }

  testVersion(Http4sVersion.V0_22)
  testVersion(Http4sVersion.V0_23)
}
