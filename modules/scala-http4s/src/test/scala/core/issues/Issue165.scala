package tests.core.issues

import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.http4s.Http4sVersion
import dev.guardrail.Context
import dev.guardrail.generators.{ Server, Servers }
import support.SwaggerSpecRunner
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Issue165 extends AnyFunSuite with Matchers with SwaggerSpecRunner {

  import scala.meta._

  val spec: String =
    s"""
       |swagger: '2.0'
       |host: petstore.swagger.io
       |paths:
       |  "/":
       |    get:
       |      x-jvm-package: store
       |      operationId: getRoot
       |      responses:
       |        200:
       |         description: description
       |  "/foo":
       |    get:
       |      x-jvm-package: store
       |      operationId: getFoo
       |      responses:
       |        200:
       |         description: description
       |  "/foo/":
       |    get:
       |      x-jvm-package: store
       |      operationId: getFooDir
       |      responses:
       |        200:
       |         description: description
       |""".stripMargin

  def testVersion(version: Http4sVersion): Unit =
    test(s"$version - Ensure routes are generated") {
      val (_, _, Servers(Server(_, _, genHandler, genResource :: _) :: Nil, Nil)) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, version.value)

      val handler = q"""
      trait StoreHandler[F[_]] {

        def getRoot(respond: StoreResource.GetRootResponse.type)(): F[StoreResource.GetRootResponse]
        def getFoo(respond: StoreResource.GetFooResponse.type)(): F[StoreResource.GetFooResponse]
        def getFooDir(respond: StoreResource.GetFooDirResponse.type)(): F[StoreResource.GetFooDirResponse]
      }
    """
      val resource = q"""
      class StoreResource[F[_]](mapRoute: (String, Request[F], F[Response[F]]) => F[Response[F]] = (_: String, _: Request[F], r: F[Response[F]]) => r)(implicit F: Async[F]) extends Http4sDsl[F] with CirceInstances {
        import StoreResource._
        def routes(handler: StoreHandler[F]): HttpRoutes[F] = HttpRoutes.of {
          {
            case req @ GET -> Root =>
              mapRoute("getRoot", req, {
                handler.getRoot(GetRootResponse)() flatMap ({
                  case GetRootResponse.Ok =>
                    F.pure(Response[F](status = org.http4s.Status.Ok))
                })
              })
            case req @ GET -> Root / "foo" =>
              mapRoute("getFoo", req, {
                handler.getFoo(GetFooResponse)() flatMap ({
                  case GetFooResponse.Ok =>
                    F.pure(Response[F](status = org.http4s.Status.Ok))
                })
              })
            case req @ GET -> Root / "foo" / "" =>
              mapRoute("getFooDir", req, {
                handler.getFooDir(GetFooDirResponse)() flatMap ({
                  case GetFooDirResponse.Ok =>
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
