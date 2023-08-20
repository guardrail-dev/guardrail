package core.issues

import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.http4s.Http4sVersion
import dev.guardrail.Context
import dev.guardrail.generators.{ Server, Servers }
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

  def testVersion(version: Http4sVersion): Unit =
    test(s"$version - Ensure mapRoute is generated") {
      val (_, _, Servers(Server(_, _, _, genResource :: _) :: Nil, _)) = runSwaggerSpec(scalaInterpreter)(swagger)(Context.empty, version.value)

      val resource =
        q"""
      class Resource[F[_]](mapRoute: (String, Request[F], F[Response[F]]) => F[Response[F]] = (_: String, _: Request[F], r: F[Response[F]]) => r)(implicit F: Async[F]) extends Http4sDsl[F] with CirceInstances {
        import Resource._
        protected[this] val getEpochSecondsDecoder: EntityDecoder[F, Long] = jsonOf[F, Long]
        def routes(handler: Handler[F]): HttpRoutes[F] = HttpRoutes.of {
          {
            case req @ GET -> Root =>
              mapRoute("getEpochSeconds", req, {
                req.attemptAs(getEpochSecondsDecoder).foldF(err => err.cause match {
                  case Some(circeErr: io.circe.DecodingFailure) =>
                    Response[F](status = org.http4s.Status.UnprocessableEntity, body = stringEncoder.toEntity("The request body was invalid. " + circeErr.message + ": " + circeErr.history.mkString(", ")).body).pure[F]
                  case _ =>
                    err.toHttpResponse[F](req.httpVersion).pure[F]
                }, body => handler.getEpochSeconds(GetEpochSecondsResponse)(body) flatMap ({
                  case GetEpochSecondsResponse.Ok =>
                    F.pure(Response[F](status = org.http4s.Status.Ok))
                }))
              })
          }
        }
      }
    """

      // Cause structure is slightly different but source code is the same the value converted to string and then parsed
      genResource.toString().parse[Stat].get.structure shouldEqual resource.structure
    }

  testVersion(Http4sVersion.V0_22)
  testVersion(Http4sVersion.V0_23)
}
