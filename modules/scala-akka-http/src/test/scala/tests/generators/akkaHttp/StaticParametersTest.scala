package tests.generators.akkaHttp

import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.Context
import dev.guardrail.generators.{ Server, Servers }
import support.{ ScalaMetaMatchers, SwaggerSpecRunner }
import scala.meta._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class StaticParametersTest extends AnyFunSuite with Matchers with SwaggerSpecRunner with ScalaMetaMatchers {
  val spec: String = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |paths:
    |  /foo?bar=1:
    |    get:
    |      operationId: getFoo1
    |      responses:
    |        200:
    |          description: Success
    |  /foo/?bar=2:
    |    get:
    |      operationId: getFoo2
    |      responses:
    |        200:
    |          description: Success
    |""".stripMargin

  test("Should produce static parameter constraints") {
    val (_, _, Servers(Server(_, _, genHandler, genResource :: Nil) :: Nil, Nil)) =
      runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, "akka-http")

    val handler = q"""
      trait Handler {
        def getFoo2(respond: Resource.GetFoo2Response.type)(): scala.concurrent.Future[Resource.GetFoo2Response]
        def getFoo1(respond: Resource.GetFoo1Response.type)(): scala.concurrent.Future[Resource.GetFoo1Response]
      }
    """

    val resource = q"""
      object Resource {
        def routes(handler: Handler)(implicit mat: akka.stream.Materializer): Route = {
          {
            (pathPrefix("foo") & pathEndOrSingleSlash & parameter("bar").require(_ == "2"))(get(discardEntity(complete(handler.getFoo2(GetFoo2Response)()))))
          } ~ {
            (path("foo") & parameter("bar").require(_ == "1"))(get(discardEntity(complete(handler.getFoo1(GetFoo1Response)()))))
          }
        }
        sealed abstract class GetFoo2Response(val statusCode: StatusCode)
        case object GetFoo2ResponseOK extends GetFoo2Response(StatusCodes.OK)
        object GetFoo2Response {
          implicit def getFoo2ResponseTRM: ToResponseMarshaller[GetFoo2Response] = Marshaller { implicit ec =>
            resp => getFoo2ResponseTR(resp)
          }
          implicit def getFoo2ResponseTR(value: GetFoo2Response)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r: GetFoo2ResponseOK.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => GetFoo2Response): GetFoo2Response = ev(value)
          def OK: GetFoo2Response = GetFoo2ResponseOK
        }
        sealed abstract class GetFoo1Response(val statusCode: StatusCode)
        case object GetFoo1ResponseOK extends GetFoo1Response(StatusCodes.OK)
        object GetFoo1Response {
          implicit def getFoo1ResponseTRM: ToResponseMarshaller[GetFoo1Response] = Marshaller { implicit ec =>
            resp => getFoo1ResponseTR(resp)
          }
          implicit def getFoo1ResponseTR(value: GetFoo1Response)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r: GetFoo1ResponseOK.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => GetFoo1Response): GetFoo1Response = ev(value)
          def OK: GetFoo1Response = GetFoo1ResponseOK
        }
      }
    """

    genHandler should matchStructure(handler)
    genResource should matchStructure(resource)
  }
}
