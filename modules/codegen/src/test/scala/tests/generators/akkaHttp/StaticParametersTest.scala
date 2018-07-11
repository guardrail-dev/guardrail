package tests.generators.akkaHttp

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.{ Context, Server, Servers }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import com.twilio.guardrail.tests._
import scala.meta._

class StaticParametersTest extends FunSuite with Matchers with SwaggerSpecRunner {
  val swagger: String = s"""
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
    val (_, _, Servers(Server(_, _, genHandler :: genResource :: Nil) :: Nil)) =
      runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)

    val handler = q"""
      trait Handler {
        def getFoo1(respond: Resource.getFoo1Response.type)(): scala.concurrent.Future[Resource.getFoo1Response]
        def getFoo2(respond: Resource.getFoo2Response.type)(): scala.concurrent.Future[Resource.getFoo2Response]
      }
    """

    val resource = q"""
      object Resource {
        def discardEntity(implicit mat: akka.stream.Materializer): Directive0 = extractRequest.flatMap { req =>
          req.discardEntityBytes().future
          Directive.Empty
        }
        def routes(handler: Handler)(implicit mat: akka.stream.Materializer): Route = {
          (get & (path("foo") & parameter("bar").require(_ == "1")) & discardEntity) {
            complete(handler.getFoo1(getFoo1Response)())
          } ~ (get & (pathPrefix("foo") & pathEndOrSingleSlash & parameter("bar").require(_ == "2")) & discardEntity) {
            complete(handler.getFoo2(getFoo2Response)())
          }
        }
        sealed abstract class getFoo1Response(val statusCode: StatusCode)
        case object getFoo1ResponseOK extends getFoo1Response(StatusCodes.OK)
        object getFoo1Response {
          implicit val getFoo1TRM: ToResponseMarshaller[getFoo1Response] = Marshaller { implicit ec =>
            resp => getFoo1TR(resp)
          }
          implicit def getFoo1TR(value: getFoo1Response)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r: getFoo1ResponseOK.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => getFoo1Response): getFoo1Response = ev(value)
          def OK: getFoo1Response = getFoo1ResponseOK
        }
        sealed abstract class getFoo2Response(val statusCode: StatusCode)
        case object getFoo2ResponseOK extends getFoo2Response(StatusCodes.OK)
        object getFoo2Response {
          implicit val getFoo2TRM: ToResponseMarshaller[getFoo2Response] = Marshaller { implicit ec =>
            resp => getFoo2TR(resp)
          }
          implicit def getFoo2TR(value: getFoo2Response)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r: getFoo2ResponseOK.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => getFoo2Response): getFoo2Response = ev(value)
          def OK: getFoo2Response = getFoo2ResponseOK
        }
      }
    """

    genHandler.structure should equal(handler.structure)
    genResource.structure should equal(resource.structure)
  }
}
