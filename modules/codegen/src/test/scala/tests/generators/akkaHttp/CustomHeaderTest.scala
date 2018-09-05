package tests.generators.akkaHttp

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.{ Clients, Context, Server, Servers }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import com.twilio.guardrail.tests._

import scala.meta._

class CustomHeaderTest extends FunSuite with Matchers with SwaggerSpecRunner {
  val swagger: String = s"""
                           |swagger: "2.0"
                           |info:
                           |  title: Whatever
                           |  version: 1.0.0
                           |host: localhost:1234
                           |schemes:
                           |  - http
                           |paths:
                           |  /foo:
                           |    get:
                           |      operationId: getFoo
                           |      parameters:
                           |      - name: CustomHeader
                           |        in: header
                           |        type: string
                           |        x-scala-type: Bar
                           |        required: true
                           |      responses:
                           |        200:
                           |          description: Success
                           |definitions:
                           |  Bar:
                           |    type: string
                           |    enum:
                           |      - v1
                           |      - v2
                           |      - i like spaces
                           |""".stripMargin

  test("Should produce static parameter constraints") {
    val (_, Clients(client :: Nil), Servers(Server(_, _, genHandler :: genResource :: Nil) :: Nil)) =
      runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)

    val handler =
      q"""trait Handler { def getFoo(respond: Resource.getFooResponse.type)(customHeader: Bar): scala.concurrent.Future[Resource.getFooResponse] }"""

    val resource = q"""
      object Resource {
        def discardEntity(implicit mat: akka.stream.Materializer): Directive0 = extractRequest.flatMap { req =>
          req.discardEntityBytes().future
          Directive.Empty
        }
        def routes(handler: Handler)(implicit mat: akka.stream.Materializer): Route = {
          (get & path("foo") & discardEntity & headerValueByName("CustomHeader").flatMap(str => onSuccess(Unmarshal(str).to[Bar]))) {
            customHeader => complete(handler.getFoo(getFooResponse)(customHeader))
          }
        }
        sealed abstract class getFooResponse(val statusCode: StatusCode)
        case object getFooResponseOK extends getFooResponse(StatusCodes.OK)
        object getFooResponse {
          implicit val getFooTRM: ToResponseMarshaller[getFooResponse] = Marshaller { implicit ec =>
            resp => getFooTR(resp)
          }
          implicit def getFooTR(value: getFooResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r: getFooResponseOK.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => getFooResponse): getFooResponse = ev(value)
          def OK: getFooResponse = getFooResponseOK
        }
      }
    """

    genHandler.structure should equal(handler.structure)
    genResource.structure should equal(resource.structure)
  }
}
