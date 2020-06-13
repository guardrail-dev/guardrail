package tests.generators.akkaHttp

import com.twilio.guardrail.generators.Scala.AkkaHttp
import com.twilio.guardrail.{ Clients, Context, Server, Servers }
import support.SwaggerSpecRunner

import scala.meta._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CustomHeaderTest extends AnyFunSuite with Matchers with SwaggerSpecRunner {
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
    val (_, Clients(client :: Nil, Nil), Servers(Server(_, _, genHandler, genResource :: Nil) :: Nil, Nil)) =
      runSwaggerSpec(swagger)(Context.empty, AkkaHttp)

    val handler =
      q"""trait Handler { def getFoo(respond: Resource.getFooResponse.type)(customHeader: Bar): scala.concurrent.Future[Resource.getFooResponse] }"""

    val resource = q"""
      object Resource {
        def routes(handler: Handler, afterPathMethodMatch: Directive0 = pass)(implicit mat: akka.stream.Materializer): Route = {
          {
            path("foo")(get(afterPathMethodMatch(headerValueByName("CustomHeader").flatMap(str => onComplete(Unmarshal(str).to[Bar](stringyJsonUnmarshaller.andThen(unmarshallJson[Bar]), mat.executionContext, mat)).flatMap[Tuple1[Bar]]({
              case Failure(e) =>
                reject(MalformedHeaderRejection("CustomHeader", e.getMessage, Some(e)))
              case Success(x) =>
                provide(x)
            })).apply(customHeader => discardEntity(complete(handler.getFoo(getFooResponse)(customHeader)))))))
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
