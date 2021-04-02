package tests.generators.akkaHttp

import dev.guardrail.generators.Scala.AkkaHttp
import dev.guardrail.{ Clients, Context, Server, Servers }
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
      q"""trait Handler { def getFoo(respond: Resource.GetFooResponse.type)(customHeader: Bar): scala.concurrent.Future[Resource.GetFooResponse] }"""

    val resource = q"""
      object Resource {
        def routes(handler: Handler)(implicit mat: akka.stream.Materializer): Route = {
          {
            path("foo")(get(headerValueByName("CustomHeader").flatMap(str => onComplete(Unmarshal(str).to[Bar](stringyJsonUnmarshaller.andThen(unmarshallJson[Bar]), mat.executionContext, mat)).flatMap[Tuple1[Bar]]({
              case Failure(e) =>
                reject(MalformedHeaderRejection("CustomHeader", e.getMessage, Some(e)))
              case Success(x) =>
                provide(x)
            })).apply(customHeader => discardEntity(complete(handler.getFoo(GetFooResponse)(customHeader))))))
          }
        }
        sealed abstract class GetFooResponse(val statusCode: StatusCode)
        case object GetFooResponseOK extends GetFooResponse(StatusCodes.OK)
        object GetFooResponse {
          implicit def getFooResponseTRM: ToResponseMarshaller[GetFooResponse] = Marshaller { implicit ec =>
            resp => getFooResponseTR(resp)
          }
          implicit def getFooResponseTR(value: GetFooResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r: GetFooResponseOK.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => GetFooResponse): GetFooResponse = ev(value)
          def OK: GetFooResponse = GetFooResponseOK
        }
      }
    """

    genHandler.structure should equal(handler.structure)
    genResource.structure should equal(resource.structure)
  }
}
