package tests.core.issues

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.{ Context, Server, Servers }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner

class Issue126 extends FunSuite with Matchers with SwaggerSpecRunner {
  import scala.meta._

  val swagger: String = s"""
    |swagger: '2.0'
    |host: petstore.swagger.io
    |paths:
    |  /:
    |    options:
    |      x-scala-package: store
    |      operationId: getRoot
    |      responses:
    |        200:
    |          description: Successful
    |""".stripMargin

  test("Ensure routes are generated for OPTIONS method") {
    val (
      _,
      _,
      Servers(Server(pkg, extraImports, genHandler, genResource :: Nil) :: Nil, Nil)
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)

    val handler  = q"""
      trait StoreHandler {
        def getRoot(respond: StoreResource.getRootResponse.type)(): scala.concurrent.Future[StoreResource.getRootResponse]
      }
    """
    val resource = q"""
      object StoreResource {
        def discardEntity: Directive0 = extractMaterializer.flatMap { implicit mat =>
          extractRequest.flatMap { req =>
            req.discardEntityBytes().future
            Directive.Empty
          }
        }
        def routes(handler: StoreHandler)(implicit mat: akka.stream.Materializer): Route = {
          {
            options(pathEndOrSingleSlash(discardEntity(complete(handler.getRoot(getRootResponse)()))))
          }
        }
        sealed abstract class getRootResponse(val statusCode: StatusCode)
        case object getRootResponseOK extends getRootResponse(StatusCodes.OK)
        object getRootResponse {
          implicit val getRootTRM: ToResponseMarshaller[getRootResponse] = Marshaller { implicit ec =>
            resp => getRootTR(resp)
          }
          implicit def getRootTR(value: getRootResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r: getRootResponseOK.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => getRootResponse): getRootResponse = ev(value)
          def OK: getRootResponse = getRootResponseOK
        }
      }
    """

    genHandler.structure shouldEqual handler.structure
    genResource.structure shouldEqual resource.structure
  }
}
