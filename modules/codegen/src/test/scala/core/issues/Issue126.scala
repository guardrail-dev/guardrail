package tests.core.issues

import dev.guardrail.generators.Scala.AkkaHttp
import dev.guardrail.{ Context, Server, Servers }
import support.SwaggerSpecRunner
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Issue126 extends AnyFunSuite with Matchers with SwaggerSpecRunner {
  import scala.meta._

  val swagger: String = s"""
    |swagger: '2.0'
    |host: petstore.swagger.io
    |paths:
    |  /:
    |    options:
    |      x-jvm-package: store
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
        def getRoot(respond: StoreResource.GetRootResponse.type)(): scala.concurrent.Future[StoreResource.GetRootResponse]
      }
    """
    val resource = q"""
      object StoreResource {
        def routes(handler: StoreHandler)(implicit mat: akka.stream.Materializer): Route = {
          {
            pathEndOrSingleSlash(options(discardEntity(complete(handler.getRoot(GetRootResponse)()))))
          }
        }
        sealed abstract class GetRootResponse(val statusCode: StatusCode)
        case object GetRootResponseOK extends GetRootResponse(StatusCodes.OK)
        object GetRootResponse {
          implicit def getRootResponseTRM: ToResponseMarshaller[GetRootResponse] = Marshaller { implicit ec =>
            resp => getRootResponseTR(resp)
          }
          implicit def getRootResponseTR(value: GetRootResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r: GetRootResponseOK.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => GetRootResponse): GetRootResponse = ev(value)
          def OK: GetRootResponse = GetRootResponseOK
        }
      }
    """

    genHandler.structure shouldEqual handler.structure
    genResource.structure shouldEqual resource.structure
  }
}
