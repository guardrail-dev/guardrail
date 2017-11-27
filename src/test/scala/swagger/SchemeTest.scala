package swagger

import _root_.io.swagger.parser.SwaggerParser
import cats.instances.all._
import com.twilio.swagger.codegen.generators.AkkaHttp
import com.twilio.swagger.codegen.{Client, Clients, Context, ClientGenerator, CodegenApplication, Target}
import org.scalatest.{FunSuite, Matchers}
import scala.meta._

class SchemeTest extends FunSuite with Matchers {

  val swagger = new SwaggerParser().parse(s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - https
    |paths:
    |  /foo:
    |    get:
    |      operationId: getFoo
    |      responses:
    |        200:
    |          schema:
    |            $$ref: "#/definitions/Bar"
    |definitions:
    |  Bar:
    |    type: object
    |    properties:
    |      id:
    |        type: integer
    |        format: int64
    |""".stripMargin)

  test("Use first scheme") {
    val companion = q"""
    object Client {
      def apply(host: String = "https://localhost:1234")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer): Client = new Client(host = host)(httpClient = httpClient, ec = ec, mat = mat)
      def httpClient(httpClient: HttpRequest => Future[HttpResponse], host: String = "https://localhost:1234")(implicit ec: ExecutionContext, mat: Materializer): Client = new Client(host = host)(httpClient = httpClient, ec = ec, mat = mat)
    }
    """
    val client = q"""
      class Client(host: String = "https://localhost:1234")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer) {
        val basePath: String = ""
        private[this] def wrap[T: FromEntityUnmarshaller](resp: Future[HttpResponse]): EitherT[Future, Either[Throwable, HttpResponse], T] = {
          EitherT(resp.flatMap(resp => if (resp.status.isSuccess) {
            Unmarshal(resp.entity).to[T].map(Right.apply _)
          } else {
            FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          }))
        }
        def getFoo(headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], Bar] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          wrap[Bar](Marshal(HttpEntity.Empty).to[RequestEntity].flatMap { entity =>
            httpClient(HttpRequest(method = HttpMethods.GET, uri = host + basePath + "/foo", entity = entity, headers = allHeaders))
          })
        }
      }
    """

    val Clients(clients, _) = Target.unsafeExtract(ClientGenerator.fromSwagger[CodegenApplication](Context.empty, swagger)(List.empty).foldMap(AkkaHttp))
    val Client(tags, className, statements) :: _ = clients
    val List(cmp, cls) = statements.dropWhile(_.isInstanceOf[Import])

    companion.structure should equal(cmp.structure)
    client.structure should equal(cls.structure)
  }
}
