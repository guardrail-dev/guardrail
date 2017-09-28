package swagger

import _root_.io.swagger.parser.SwaggerParser
import cats.instances.all._
import com.twilio.swagger.codegen.generators.AkkaHttp
import com.twilio.swagger.codegen.{Client, Clients, Context, ClientGenerator, CodegenApplication}
import org.scalatest.{FunSuite, Matchers}
import scala.collection.immutable.{Seq => ISeq}
import scala.meta._

class MultipartTest extends FunSuite with Matchers {
  val swagger = new SwaggerParser().parse(s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |paths:
    |  /foo:
    |    post:
    |      operationId: createFoo
    |      consumes:
    |        - multipart/form-data
    |      produces:
    |        - application/json
    |      parameters:
    |        - name: bar
    |          in: formData
    |          type: string
    |          required: true
    |        - name: Quux
    |          in: formData
    |          type: file
    |          required: true
    |        - name: Baz
    |          in: formData
    |          type: string
    |        - name: oort_cloud
    |          in: formData
    |          type: file
    |      responses:
    |        200:
    |          description: Success
    |          schema:
    |            type: string
    |""".stripMargin)

  test("Multipart form data") {
    val Right(Clients(clients, _)) = ClientGenerator.fromSwagger[CodegenApplication](Context.empty, swagger)(List.empty).foldMap(AkkaHttp)
    val Client(_, className, statements) :: _ = clients

    val Seq(_, cls) = statements.dropWhile(_.isInstanceOf[Import])

    val client = q"""
    class Client(host: String = "http://localhost:1234")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer) {
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
      def createFoo(bar: String, quux: BodyPartEntity, baz: Option[String] = None, oortCloud: Option[BodyPartEntity] = None, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], String] = {
        val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
        wrap[String](httpClient(HttpRequest(method = HttpMethods.POST, uri = host + basePath + "/foo" + "?", entity = Multipart.FormData(Source.fromIterator {
          () => Seq(Some(Multipart.FormData.BodyPart("bar", Formatter.show(bar))), Some(Multipart.FormData.BodyPart("Quux", quux)), baz.map(v => Multipart.FormData.BodyPart("Baz", Formatter.show(v))), oortCloud.map(v => Multipart.FormData.BodyPart("oort_cloud", v))).flatten.iterator
        }).toEntity, headers = allHeaders)))
      }
    }
    """

    cls.structure should equal(client.structure)
  }
}
