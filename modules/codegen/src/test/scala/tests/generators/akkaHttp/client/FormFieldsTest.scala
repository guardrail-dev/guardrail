package tests.generators.akkaHttp.client

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.{ Client, Clients, Context }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import com.twilio.guardrail.tests._
import scala.meta._

class FormFieldsTest extends FunSuite with Matchers with SwaggerSpecRunner {
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
    |    put:
    |      operationId: putFoo
    |      consumes:
    |        - multipart/form-data
    |      parameters:
    |        - name: foo
    |          in: formData
    |          type: string
    |          required: true
    |        - name: bar
    |          in: formData
    |          type: integer
    |          format: int64
    |          required: true
    |        - name: baz
    |          in: formData
    |          type: file
    |          required: true
    |      responses:
    |        200:
    |          description: Success
    |""".stripMargin

  test("Properly handle all methods") {
    val (
      _,
      Clients(Client(tags, className, statements) :: _),
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)

    val Seq(cmp, cls) = statements.dropWhile(_.isInstanceOf[Import])

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
        def putFoo(foo: String, bar: Long, baz: BodyPartEntity, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          wrap[IgnoredEntity](Marshal(Multipart.FormData(Source.fromIterator {
            () => List(Some(Multipart.FormData.BodyPart("foo", Formatter.show(foo))), Some(Multipart.FormData.BodyPart("bar", Formatter.show(bar))), Some(Multipart.FormData.BodyPart("baz", baz))).flatten.iterator
          })).to[RequestEntity].flatMap {
            entity => httpClient(HttpRequest(method = HttpMethods.PUT, uri = host + basePath + "/foo", entity = entity, headers = allHeaders))
          })
        }
      }
    """

    cls.structure should equal(client.structure)
  }
}
