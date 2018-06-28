package tests.generators.akkaHttp.client

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.{ Client, Clients, Context }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import com.twilio.guardrail.tests._
import scala.meta._

class HardcodedQSSpec extends FunSuite with Matchers with SwaggerSpecRunner {
  val swagger: String = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |paths:
    |  /hardcodedQs?foo=bar:
    |    get:
    |      operationId: getHardcodedQs
    |      parameters:
    |      - name: bar
    |        type: integer
    |        format: int32
    |        in: query
    |      responses:
    |        200:
    |          description: Success
    |  /specViolation?isThisSupported={value}:
    |    get:
    |      operationId: getHardcodedQs
    |      parameters:
    |      - name: value
    |        type: integer
    |        format: int32
    |        in: path
    |      - name: bar
    |        type: integer
    |        format: int32
    |        in: query
    |      responses:
    |        200:
    |          description: Success
    |""".stripMargin

  test("Test all cases") {
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
      def getHardcodedQs(bar: Option[Int] = None, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
        val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
        wrap[IgnoredEntity](Marshal(HttpEntity.Empty).to[RequestEntity].flatMap { entity =>
          httpClient(HttpRequest(method = HttpMethods.GET, uri = host + basePath + "/hardcodedQs?foo=bar" + "&" + Formatter.addArg("bar", bar), entity = entity, headers = allHeaders))
        })
      }
      def getHardcodedQs(value: Int, bar: Option[Int] = None, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
        val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
        wrap[IgnoredEntity](Marshal(HttpEntity.Empty).to[RequestEntity].flatMap { entity =>
          httpClient(HttpRequest(method = HttpMethods.GET, uri = host + basePath + "/specViolation?isThisSupported=" + Formatter.addPath(value) + "&" + Formatter.addArg("bar", bar), entity = entity, headers = allHeaders))
        })
      }
    }
    """

    cls.structure should equal(client.structure)
  }
}
