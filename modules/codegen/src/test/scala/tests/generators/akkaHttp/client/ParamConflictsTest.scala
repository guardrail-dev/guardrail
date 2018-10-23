package tests.generators.akkaHttp.client

import com.twilio.guardrail._
import com.twilio.guardrail.generators.AkkaHttp
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import com.twilio.guardrail.tests._
import scala.meta._

class ParamConflictsTest extends FunSuite with Matchers with SwaggerSpecRunner {

  val swagger = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |paths:
    |  /foo:
    |    get:
    |      operationId: getFoo
    |      parameters:
    |        - in: formData
    |          name: conflicting_name
    |          type: string
    |          required: true
    |        - in: formData
    |          name: ConflictingName
    |          type: string
    |          required: true
    |      responses:
    |        200:
    |          description: Success
    |definitions:
    |  Foo:
    |    type: object
    |    properties:
    |      conflicting_name:
    |        type: string
    |      ConflictingName:
    |        type: string
    |""".stripMargin

  test("Generate non-conflicting names in clients") {
    val (
      _,
      Clients(Client(tags, className, statements) :: _),
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)

    val List(cmp, cls) = statements.dropWhile(_.isInstanceOf[Import])

    val client = q"""
      class Client(host: String = "http://localhost:1234")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer) {
        val basePath: String = ""
        private[this] def makeRequest[T: ToEntityMarshaller](method: HttpMethod, uri: Uri, headers: scala.collection.immutable.Seq[HttpHeader], entity: T, protocol: HttpProtocol): EitherT[Future, Either[Throwable, HttpResponse], HttpRequest] = {
          EitherT(Marshal(entity).to[RequestEntity].map[Either[Either[Throwable, HttpResponse], HttpRequest]] {
            entity => Right(HttpRequest(method = method, uri = uri, headers = headers, entity = entity, protocol = protocol))
          }.recover({
            case t =>
              Left(Left(t))
          }))
        }
        private[this] def wrap[T: FromEntityUnmarshaller](client: HttpClient, request: HttpRequest): EitherT[Future, Either[Throwable, HttpResponse], T] = {
          EitherT(client(request).flatMap(resp => if (resp.status.isSuccess) {
            Unmarshal(resp.entity).to[T].map(Right.apply _)
          } else {
            FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          }))
        }
        def getFoo(conflicting_name: String, ConflictingName: String, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/foo", allHeaders, FormData(List(("conflicting_name", Some(Formatter.show(conflicting_name))), ("ConflictingName", Some(Formatter.show(ConflictingName)))).collect({
            case (n, Some(v)) =>
              (n, v)
          }): _*), HttpProtocols.`HTTP/1.1`).flatMap(req => wrap[IgnoredEntity](httpClient, req))
        }
      }
    """

    cls.structure should equal(client.structure)
  }

  test("Generate non-conflicting names in definitions") {
    val (
      ProtocolDefinitions(ClassDefinition(_, _, cls, cmp, _) :: _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)

    val definition = q"""
      case class Foo(conflicting_name: Option[String] = None, ConflictingName: Option[String] = None)
    """
    val companion  = q"""
      object Foo {
        implicit val encodeFoo = {
          val readOnlyKeys = Set[String]()
          Encoder.forProduct2("conflicting_name", "ConflictingName")((o: Foo) => (o.conflicting_name, o.ConflictingName)).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeFoo = Decoder.forProduct2("conflicting_name", "ConflictingName")(Foo.apply _)
      }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
  }
}
