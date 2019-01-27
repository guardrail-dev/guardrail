package tests.generators.akkaHttp.client

import com.twilio.guardrail._
import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.generators.syntax.Scala.companionForStaticDefns
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
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
    |      consumes:
    |        - application/x-www-form-urlencoded
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
      Clients(Client(tags, className, _, _, cls, _) :: _),
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)

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
        def getFoo(conflicting_name: String, ConflictingName: String, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], GetFooResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/foo", allHeaders, FormData(List(List(("conflicting_name", Formatter.show(conflicting_name))), List(("ConflictingName", Formatter.show(ConflictingName)))).flatten: _*), HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              resp.discardEntityBytes().future.map(_ => Right(GetFooResponse.OK))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
      }
    """

    cls.structure should equal(client.structure)
  }

  test("Generate non-conflicting names in definitions") {
    val (
      ProtocolDefinitions(ClassDefinition(_, _, cls, staticDefns, _) :: _, _, _, _),
      _,
      _
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

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
