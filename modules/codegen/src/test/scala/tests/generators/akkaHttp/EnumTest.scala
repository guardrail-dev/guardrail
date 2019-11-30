package tests.generators.akkaHttp

import com.twilio.guardrail._
import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.generators.syntax.Scala.companionForStaticDefns
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import scala.meta._

class EnumTest extends FunSuite with Matchers with SwaggerSpecRunner {

  val swagger: String = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |paths:
    |  /foo/{pathparam}:
    |    get:
    |      operationId: getFoo
    |      produces:
    |        - application/json
    |      parameters:
    |        - name: bar
    |          in: query
    |          required: true
    |          type: string
    |          x-scala-type: Bar
    |        - name: pathparam
    |          in: path
    |          type: string
    |          x-scala-type: Bar
    |        - name: defaultparam
    |          in: query
    |          type: string
    |          x-scala-type: Bar
    |          required: true
    |          default: "i like spaces"
    |      responses:
    |        200:
    |          description: Success
    |          schema:
    |            $$ref: "#/definitions/Bar"
    |definitions:
    |  Bar:
    |    type: string
    |    enum:
    |      - v1
    |      - v2
    |      - i like spaces
    |""".stripMargin

  test("Generate enums") {
    val (
      ProtocolDefinitions(EnumDefinition(_, _, _, _, cls, staticDefns) :: Nil, _, _, _),
      _,
      _
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
    sealed abstract class Bar(val value: String) {
      override def toString: String = value.toString
    }
    """
    val companion  = q"""
    object Bar {
      object members {
        case object V1 extends Bar("v1")
        case object V2 extends Bar("v2")
        case object ILikeSpaces extends Bar("i like spaces")
      }
      val V1: Bar = members.V1
      val V2: Bar = members.V2
      val ILikeSpaces: Bar = members.ILikeSpaces
      val values = Vector(V1, V2, ILikeSpaces)

      implicit val encodeBar: Encoder[Bar] = Encoder[String].contramap(_.value)
      implicit val decodeBar: Decoder[Bar] = Decoder[String].emap(value => parse(value).toRight(s"$${value} not a member of Bar"))
      implicit val addPathBar: AddPath[Bar] = AddPath.build(_.value)
      implicit val showBar: Show[Bar] = Show.build(_.value)

      def parse(value: String): Option[Bar] = values.find(_.value == value)
      implicit val order: cats.Order[Bar] = cats.Order.by[Bar, Int](values.indexOf)
    }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
  }

  test("Use enums") {
    val (
      _,
      Clients(Client(tags, className, _, staticDefns, cls, _) :: _, Nil),
      _
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

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
        val getFooOKDecoder = {
          structuredJsonEntityUnmarshaller.flatMap(_ => _ => json => io.circe.Decoder[Bar].decodeJson(json).fold(FastFuture.failed, FastFuture.successful))
        }
        def getFoo(pathparam: Bar, bar: Bar, defaultparam: Bar = Bar.ILikeSpaces, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], GetFooResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/foo/" + Formatter.addPath(pathparam) + "?" + Formatter.addArg("bar", bar) + Formatter.addArg("defaultparam", defaultparam), allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              Unmarshal(resp.entity).to[Bar](getFooOKDecoder, implicitly, implicitly).map(x => Right(GetFooResponse.OK(x)))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
      }
    """

    cls.head.right.get.structure should equal(client.structure)
  }
}
