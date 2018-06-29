package tests.generators.akkaHttp

import com.twilio.guardrail._
import com.twilio.guardrail.generators.AkkaHttp
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import com.twilio.guardrail.tests._
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
      ProtocolDefinitions(EnumDefinition(_, _, _, cls, cmp) :: Nil, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)

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
      def parse(value: String): Option[Bar] = values.find(_.value == value)

      implicit val encodeBar: Encoder[Bar] = Encoder[String].contramap(_.value)
      implicit val decodeBar: Decoder[Bar] = Decoder[String].emap(value => parse(value).toRight(s"$${value} not a member of Bar"))
      implicit val addPathBar: AddPath[Bar] = AddPath.build(_.value)
      implicit val showBar: Show[Bar] = Show.build(_.value)
    }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
  }

  test("Use enums") {
    val (
      _,
      Clients(Client(tags, className, statements) :: _),
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)

    val List(cmp, cls) = statements.dropWhile(_.isInstanceOf[Import])

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
        def getFoo(pathparam: Bar, bar: Bar, defaultparam: Bar = Bar.ILikeSpaces, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], Bar] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          wrap[Bar](Marshal(HttpEntity.Empty).to[RequestEntity].flatMap { entity =>
            httpClient(HttpRequest(method = HttpMethods.GET, uri = host + basePath + "/foo/" + Formatter.addPath(pathparam) + "?" + Formatter.addArg("bar", bar) + Formatter.addArg("defaultparam", defaultparam), entity = entity, headers = allHeaders))
          })
        }
      }
    """

    cls.structure should equal(client.structure)
  }
}
