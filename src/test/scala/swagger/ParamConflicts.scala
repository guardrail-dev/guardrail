package swagger

import _root_.io.swagger.parser.SwaggerParser
import cats.instances.all._
import com.twilio.swagger.codegen.generators.AkkaHttp
import com.twilio.swagger.codegen.{ClassDefinition, Client, Clients, Context, ClientGenerator, CodegenApplication, ProtocolGenerator, Target}
import org.scalatest.{FunSuite, Matchers}
import scala.meta._

class ParamConflictsTest extends FunSuite with Matchers {

  val swagger = new SwaggerParser().parse(s"""
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
    |""".stripMargin)

  test("Generate non-conflicting names in clients") {
    val Clients(Client(tags, className, statements) :: _, _) = Target.unsafeExtract(ClientGenerator.fromSwagger[CodegenApplication](Context.empty, swagger)(List.empty).foldMap(AkkaHttp))

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
      def getFoo(conflicting_name: String, ConflictingName: String, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
        val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
        wrap[IgnoredEntity](Marshal(FormData(List(("conflicting_name", Some(Formatter.show(conflicting_name))), ("ConflictingName", Some(Formatter.show(ConflictingName)))).collect({
          case (n, Some(v)) =>
            (n, v)
        }): _*)).to[RequestEntity].flatMap {
          entity => httpClient(HttpRequest(method = HttpMethods.GET, uri = host + basePath + "/foo", entity = entity, headers = allHeaders))
        })
      }
    }
    """

    cls.structure should equal(client.structure)
  }

  test("Generate non-conflicting names in definitions") {
    val definitions = Target.unsafeExtract(ProtocolGenerator.fromSwagger[CodegenApplication](swagger).foldMap(AkkaHttp)).elems
    val ClassDefinition(_, cls, cmp) :: _ = definitions

    val definition = q"""
      case class Foo(conflicting_name: Option[String] = None, ConflictingName: Option[String] = None)
    """
    val companion = q"""
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
