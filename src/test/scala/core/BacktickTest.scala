package tests.core

import cats.instances.all._
import collection.JavaConverters._
import com.twilio.swagger._
import com.twilio.swagger.codegen.generators.AkkaHttp
import com.twilio.swagger.codegen.{Context, ClassDefinition, EnumDefinition, Client, Clients, ClientGenerator, ProtocolGenerator, ProtocolDefinitions, CodegenApplication, Target}
import org.scalatest.{FunSuite, Matchers}
import scala.meta._

class BacktickTest extends FunSuite with Matchers {

  val swagger = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |paths:
    |  /dashy-route:
    |    get:
    |      tags: ["dashy-package"]
    |      x-scala-package: dashy-package
    |      operationId: dashy-op-id
    |      produces:
    |        - application/json
    |      parameters:
    |        - name: dashy-parameter
    |          in: query
    |          required: true
    |          type: string
    |      responses:
    |        200:
    |          description: Success
    |          schema:
    |            $$ref: "#/definitions/dashy-class"
    |    post:
    |      tags: ["dashy-package"]
    |      x-scala-package: dashy-package
    |      operationId: postDashy-op-id
    |      parameters:
    |        - name: dashy-parameter
    |          in: query
    |          required: true
    |          type: string
    |      responses:
    |        200:
    |          description: Success
    |          schema:
    |            $$ref: "#/definitions/dashy-class"
    |definitions:
    |  dashy-class:
    |    type: object
    |    properties:
    |      dashy-param:
    |        type: integer
    |        format: int64
    |  dashy-enum:
    |    type: string
    |    enum:
    |      - dashy-value-a
    |      - dashy-value-b
    |      - dashy-value.c
    |""".stripMargin

  test("Ensure paths are generated with escapes") {
    val (
      _,
      Clients(Client(tags, className, statements) :: _),
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)

    tags should equal (Seq("dashy-package"))

    val List(cmp, cls) = statements.dropWhile(_.isInstanceOf[Import])

    val client = q"""
    class ${Type.Name("`Dashy-packageClient`")}(host: String = "http://localhost:1234")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer) {
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
      def ${Term.Name("`dashy-op-id`")}(${Term.Name("dashyParameter")}: String, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], ${Type.Name("`dashy-class`")}] = {
        val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
        wrap[${Type.Name("`dashy-class`")}](Marshal(HttpEntity.Empty).to[RequestEntity].flatMap { entity =>
          httpClient(HttpRequest(method = HttpMethods.GET, uri = host + basePath + "/dashy-route" + "?" + Formatter.addArg("dashy-parameter", dashyParameter), entity = entity, headers = allHeaders))
        })
      }
      def ${Term.Name("`postDashy-op-id`")}(dashyParameter: String, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], ${Type.Name("`dashy-class`")}] = {
        val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
        wrap[${Type.Name("`dashy-class`")}](Marshal(HttpEntity.Empty).to[RequestEntity].flatMap { entity =>
          httpClient(HttpRequest(method = HttpMethods.POST, uri = host + basePath + "/dashy-route" + "?" + Formatter.addArg("dashy-parameter", dashyParameter), entity = entity, headers = allHeaders))
        })
      }

    }
    """

    cls.structure should equal(client.structure)
    cls.toString should include("class `Dashy-packageClient`")
    cls.toString should include("def `dashy-op-id`")
    cls.toString should include("dashyParameter: String")
    cls.toString should include("\"dashy-parameter\", dashyParameter")
    cls.toString shouldNot include ("``")

    // Note regarding: def ${Term.Name("post /dashy-route")}
    //   This matches the expected behavior of scala.meta regarding terms that contain spaces.
    //   Were this term name to be wrapped in backticks, the test would fail due to scalameta
    //   automatically stripping the backticks. The following test ensures that even though
    //   the test doesn't follow the pattern, the generated code is still escaped.
    //   This behavior may change in scalameta 2.0.0+
    cls.toString should include("def `postDashy-op-id`(dashyParameter")

    cmp.toString shouldNot include ("``")
  }

  test("Ensure dtos are generated with escapes") {
    val (
      ProtocolDefinitions(ClassDefinition(_, _, cls, cmp) :: _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)

    val definition = q"""
    case class ${Type.Name("`dashy-class`")}(${Term.Name("`dashy-param`")}: Option[Long] = None)
    """
    val companion = q"""
    object ${Term.Name("`dashy-class`")} {
      implicit val ${Pat.Var(Term.Name("`encodedashy-class`"))} = {
        val readOnlyKeys = Set[String]()
        Encoder.forProduct1("dashy-param")((o: ${Type.Name("`dashy-class`")}) => o.${Term.Name("`dashy-param`")}).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
      }
      implicit val ${Pat.Var(Term.Name("`decodedashy-class`"))} = Decoder.forProduct1("dashy-param")(${Term.Name("`dashy-class`")}.apply _)
    }
    """

    cls.structure should equal(definition.structure)
    cls.toString should include("case class `dashy-class`")
    cls.toString should include("`dashy-param`")
    cls.toString shouldNot include ("``")

    cmp.structure should equal(companion.structure)
    cmp.toString should include ("`encodedashy-class`")
    cmp.toString should include ("`decodedashy-class`")
    cmp.toString should include ("(`dashy-class`.apply _)")
    cmp.toString shouldNot include (".dashy-")
    cmp.toString shouldNot include ("[dashy-")
    cmp.toString shouldNot include ("= dashy-")
    cmp.toString shouldNot include ("``")
  }

  test("Ensure enums are generated with escapes") {
    val (
      ProtocolDefinitions(_ :: EnumDefinition(_, _, _, cls, cmp) :: _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)

    val definition = q"""
    sealed abstract class ${Type.Name("`dashy-enum`")}(val value: String) {
      override def toString: String = value.toString
    }
    """
    val companion = q"""
    object ${Term.Name("`dashy-enum`")} {
      object members {
        case object DashyValueA extends ${Type.Name("`dashy-enum`")}("dashy-value-a")
        case object DashyValueB extends ${Type.Name("`dashy-enum`")}("dashy-value-b")
        case object DashyValueC extends ${Type.Name("`dashy-enum`")}("dashy-value.c")
      }
      val DashyValueA: ${Type.Name("`dashy-enum`")} = members.DashyValueA
      val DashyValueB: ${Type.Name("`dashy-enum`")} = members.DashyValueB
      val DashyValueC: ${Type.Name("`dashy-enum`")} = members.DashyValueC
      val values = Vector(DashyValueA, DashyValueB, DashyValueC)
      def parse(value: String): Option[${Type.Name("`dashy-enum`")}] = values.find(_.value == value)
      implicit val ${Pat.Var(Term.Name("`encodedashy-enum`"))}: Encoder[${Type.Name("`dashy-enum`")}] = Encoder[String].contramap(_.value)
      implicit val ${Pat.Var(Term.Name("`decodedashy-enum`"))}: Decoder[${Type.Name("`dashy-enum`")}] = Decoder[String].emap(value => parse(value).toRight(s"$$value not a member of dashy-enum"))
      implicit val ${Pat.Var(Term.Name("`addPathdashy-enum`"))}: AddPath[${Type.Name("`dashy-enum`")}] = AddPath.build(_.value)
      implicit val ${Pat.Var(Term.Name("`showdashy-enum`"))}: Show[${Type.Name("`dashy-enum`")}] = Show.build(_.value)
    }
    """

    cls.structure should equal(definition.structure)
    cls.toString should include("sealed abstract class `dashy-enum`")
    cls.toString shouldNot include ("``")

    cmp.structure should equal(companion.structure)
    cmp.toString should include ("val DashyValueA")
    cmp.toString should include ("case object DashyValueB")
    cmp.toString should include ("`encodedashy-enum`")
    cmp.toString should include ("`decodedashy-enum`")
    cmp.toString should include ("def parse(value: String): Option[`dashy-enum`]")
    cmp.toString should include ("DashyValueC")
    cmp.toString shouldNot include (".dashy-")
    cmp.toString shouldNot include ("[dashy-")
    cmp.toString shouldNot include ("= dashy-")
    cmp.toString shouldNot include ("``")
  }
}
