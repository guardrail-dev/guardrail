package core.issues

import com.twilio.guardrail.generators.Scala.AkkaHttp
import com.twilio.guardrail.generators.syntax.Scala.companionForStaticDefns
import com.twilio.guardrail.{ Client, Clients, Context, Server, Servers }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import support.SwaggerSpecRunner
import org.scalactic.source

import scala.meta._

class Issue314 extends AnyFunSuite with Matchers with SwaggerSpecRunner {

  private def swagger(config: String) =
    s"""
       |swagger: '2.0'
       |host: localhost:1234
       |paths:
       |  /user/{id}:
       |    get:
       |      operationId: getUser
       |      $config
       |      consumes:
       |        - application/json
       |      produces:
       |        - application/json
       |      parameters:
       |      - name: id
       |        in: path
       |        type: string
       |      responses:
       |        200:
       |          description: Successful
       |""".stripMargin

  private val flavours = List("jvm", "scala")

  test("Ensure class name can be arbitrary") {
    flavours.foreach { flavour =>
      verify(s"""
           |      x-$flavour-package: users
           |      x-$flavour-class-prefix: hello
           """.stripMargin, "Hello")
    }
  }

  test("Ensure last component of package name is used as class name") {
    flavours.foreach { flavour =>
      verify(s"""
           |      x-$flavour-package: users
           """.stripMargin, "Users")
    }
  }

  test("Ensure class name is determined by tags") {
    verify(s"""
         |      tags: [hello]
       """.stripMargin, "Hello")
  }

  test("Ensure class name can be overridden") {
    flavours.foreach { flavour =>
      verify(
        s"""
           |      tags: [whatever]
           |      x-$flavour-package: users
           |      x-$flavour-class-prefix: hello
           """.stripMargin,
        "Hello"
      )
    }
  }

  private def client(prefix: String) =
    s"""class ${prefix}Client(host: String = "http://localhost:1234")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer) {
       |  val basePath: String = ""
       |  private[this] def makeRequest[T: ToEntityMarshaller](method: HttpMethod, uri: Uri, headers: scala.collection.immutable.Seq[HttpHeader], entity: T, protocol: HttpProtocol): EitherT[Future, Either[Throwable, HttpResponse], HttpRequest] = {
       |    EitherT(Marshal(entity).to[RequestEntity].map[Either[Either[Throwable, HttpResponse], HttpRequest]] {
       |      entity => Right(HttpRequest(method = method, uri = uri, headers = headers, entity = entity, protocol = protocol))
       |    }.recover({
       |      case t =>
       |        Left(Left(t))
       |    }))
       |  }
       |  def getUser(id: String, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], GetUserResponse] = {
       |    val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
       |    makeRequest(HttpMethods.GET, host + basePath + "/user/" + Formatter.addPath(id), allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
       |      case StatusCodes.OK =>
       |        resp.discardEntityBytes().future.map(_ => Right(GetUserResponse.OK))
       |      case _ =>
       |        FastFuture.successful(Left(Right(resp)))
       |    }).recover({
       |      case e: Throwable =>
       |        Left(Left(e))
       |    })))
       |  }
       |}""".stripMargin

  private def companion(prefix: String) =
    s"""object ${prefix}Client {
       |  def apply(host: String = "http://localhost:1234")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer): ${prefix}Client = new ${prefix}Client(host = host)(httpClient = httpClient, ec = ec, mat = mat)
       |  def httpClient(httpClient: HttpRequest => Future[HttpResponse], host: String = "http://localhost:1234")(implicit ec: ExecutionContext, mat: Materializer): ${prefix}Client = new ${prefix}Client(host = host)(httpClient = httpClient, ec = ec, mat = mat)
       |}""".stripMargin

  private def handler(prefix: String) =
    s"trait ${prefix}Handler { def getUser(respond: ${prefix}Resource.GetUserResponse.type)(id: String): scala.concurrent.Future[${prefix}Resource.GetUserResponse] }"

  private def resource(prefix: String) =
    s"""object ${prefix}Resource {
       |  def routes(handler: ${prefix}Handler)(implicit mat: akka.stream.Materializer): Route = {
       |    {
       |      path("user" / Segment).apply(id => get(discardEntity(complete(handler.getUser(GetUserResponse)(id)))))
       |    }
       |  }
       |  sealed abstract class GetUserResponse(val statusCode: StatusCode)
       |  case object GetUserResponseOK extends GetUserResponse(StatusCodes.OK)
       |  object GetUserResponse {
       |    implicit val getUserResponseTRM: ToResponseMarshaller[GetUserResponse] = Marshaller { implicit ec =>
       |      resp => getUserResponseTR(resp)
       |    }
       |    implicit def getUserResponseTR(value: GetUserResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
       |      case r: GetUserResponseOK.type =>
       |        scala.concurrent.Future.successful(Marshalling.Opaque {
       |          () => HttpResponse(r.statusCode)
       |        } :: Nil)
       |    }
       |    def apply[T](value: T)(implicit ev: T => GetUserResponse): GetUserResponse = ev(value)
       |    def OK: GetUserResponse = GetUserResponseOK
       |  }
       |}""".stripMargin

  private def verify(config: String, expectedClassPrefix: String)(implicit pos: source.Position): Unit = {
    verifyClient(config, expectedClassPrefix)
    verifyServer(config, expectedClassPrefix)
  }

  private def verifyClient(config: String, expectedClassPrefix: String)(implicit pos: source.Position): Unit = {
    val (
      _,
      Clients(Client(_, _, _, staticDefns, cls, _) :: _, Nil),
      _
    )       = runSwaggerSpec(swagger(config))(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

    verifyTree(cls.head.right.get, client(expectedClassPrefix))
    verifyTree(cmp, companion(expectedClassPrefix))
  }

  private def verifyServer(config: String, expectedClassPrefix: String)(implicit pos: source.Position): Unit = {
    val (
      _,
      _,
      Servers(Server(_, _, genHandler, genResource :: Nil) :: Nil, Nil)
    ) = runSwaggerSpec(swagger(config))(Context.empty, AkkaHttp)

    verifyTree(genHandler, handler(expectedClassPrefix))
    verifyTree(genResource, resource(expectedClassPrefix))
  }

  private def verifyTree(tree: scala.meta.Tree, expectedSyntax: String)(implicit pos: source.Position): Unit =
    normalized(tree.syntax) shouldBe normalized(expectedSyntax)

  private def normalized(s: String): String = s.replaceAll("(?s)\\s+", " ").trim

}
