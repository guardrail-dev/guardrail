package tests.core

import com.twilio.guardrail.generators.Http4s
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.{ ClassDefinition, Client, Clients, Context, ProtocolDefinitions, Server, Servers }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner

import scala.meta._

class FullyQualifiedNames extends FunSuite with Matchers with SwaggerSpecRunner {

  val swagger =
    """
      |swagger: "2.0"
      |definitions:
      |  User:
      |    type: object
      |    required:
      |      - id
      |      - address
      |    properties:
      |      id:
      |        type: string
      |paths:
      |  /user/{id}:
      |    get:
      |      operationId: getUser
      |      parameters:
      |      - name: id
      |        in: path
      |        type: string
      |      responses:
      |        '200':
      |          schema:
      |            $ref: '#/definitions/User'
      |""".stripMargin

  test("Test that fully qualified names are used") {
    val (
      ProtocolDefinitions(List(clz @ ClassDefinition(_, _, fullType, _, _, _)), _, _, _),
      Clients(List(Client(_, _, _, _, client, List(respTrait, respObject))), _),
      _
    ) = runSwaggerSpec(swagger, List("_root_", "com", "test"))(Context.empty, Http4s)

    clz.fullType shouldEqual t"_root_.com.test.User"
    client.head.toOption.get shouldEqual q"""
       class Client[F[_]](host: String)(implicit F: Async[F], httpClient: Http4sClient[F]) {
         val basePath: String = ""
         private def parseOptionalHeader(response: Response[F], header: String): F[Option[String]] = F.pure(response.headers.get(header.ci).map(_.value))
         private def parseRequiredHeader(response: Response[F], header: String): F[String] = response.headers.get(header.ci).map(_.value).fold[F[String]](F.raiseError(ParseFailure("Missing required header.", s"HTTP header '$$header' is not present.")))(F.pure)
         private[this] val getUserOkDecoder = jsonOf[F, _root_.com.test.User]
         def getUser(id: String, headers: List[Header] = List.empty): F[GetUserResponse] = {
           val allHeaders = headers ++ List[Option[Header]]().flatten
           val req = Request[F](method = Method.GET, uri = Uri.unsafeFromString(host + basePath + "/user/" + Formatter.addPath(id)), headers = Headers(allHeaders))
           httpClient.fetch(req)({
             case _root_.org.http4s.Status.Ok(resp) =>
               F.map(getUserOkDecoder.decode(resp, strict = false).value.flatMap(F.fromEither))(GetUserResponse.Ok.apply): F[GetUserResponse]
             case resp =>
               F.raiseError[GetUserResponse](UnexpectedStatus(resp.status))
           })
         }
       }
    """

    respTrait shouldEqual
      q"""
        sealed abstract class GetUserResponse {
          def fold[A](handleOk: _root_.com.test.User => A): A = this match {
          case x: GetUserResponse.Ok =>
            handleOk(x.value)
          }
        }
       """

    respObject shouldEqual q"""object GetUserResponse { case class Ok(value: _root_.com.test.User) extends GetUserResponse }"""

  }
}
