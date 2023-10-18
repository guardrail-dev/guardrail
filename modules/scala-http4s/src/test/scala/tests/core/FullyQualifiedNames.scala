package tests.core

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.meta._

import support.{ ScalaMetaMatchers, SwaggerSpecRunner }

import dev.guardrail.Context
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.http4s.Http4sVersion
import dev.guardrail.generators.{ Client, Clients }
import dev.guardrail.terms.protocol.ClassDefinition

class FullyQualifiedNames extends AnyFunSuite with Matchers with SwaggerSpecRunner with ScalaMetaMatchers {

  val spec =
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

  def testVersion(version: Http4sVersion): Unit =
    test(s"$version - Test that fully qualified names are used") {
      val (
        ProtocolDefinitions(List(clz @ ClassDefinition(_, _, fullType, _, _, _)), _, _, _, _),
        Clients(List(Client(_, _, _, _, client, List(respTrait, respObject))), _),
        _
      ) = runSwaggerSpec(scalaInterpreter)(spec, List("_root_", "com", "test"))(Context.empty, version.value)

      clz.fullType shouldEqual t"_root_.com.test.User"
      client.head.toOption.get should matchStructure(q"""
        class Client[F[_]](host: String)(implicit F: Async[F], httpClient: Http4sClient[F]) {
          val basePath: String = ""
          private def parseOptionalHeader(response: Response[F], header: String): F[Option[String]] = F.pure(response.headers.get(CIString(header)).map(_.head.value))
          private def parseRequiredHeader(response: Response[F], header: String): F[String] = response.headers.get(CIString(header)).map(_.head.value).fold[F[String]](F.raiseError(ParseFailure("Missing required header.", s"HTTP header '$$header' is not present.")))(F.pure)
          private[this] val getUserOkDecoder = jsonOf[F, _root_.com.test.User]
          def getUser(id: String, headers: List[Header.ToRaw] = List.empty): F[GetUserResponse] = {
            val allHeaders: List[org.http4s.Header.ToRaw] = List.empty[Header.ToRaw] ++ headers ++ List[Option[Header.ToRaw]]().flatten
            val req = Request[F](method = Method.GET, uri = Uri.unsafeFromString(host + basePath + "/user/" + Formatter.addPath(id)), headers = Headers(allHeaders))
            httpClient.run(req).use({
              case _root_.org.http4s.Status.Ok(resp) =>
                F.map(getUserOkDecoder.decode(resp, strict = false).value.flatMap(F.fromEither))(GetUserResponse.Ok.apply): F[GetUserResponse]
              case resp =>
                F.raiseError[GetUserResponse](UnexpectedStatus(resp.status, Method.GET, req.uri))
            })
          }
        }
      """)

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

  testVersion(Http4sVersion.V0_22)
  testVersion(Http4sVersion.V0_23)
}
