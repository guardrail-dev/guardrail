package core.issues

import _root_.department.client.http4sV022.department.DepartmentClient
import _root_.department.client.http4sV022.department.{ GetDepartmentResponse => ClientGDR }
import _root_.department.client.http4sV022.department.{ SearchDepartmentsResponse => ClientSDR }
import _root_.department.client.http4sV022.{ definitions => cdefs }
import _root_.department.server.http4sV022.department.DepartmentResource._
import _root_.department.server.http4sV022.department._
import _root_.department.server.http4sV022.{ definitions => sdefs }
import cats.effect.IO
import cats.effect.IO._
import org.http4s.Request
import org.http4s.Response
import org.http4s.client.Client
import org.http4s.implicits._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Issue1229Suite extends AnyFunSuite with Matchers {

  type AuthContext = Unit
  val dummyAuth = (_: Request[IO]) => IO.pure[Option[AuthContext]](Some(()))

  test("round-trip: definition query, unit response") {
    val httpService = new DepartmentResource(dummyAuth).routes(new DepartmentHandler[IO, AuthContext] {
      val fooDept = sdefs.Department("123", "foo", "bar")
      def getDepartment(respond: GetDepartmentResponse.type)(a: Option[AuthContext], id: String): cats.effect.IO[GetDepartmentResponse] =
        IO.pure(respond.Ok(fooDept))
      def searchDepartments(
          respond: SearchDepartmentsResponse.type
      )(query: Option[String], page: Int, pageSize: Int, sort: Option[Iterable[String]]): cats.effect.IO[SearchDepartmentsResponse] =
        IO.pure(respond.Ok(sdefs.DepartmentSearchResponse(Vector(fooDept), 0, 0, 0)))
    })

    val departmentClient = DepartmentClient.httpClient(Client.fromHttpApp(httpService.orNotFound), "http://localhost")

    val fooDept = cdefs.Department("123", "foo", "bar")
    departmentClient
      .getDepartment(fooDept.id)
      .attempt
      .unsafeRunSync() should be(Right(ClientGDR.Ok(fooDept)))

    departmentClient
      .searchDepartments(None, 0, 0, None)
      .attempt
      .unsafeRunSync() should be(Right(ClientSDR.Ok(cdefs.DepartmentSearchResponse(Vector(fooDept), 0, 0, 0))))
  }
}
