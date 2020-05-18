package core.issues

import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{ Assertion, EitherValues }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.concurrent.Future
import scala.concurrent.duration._
import org.scalactic.source

import _root_.tests.scalatest.EitherTValues

import io.circe.syntax._
import cats.implicits._

class Issue315Suite extends AnyFunSuite with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest {
  override implicit val patienceConfig = PatienceConfig(1000.millis, 1000.millis)

  def expectSuccess[A, B](value: Either[Either[Throwable, A], B])(implicit pos: source.Position): Unit = {
    val _ = value.right.value
  }

  def expectProtocolError[A, B](value: Either[Either[Throwable, A], B])(implicit pos: source.Position): Unit = {
    val _ = value.left.value.right.value
  }

  object client {
    def legacylegacy(httpClient: HttpRequest => Future[HttpResponse]) = {
      import _root_.issues.issue315.legacylegacy.client.akkaHttp.Client
      import _root_.issues.issue315.legacylegacy.client.akkaHttp.definitions._
      import _root_.issues.issue315.legacylegacy.client.akkaHttp.support.Presence
      val clientBody = TestObject(
        required = "foo",
        requiredNullable = None,
        optional = Presence.absent[String],
        optionalNullable = Presence.absent[Option[String]],
        legacy = None
      )
      Client
        .httpClient(httpClient)
        .postTest(clientBody)
        .map(_.fold(succeed, fail("Server business logic rejected request")))
        .value
        .futureValue
    }

    def legacyoptional(httpClient: HttpRequest => Future[HttpResponse]) = {
      import _root_.issues.issue315.legacyoptional.client.akkaHttp.Client
      import _root_.issues.issue315.legacyoptional.client.akkaHttp.definitions._
      import _root_.issues.issue315.legacyoptional.client.akkaHttp.support.Presence
      val clientBody = TestObject(
        required = "foo",
        requiredNullable = None,
        optional = Presence.absent[String],
        optionalNullable = Presence.absent[Option[String]],
        legacy = None
      )
      Client
        .httpClient(httpClient)
        .postTest(clientBody)
        .map(_.fold(succeed, fail("Server business logic rejected request")))
        .value
        .futureValue
    }

    def legacyrequired_nullable(httpClient: HttpRequest => Future[HttpResponse]) = {
      import _root_.issues.issue315.legacyrequired_nullable.client.akkaHttp.Client
      import _root_.issues.issue315.legacyrequired_nullable.client.akkaHttp.definitions._
      import _root_.issues.issue315.legacyrequired_nullable.client.akkaHttp.support.Presence
      val clientBody = TestObject(
        required = "foo",
        requiredNullable = None,
        optional = Presence.absent[String],
        optionalNullable = Presence.absent[Option[String]],
        legacy = None
      )
      Client
        .httpClient(httpClient)
        .postTest(clientBody)
        .map(_.fold(succeed, fail("Server business logic rejected request")))
        .value
        .futureValue
    }

    def optionallegacy(httpClient: HttpRequest => Future[HttpResponse]) = {
      import _root_.issues.issue315.optionallegacy.client.akkaHttp.Client
      import _root_.issues.issue315.optionallegacy.client.akkaHttp.definitions._
      import _root_.issues.issue315.optionallegacy.client.akkaHttp.support.Presence
      val clientBody = TestObject(
        required = "foo",
        requiredNullable = None,
        optional = Presence.absent[String],
        optionalNullable = Presence.absent[Option[String]],
        legacy = None
      )
      Client
        .httpClient(httpClient)
        .postTest(clientBody)
        .map(_.fold(succeed, fail("Server business logic rejected request")))
        .value
        .futureValue
    }

    def optionaloptional(httpClient: HttpRequest => Future[HttpResponse]) = {
      import _root_.issues.issue315.optionaloptional.client.akkaHttp.Client
      import _root_.issues.issue315.optionaloptional.client.akkaHttp.definitions._
      import _root_.issues.issue315.optionaloptional.client.akkaHttp.support.Presence
      val clientBody = TestObject(
        required = "foo",
        requiredNullable = None,
        optional = Presence.absent[String],
        optionalNullable = Presence.absent[Option[String]],
        legacy = Presence.absent
      )
      Client
        .httpClient(httpClient)
        .postTest(clientBody)
        .map(_.fold(succeed, fail("Server business logic rejected request")))
        .value
        .futureValue
    }

    def optionalrequired_nullable(httpClient: HttpRequest => Future[HttpResponse]) = {
      import _root_.issues.issue315.optionalrequired_nullable.client.akkaHttp.Client
      import _root_.issues.issue315.optionalrequired_nullable.client.akkaHttp.definitions._
      import _root_.issues.issue315.optionalrequired_nullable.client.akkaHttp.support.Presence
      val clientBody = TestObject(
        required = "foo",
        requiredNullable = None,
        optional = Presence.absent[String],
        optionalNullable = Presence.absent[Option[String]],
        legacy = None
      )
      Client
        .httpClient(httpClient)
        .postTest(clientBody)
        .map(_.fold(succeed, fail("Server business logic rejected request")))
        .value
        .futureValue
    }

    def required_nullablelegacy(httpClient: HttpRequest => Future[HttpResponse]) = {
      import _root_.issues.issue315.required_nullablelegacy.client.akkaHttp.Client
      import _root_.issues.issue315.required_nullablelegacy.client.akkaHttp.definitions._
      import _root_.issues.issue315.required_nullablelegacy.client.akkaHttp.support.Presence
      val clientBody = TestObject(
        required = "foo",
        requiredNullable = None,
        optional = Presence.absent[String],
        optionalNullable = Presence.absent[Option[String]],
        legacy = None
      )
      Client
        .httpClient(httpClient)
        .postTest(clientBody)
        .map(_.fold(succeed, fail("Server business logic rejected request")))
        .value
        .futureValue
    }

    def required_nullableoptional(httpClient: HttpRequest => Future[HttpResponse]) = {
      import _root_.issues.issue315.required_nullableoptional.client.akkaHttp.Client
      import _root_.issues.issue315.required_nullableoptional.client.akkaHttp.definitions._
      import _root_.issues.issue315.required_nullableoptional.client.akkaHttp.support.Presence
      val clientBody = TestObject(
        required = "foo",
        requiredNullable = None,
        optional = Presence.absent[String],
        optionalNullable = Presence.absent[Option[String]],
        legacy = None
      )
      Client
        .httpClient(httpClient)
        .postTest(clientBody)
        .map(_.fold(succeed, fail("Server business logic rejected request")))
        .value
        .futureValue
    }

    def required_nullablerequired_nullable(httpClient: HttpRequest => Future[HttpResponse]) = {
      import _root_.issues.issue315.required_nullablerequired_nullable.client.akkaHttp.Client
      import _root_.issues.issue315.required_nullablerequired_nullable.client.akkaHttp.definitions._
      import _root_.issues.issue315.required_nullablerequired_nullable.client.akkaHttp.support.Presence
      val clientBody = TestObject(
        required = "foo",
        requiredNullable = None,
        optional = Presence.absent[String],
        optionalNullable = Presence.absent[Option[String]],
        legacy = None
      )
      Client
        .httpClient(httpClient)
        .postTest(clientBody)
        .map(_.fold(succeed, fail("Server business logic rejected request")))
        .value
        .futureValue
    }
  }

  object server {
    val legacylegacy: HttpRequest => Future[HttpResponse] = {
      import _root_.issues.issue315.legacylegacy.server.akkaHttp.{ Handler, Resource }
      import _root_.issues.issue315.legacylegacy.server.akkaHttp.definitions._
      import _root_.issues.issue315.legacylegacy.server.akkaHttp.support.Presence
      Route.asyncHandler(Resource.routes(new Handler {
        def postTest(respond: Resource.postTestResponse.type)(body: TestObject) =
          body match {
            case TestObject("foo", None, Presence.Absent, Presence.Absent, None) => respond.OK.pure[Future]
            case _                                                               => respond.NotAcceptable.pure[Future]
          }
      }))
    }

    val legacyoptional: HttpRequest => Future[HttpResponse] = {
      import _root_.issues.issue315.legacyoptional.server.akkaHttp.{ Handler, Resource }
      import _root_.issues.issue315.legacyoptional.server.akkaHttp.definitions._
      import _root_.issues.issue315.legacyoptional.server.akkaHttp.support.Presence
      Route.asyncHandler(Resource.routes(new Handler {
        def postTest(respond: Resource.postTestResponse.type)(body: TestObject) =
          body match {
            case TestObject("foo", None, Presence.Absent, Presence.Absent, None) => respond.OK.pure[Future]
            case _                                                               => respond.NotAcceptable.pure[Future]
          }
      }))
    }

    val legacyrequired_nullable: HttpRequest => Future[HttpResponse] = {
      import _root_.issues.issue315.legacyrequired_nullable.server.akkaHttp.{ Handler, Resource }
      import _root_.issues.issue315.legacyrequired_nullable.server.akkaHttp.definitions._
      import _root_.issues.issue315.legacyrequired_nullable.server.akkaHttp.support.Presence
      Route.asyncHandler(Resource.routes(new Handler {
        def postTest(respond: Resource.postTestResponse.type)(body: TestObject) =
          body match {
            case TestObject("foo", None, Presence.Absent, Presence.Absent, None) => respond.OK.pure[Future]
            case _                                                               => respond.NotAcceptable.pure[Future]
          }
      }))
    }

    val optionallegacy: HttpRequest => Future[HttpResponse] = {
      import _root_.issues.issue315.optionallegacy.server.akkaHttp.{ Handler, Resource }
      import _root_.issues.issue315.optionallegacy.server.akkaHttp.definitions._
      import _root_.issues.issue315.optionallegacy.server.akkaHttp.support.Presence
      Route.asyncHandler(Resource.routes(new Handler {
        def postTest(respond: Resource.postTestResponse.type)(body: TestObject) =
          body match {
            case TestObject("foo", None, Presence.Absent, Presence.Absent, None) => respond.OK.pure[Future]
            case _                                                               => respond.NotAcceptable.pure[Future]
          }
      }))
    }

    val optionaloptional: HttpRequest => Future[HttpResponse] = {
      import _root_.issues.issue315.optionaloptional.server.akkaHttp.{ Handler, Resource }
      import _root_.issues.issue315.optionaloptional.server.akkaHttp.definitions._
      import _root_.issues.issue315.optionaloptional.server.akkaHttp.support.Presence
      Route.asyncHandler(Resource.routes(new Handler {
        def postTest(respond: Resource.postTestResponse.type)(body: TestObject) =
          body match {
            case TestObject("foo", None, Presence.Absent, Presence.Absent, Presence.Absent) => respond.OK.pure[Future]
            case _                                                                          => respond.NotAcceptable.pure[Future]
          }
      }))
    }

    val optionalrequired_nullable: HttpRequest => Future[HttpResponse] = {
      import _root_.issues.issue315.optionalrequired_nullable.server.akkaHttp.{ Handler, Resource }
      import _root_.issues.issue315.optionalrequired_nullable.server.akkaHttp.definitions._
      import _root_.issues.issue315.optionalrequired_nullable.server.akkaHttp.support.Presence
      Route.asyncHandler(Resource.routes(new Handler {
        def postTest(respond: Resource.postTestResponse.type)(body: TestObject) =
          body match {
            case TestObject("foo", None, Presence.Absent, Presence.Absent, None) => respond.OK.pure[Future]
            case _                                                               => respond.NotAcceptable.pure[Future]
          }
      }))
    }

    val required_nullablelegacy: HttpRequest => Future[HttpResponse] = {
      import _root_.issues.issue315.required_nullablelegacy.server.akkaHttp.{ Handler, Resource }
      import _root_.issues.issue315.required_nullablelegacy.server.akkaHttp.definitions._
      import _root_.issues.issue315.required_nullablelegacy.server.akkaHttp.support.Presence
      Route.asyncHandler(Resource.routes(new Handler {
        def postTest(respond: Resource.postTestResponse.type)(body: TestObject) =
          body match {
            case TestObject("foo", None, Presence.Absent, Presence.Absent, None) => respond.OK.pure[Future]
            case _                                                               => respond.NotAcceptable.pure[Future]
          }
      }))
    }

    val required_nullableoptional: HttpRequest => Future[HttpResponse] = {
      import _root_.issues.issue315.required_nullableoptional.server.akkaHttp.{ Handler, Resource }
      import _root_.issues.issue315.required_nullableoptional.server.akkaHttp.definitions._
      import _root_.issues.issue315.required_nullableoptional.server.akkaHttp.support.Presence
      Route.asyncHandler(Resource.routes(new Handler {
        def postTest(respond: Resource.postTestResponse.type)(body: TestObject) =
          body match {
            case TestObject("foo", None, Presence.Absent, Presence.Absent, None) => respond.OK.pure[Future]
            case _                                                               => respond.NotAcceptable.pure[Future]
          }
      }))
    }

    val required_nullablerequired_nullable: HttpRequest => Future[HttpResponse] = {
      import _root_.issues.issue315.required_nullablerequired_nullable.server.akkaHttp.{ Handler, Resource }
      import _root_.issues.issue315.required_nullablerequired_nullable.server.akkaHttp.definitions._
      import _root_.issues.issue315.required_nullablerequired_nullable.server.akkaHttp.support.Presence
      Route.asyncHandler(Resource.routes(new Handler {
        def postTest(respond: Resource.postTestResponse.type)(body: TestObject) =
          body match {
            case TestObject("foo", None, Presence.Absent, Presence.Absent, None) => respond.OK.pure[Future]
            case _                                                               => respond.NotAcceptable.pure[Future]
          }
      }))
    }
  }

  test("expected protocol rejections") {
    // `"legacy": null` (from required/nullable) incompatible with `optional` decoder
    expectProtocolError(client.required_nullableoptional(server.legacyoptional))
    expectProtocolError(client.required_nullableoptional(server.optionaloptional))
    expectProtocolError(client.required_nullableoptional(server.required_nullableoptional))

    // `"legacy": null` (from legacy encoder) incompatible with `optional` decoder
    expectProtocolError(client.legacylegacy(server.legacyoptional))
    expectProtocolError(client.legacylegacy(server.optionaloptional))
    expectProtocolError(client.legacylegacy(server.required_nullableoptional))
    expectProtocolError(client.legacyoptional(server.legacyoptional))
    expectProtocolError(client.legacyoptional(server.optionaloptional))
    expectProtocolError(client.legacyoptional(server.required_nullableoptional))
    expectProtocolError(client.legacyrequired_nullable(server.legacyoptional))
    expectProtocolError(client.legacyrequired_nullable(server.optionaloptional))
    expectProtocolError(client.legacyrequired_nullable(server.required_nullableoptional))

    // `"legacy": null` incompatible with `optional` decoder
    expectProtocolError(client.required_nullablelegacy(server.legacyoptional))
    expectProtocolError(client.required_nullablelegacy(server.optionaloptional))
    expectProtocolError(client.required_nullablelegacy(server.required_nullableoptional))
    expectProtocolError(client.required_nullablerequired_nullable(server.legacyoptional))
    expectProtocolError(client.required_nullablerequired_nullable(server.optionaloptional))
    expectProtocolError(client.required_nullablerequired_nullable(server.required_nullableoptional))

    // Missing `legacy` incompatible with `required_nullable`
    expectProtocolError(client.optionallegacy(server.legacyrequired_nullable))
    expectProtocolError(client.optionallegacy(server.optionalrequired_nullable))
    expectProtocolError(client.optionallegacy(server.required_nullablerequired_nullable))
    expectProtocolError(client.optionaloptional(server.legacyrequired_nullable))
    expectProtocolError(client.optionaloptional(server.optionalrequired_nullable))
    expectProtocolError(client.optionaloptional(server.required_nullablerequired_nullable))
    expectProtocolError(client.optionalrequired_nullable(server.legacyrequired_nullable))
    expectProtocolError(client.optionalrequired_nullable(server.optionalrequired_nullable))
    expectProtocolError(client.optionalrequired_nullable(server.required_nullablerequired_nullable))
  }

  test("expect success") {
    expectSuccess(client.legacylegacy(server.legacylegacy))
    expectSuccess(client.legacylegacy(server.legacyrequired_nullable))
    expectSuccess(client.legacylegacy(server.optionallegacy))
    expectSuccess(client.legacylegacy(server.optionalrequired_nullable))
    expectSuccess(client.legacylegacy(server.required_nullablelegacy))
    expectSuccess(client.legacylegacy(server.required_nullablerequired_nullable))
    expectSuccess(client.legacyoptional(server.legacylegacy))
    expectSuccess(client.legacyoptional(server.legacyrequired_nullable))
    expectSuccess(client.legacyoptional(server.optionallegacy))
    expectSuccess(client.legacyoptional(server.optionalrequired_nullable))
    expectSuccess(client.legacyoptional(server.required_nullablelegacy))
    expectSuccess(client.legacyoptional(server.required_nullablerequired_nullable))
    expectSuccess(client.legacyrequired_nullable(server.legacylegacy))
    expectSuccess(client.legacyrequired_nullable(server.legacyrequired_nullable))
    expectSuccess(client.legacyrequired_nullable(server.optionallegacy))
    expectSuccess(client.legacyrequired_nullable(server.optionalrequired_nullable))
    expectSuccess(client.legacyrequired_nullable(server.required_nullablelegacy))
    expectSuccess(client.legacyrequired_nullable(server.required_nullablerequired_nullable))
    expectSuccess(client.optionallegacy(server.legacylegacy))
    expectSuccess(client.optionallegacy(server.legacyoptional))
    expectSuccess(client.optionallegacy(server.optionallegacy))
    expectSuccess(client.optionallegacy(server.optionaloptional))
    expectSuccess(client.optionallegacy(server.required_nullablelegacy))
    expectSuccess(client.optionallegacy(server.required_nullableoptional))
    expectSuccess(client.optionaloptional(server.legacylegacy))
    expectSuccess(client.optionaloptional(server.legacyoptional))
    expectSuccess(client.optionaloptional(server.optionallegacy))
    expectSuccess(client.optionaloptional(server.optionaloptional))
    expectSuccess(client.optionaloptional(server.required_nullablelegacy))
    expectSuccess(client.optionaloptional(server.required_nullableoptional))
    expectSuccess(client.optionalrequired_nullable(server.legacylegacy))
    expectSuccess(client.optionalrequired_nullable(server.legacyoptional))
    expectSuccess(client.optionalrequired_nullable(server.optionallegacy))
    expectSuccess(client.optionalrequired_nullable(server.optionaloptional))
    expectSuccess(client.optionalrequired_nullable(server.required_nullablelegacy))
    expectSuccess(client.optionalrequired_nullable(server.required_nullableoptional))
    expectSuccess(client.required_nullablelegacy(server.legacylegacy))
    expectSuccess(client.required_nullablelegacy(server.legacyrequired_nullable))
    expectSuccess(client.required_nullablelegacy(server.optionallegacy))
    expectSuccess(client.required_nullablelegacy(server.optionalrequired_nullable))
    expectSuccess(client.required_nullablelegacy(server.required_nullablelegacy))
    expectSuccess(client.required_nullablelegacy(server.required_nullablerequired_nullable))
    expectSuccess(client.required_nullableoptional(server.legacylegacy))
    expectSuccess(client.required_nullableoptional(server.legacyrequired_nullable))
    expectSuccess(client.required_nullableoptional(server.optionallegacy))
    expectSuccess(client.required_nullableoptional(server.optionalrequired_nullable))
    expectSuccess(client.required_nullableoptional(server.required_nullablelegacy))
    expectSuccess(client.required_nullableoptional(server.required_nullablerequired_nullable))
    expectSuccess(client.required_nullablerequired_nullable(server.legacylegacy))
    expectSuccess(client.required_nullablerequired_nullable(server.legacyrequired_nullable))
    expectSuccess(client.required_nullablerequired_nullable(server.optionallegacy))
    expectSuccess(client.required_nullablerequired_nullable(server.optionalrequired_nullable))
    expectSuccess(client.required_nullablerequired_nullable(server.required_nullablelegacy))
    expectSuccess(client.required_nullablerequired_nullable(server.required_nullablerequired_nullable))
  }
}
