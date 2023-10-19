package core.issues

import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.unmarshalling.{ FromEntityUnmarshaller, Unmarshal, Unmarshaller }
import cats.instances.future._
import io.circe._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.SpanSugar._
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import scala.concurrent.Future
import scala.concurrent.duration.{ Duration, SECONDS }

class Issue357Suite extends AnyFunSpec with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest {
  override implicit val patienceConfig: PatienceConfig = PatienceConfig(10 seconds, 1 second)

  describe("akka-http server should") {
    import issues.issue357.server.akkaHttp.{ Handler, Resource }
    val route = Resource.routes(new Handler {
      def deleteFoo(respond: Resource.DeleteFooResponse.type)(path: String, query: String, form: String): Future[Resource.DeleteFooResponse] =
        Future.successful((path, query, form) match {
          case ("1234", "2345", "3456")             => respond.NoContent
          case ("foo", "bar", "baz")                => respond.NoContent
          case ("\"qfoo\"", "\"qbar\"", "\"qbaz\"") => respond.NoContent
          case _                                    => respond.BadRequest
        })
      def patchFoo(respond: Resource.PatchFooResponse.type)(path: String, query: String, form: String): Future[Resource.PatchFooResponse] =
        Future.successful((path, query, form) match {
          case ("1234", "2345", "3456")             => respond.NoContent
          case ("foo", "bar", "baz")                => respond.NoContent
          case ("\"qfoo\"", "\"qbar\"", "\"qbaz\"") => respond.NoContent
          case _                                    => respond.BadRequest
        })
      def putFoo(respond: Resource.PutFooResponse.type)(path: String, query: String, form: String): Future[Resource.PutFooResponse] =
        Future.successful((path, query, form) match {
          case ("1234", "2345", "3456")             => respond.NoContent
          case ("foo", "bar", "baz")                => respond.NoContent
          case ("\"qfoo\"", "\"qbar\"", "\"qbaz\"") => respond.NoContent
          case _                                    => respond.BadRequest
        })
    })

    describe("correctly navigate difficult JSON/string encodings") {
      describe("should handle url-encoded forms") {
        it("numbers") {
          Delete(Uri("/1234").withQuery(Uri.Query("query" -> "2345")), FormData("form" -> "3456")) ~> route ~> check {
            val _ = assert(status === StatusCodes.NoContent)
          }
        }

        it("strings") {
          Delete(Uri("/foo").withQuery(Uri.Query("query" -> "bar")), FormData("form" -> "baz")) ~> route ~> check {
            val _ = assert(status === StatusCodes.NoContent)
          }

          Delete(Uri("/\"qfoo\"").withQuery(Uri.Query("query" -> "\"qbar\"")), FormData("form" -> "\"qbaz\"")) ~> route ~> check {
            val _ = assert(status === StatusCodes.NoContent)
          }
        }
      }

      describe("should handle multipart forms") {
        it("numbers") {
          Put(Uri("/1234").withQuery(Uri.Query("query" -> "2345")), Multipart.FormData(Multipart.FormData.BodyPart("form", "3456"))) ~> route ~> check {
            val _ = assert(status === StatusCodes.NoContent)
          }
        }

        it("strings") {
          Put(Uri("/foo").withQuery(Uri.Query("query" -> "bar")), Multipart.FormData(Multipart.FormData.BodyPart("form", "baz"))) ~> route ~> check {
            val _ = assert(status === StatusCodes.NoContent)
          }

          Put(
            Uri("/\"qfoo\"").withQuery(Uri.Query("query" -> "\"qbar\"")),
            Multipart.FormData(Multipart.FormData.BodyPart("form", "\"qbaz\""))
          ) ~> route ~> check {
            val _ = assert(status === StatusCodes.NoContent)
          }
        }
      }

      describe("should handle text/plain") {
        it("numbers") {
          Patch(Uri("/1234").withQuery(Uri.Query("query" -> "2345")), HttpEntity(ContentTypes.`text/plain(UTF-8)`, "3456")) ~> route ~> check {
            val _ = assert(status === StatusCodes.NoContent)
          }
        }

        it("strings") {
          Patch(Uri("/foo").withQuery(Uri.Query("query" -> "bar")), HttpEntity(ContentTypes.`text/plain(UTF-8)`, "baz")) ~> route ~> check {
            val _ = assert(status === StatusCodes.NoContent)
          }

          Patch(Uri("/\"qfoo\"").withQuery(Uri.Query("query" -> "\"qbar\"")), HttpEntity(ContentTypes.`text/plain(UTF-8)`, "\"qbaz\"")) ~> route ~> check {
            val _ = assert(status === StatusCodes.NoContent)
          }
        }
      }
    }
  }

  describe("akka-http client should") {
    import issues.issue357.client.akkaHttp.{ Client, DeleteFooResponse, PatchFooResponse, PutFooResponse }

    object pathRE {
      def unapply(value: Uri.Path): Some[String] =
        value match {
          case Uri.Path.Slash(Uri.Path.Segment(head, tail)) => Some(head)
          case rest                                         => fail("Expected \"/{arg}\" did not match: " + rest.toString())
        }
    }

    def strictResponse[A](method: HttpMethod, extract: A => Option[String])(implicit um: FromEntityUnmarshaller[A]): HttpRequest => Future[HttpResponse] =
      response[A](
        method,
        x => Future.successful(extract(x))
      )

    def response[A](method: HttpMethod, extract: A => Future[Option[String]])(implicit um: FromEntityUnmarshaller[A]): HttpRequest => Future[HttpResponse] = {
      case HttpRequest(`method`, uri, headers, entity, protocol) =>
        for {
          entity <- Unmarshal(entity).to[A]
          res    <- extract(entity)
        } yield (uri.path, uri.query().get("query"), res) match {
          case (pathRE("1234"), Some("2345"), Some("3456"))             => HttpResponse(204)
          case (pathRE("foo"), Some("bar"), Some("baz"))                => HttpResponse(204)
          case (pathRE("\"qfoo\""), Some("\"qbar\""), Some("\"qbaz\"")) => HttpResponse(204)
          case _                                                        => HttpResponse(400)
        }
      case ex => failTest(s"Unknown: ${ex}")
    }

    describe("correctly navigate difficult JSON/string encodings") {
      describe("given url-encoded forms containing") {
        it("numbers") {
          Client
            .httpClient(strictResponse[FormData](HttpMethods.DELETE, _.fields.get("form")), "http://localhost:80")
            .deleteFoo("1234", "2345", "3456")
            .value
            .futureValue match {
            case Right(DeleteFooResponse.NoContent) =>
            case ex                                 => failTest(s"Unknown: ${ex}")
          }
        }

        it("strings") {
          Client
            .httpClient(strictResponse[FormData](HttpMethods.DELETE, _.fields.get("form")), "http://localhost:80")
            .deleteFoo("foo", "bar", "baz")
            .value
            .futureValue match {
            case Right(DeleteFooResponse.NoContent) =>
            case ex                                 => failTest(s"Unknown: ${ex}")
          }

          Client
            .httpClient(strictResponse[FormData](HttpMethods.DELETE, _.fields.get("form")), "http://localhost:80")
            .deleteFoo("\"qfoo\"", "\"qbar\"", "\"qbaz\"")
            .value
            .futureValue match {
            case Right(DeleteFooResponse.NoContent) =>
            case ex                                 => failTest(s"Unknown: ${ex}")
          }
        }
      }

      describe("given multipart forms containing") {
        it("numbers") {
          Client
            .httpClient(
              response[Multipart.FormData](
                HttpMethods.PUT,
                _.toStrict(Duration(5, SECONDS))
                  .map(_.strictParts.find(_.name == "form").map(_.entity.data.utf8String))
              ),
              "http://localhost:80"
            )
            .putFoo("1234", "2345", "3456")
            .value
            .futureValue match {
            case Right(PutFooResponse.NoContent) =>
            case ex                              => failTest(s"Unknown: ${ex}")
          }
        }

        it("strings") {
          Client
            .httpClient(
              response[Multipart.FormData](
                HttpMethods.PUT,
                _.toStrict(Duration(5, SECONDS))
                  .map(_.strictParts.find(_.name == "form").map(_.entity.data.utf8String))
              ),
              "http://localhost:80"
            )
            .putFoo("foo", "bar", "baz")
            .value
            .futureValue match {
            case Right(PutFooResponse.NoContent) =>
            case ex                              => failTest(s"Unknown: ${ex}")
          }

          Client
            .httpClient(
              response[Multipart.FormData](
                HttpMethods.PUT,
                _.toStrict(Duration(5, SECONDS))
                  .map(_.strictParts.find(_.name == "form").map(_.entity.data.utf8String))
              ),
              "http://localhost:80"
            )
            .putFoo("\"qfoo\"", "\"qbar\"", "\"qbaz\"")
            .value
            .futureValue match {
            case Right(PutFooResponse.NoContent) =>
            case ex                              => failTest(s"Unknown: ${ex}")
          }
        }
      }

      describe("given text/plain containing") {
        it("numbers") {
          Client
            .httpClient(
              strictResponse[String](
                HttpMethods.PATCH,
                Option.apply
              ),
              "http://localhost:80"
            )
            .patchFoo("1234", "2345", "3456")
            .value
            .futureValue match {
            case Right(PatchFooResponse.NoContent) =>
            case ex                                => failTest(s"Unknown: ${ex}")
          }
        }

        it("strings") {
          Client
            .httpClient(
              strictResponse[String](
                HttpMethods.PATCH,
                Option.apply
              ),
              "http://localhost:80"
            )
            .patchFoo("foo", "bar", "baz")
            .value
            .futureValue match {
            case Right(PatchFooResponse.NoContent) =>
            case ex                                => failTest(s"Unknown: ${ex}")
          }

          Client
            .httpClient(
              strictResponse[String](
                HttpMethods.PATCH,
                Option.apply
              ),
              "http://localhost:80"
            )
            .patchFoo("\"qfoo\"", "\"qbar\"", "\"qbaz\"")
            .value
            .futureValue match {
            case Right(PatchFooResponse.NoContent) =>
            case ex                                => failTest(s"Unknown: ${ex}")
          }
        }
      }
    }
  }
}
