package core.issues

import akka.http.scaladsl.model._
import akka.http.scaladsl.server._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.unmarshalling.Unmarshaller
import akka.util.ByteString
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.SpanSugar._
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.concurrent.{ ExecutionContext, Future }
import cats.implicits._
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import core.TestImplicits

class Issue325Suite extends AnyFunSuite with TestImplicits with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest {
  override implicit val patienceConfig: PatienceConfig = PatienceConfig(10 seconds, 1 second)

  def multipartChunk(key: String, value: String, contentType: ContentType): Multipart.FormData.BodyPart.Strict =
    Multipart.FormData.BodyPart.Strict(key, HttpEntity.Strict(contentType, ByteString.fromArray(value.getBytes)))

  test("Ensure that servers can be constructed") {
    import issues.issue325.server.akkaHttpJackson.{ Handler, Resource }
    val route = Resource.routes(new Handler {
      override def testMultipleContentTypes(
          respond: Resource.TestMultipleContentTypesResponse.type
      )(foo: String, bar: Int, baz: Option[Int], file: Option[(java.io.File, Option[String], ContentType)]): Future[Resource.TestMultipleContentTypesResponse] =
        Future.successful(
          if (foo == foo && bar == 5 && baz.forall(_ == 10)) {
            respond.OK
          } else {
            respond.InternalServerError
          }
        )
      override def testMultipleContentTypesMapFileField(fieldName: String, fileName: Option[String], contentType: ContentType): java.io.File =
        java.io.File.createTempFile("guardrail-issue-325", "dat")
      override def emptyConsumes(
          respond: Resource.EmptyConsumesResponse.type
      )(foo: String): Future[Resource.EmptyConsumesResponse] =
        Future.successful(respond.OK)
    })

    Post("/test") ~> route ~> check {
      rejection match {
        case MissingFormFieldRejection("foo") => ()
      }
    }

    Post("/test")
      .withEntity(ContentType.apply(MediaTypes.`application/x-www-form-urlencoded`, () => HttpCharsets.`UTF-8`), "".getBytes) ~> route ~> check {
      rejection match {
        case MissingFormFieldRejection("foo") => ()
      }
    }

    Post("/test")
      .withEntity(ContentType.apply(MediaTypes.`application/x-www-form-urlencoded`, () => HttpCharsets.`UTF-8`), "foo=foo&bar=5".getBytes) ~> route ~> check {
      status should equal(StatusCodes.OK)
    }

    // Test that text/plain can be handled
    Post("/test")
      .withEntity(
        Multipart
          .FormData(
            multipartChunk("foo", "foo", ContentTypes.`text/plain(UTF-8)`),
            multipartChunk("bar", "5", ContentTypes.`text/plain(UTF-8)`)
          )
          .toEntity
      ) ~> route ~> check {
      status should equal(StatusCodes.OK)
    }

    // Test that mixed text/plain and application/json can be handled
    Post("/test")
      .withEntity(
        Multipart
          .FormData(
            multipartChunk("foo", "\"foo\"", ContentTypes.`application/json`),
            multipartChunk("bar", "5", ContentTypes.`text/plain(UTF-8)`)
          )
          .toEntity
      ) ~> route ~> check {
      status should equal(StatusCodes.OK)
    }

    // Test that application/json can be handled
    Post("/test")
      .withEntity(
        Multipart
          .FormData(
            multipartChunk("foo", "\"foo\"", ContentTypes.`application/json`),
            multipartChunk("bar", "5", ContentTypes.`application/json`)
          )
          .toEntity
      ) ~> route ~> check {
      status should equal(StatusCodes.OK)
    }

    Put("/test")
      .withEntity(ContentType.apply(MediaTypes.`application/x-www-form-urlencoded`, () => HttpCharsets.`UTF-8`), "foo=foo".getBytes) ~> route ~> check {
      rejection match {
        case UnsupportedRequestContentTypeRejection(supportedContentTypes) =>
          supportedContentTypes.size should equal(1)
          supportedContentTypes.head.matches(ContentType(MediaTypes.`multipart/form-data`)) should equal(true)
      }
    }

    Put("/test")
      .withEntity(
        Multipart
          .FormData(
            multipartChunk("foo", "foo", ContentTypes.`text/plain(UTF-8)`)
          )
          .toEntity
      ) ~> route ~> check {
      status should equal(StatusCodes.OK)
    }
  }

  test("Ensure that clients supply the correct arguments encoded in the expected way") {
    import akka.stream.scaladsl.Sink
    import issues.issue325.client.akkaHttpJackson.{ Client, TestMultipleContentTypesResponse }

    def expectResponse(p: (String, List[Multipart.FormData.BodyPart.Strict]) => Boolean)(implicit ec: ExecutionContext): HttpRequest => Future[HttpResponse] = {
      req =>
        import scala.concurrent.duration._
        for {
          sreq     <- req.toStrict(Duration(5, SECONDS))
          chunks   <- Unmarshaller.multipartFormDataUnmarshaller.apply(sreq.entity)
          elements <- chunks.parts.runFold(List.empty[Multipart.FormData.BodyPart])(_ :+ _)
          res <- elements.groupBy(_.name).toList.traverse { case (name, chunks) =>
            for {
              chunks <- chunks.traverse[Future, Multipart.FormData.BodyPart.Strict](_.toStrict(Duration(5, SECONDS)))
            } yield p(name, chunks)
          }
        } yield
          if (res.forall(_ == true)) {
            HttpResponse(200)
          } else {
            HttpResponse(500)
          }
    }

    Client
      .httpClient(
        expectResponse {
          case ("foo", chunks) if chunks.length == 1 && chunks.forall(_.entity.data.utf8String == "foo") => true
          case ("bar", chunks) if chunks.length == 1 && chunks.forall(_.entity.data.utf8String == "5")   => true
          case _                                                                                         => false
        },
        "http://localhost:80"
      )
      .testMultipleContentTypes("foo", 5)
      .value
      .futureValue match {
      case Right(TestMultipleContentTypesResponse.OK) => ()
      case ex                                         => failTest(s"Unknown: ${ex}")
    }
  }
}
