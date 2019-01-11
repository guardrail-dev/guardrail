package core.issues

import _root_.issues.issue143.{ Handler, Resource }
import akka.http.scaladsl.model._
import akka.http.scaladsl.server._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.stream.scaladsl.Source
import cats.implicits._
import java.io.File
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{ EitherValues, FunSuite, Matchers }
import scala.concurrent.Future
import scala.concurrent.duration._

class Issue143 extends FunSuite with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest {
  override def testConfigSource =
    s"""
      |akka.loglevel = OFF
    """.stripMargin

  test("Ensure that failed uploads are cleaned up afterwards") {
    val tempDest = File.createTempFile("guardrail.", ".dat")
    val route = Resource.routes(new Handler {
      def uploadFile(respond: Resource.uploadFileResponse.type)(file: (File, Option[String], akka.http.scaladsl.model.ContentType)): Future[Resource.uploadFileResponse] =
        Future.successful(respond.Created)
      def uploadFileMapFileField(fieldName: String,fileName: Option[String],contentType: akka.http.scaladsl.model.ContentType): java.io.File =
        tempDest
    })

    val chunks = 1000
    val data = "foo"
    val contentLength = chunks * data.length
    val req = Post("/file").withEntity(Multipart.FormData(
        Multipart.FormData.BodyPart("file",
          HttpEntity(
            ContentTypes.`text/plain(UTF-8)`,
            contentLength,
            Source.fromIterator(() => List.fill(chunks)(akka.util.ByteString.fromString(data)).toIterator)
          )
        )
      ).toEntity.withSizeLimit(1001))

    req ~> route ~> check {
      status should equal(StatusCodes.RequestEntityTooLarge)
      tempDest.exists() should equal(false)
    }
  }
}
