package swagger

import _root_.clients.pet.PetClient
import _root_.clients.{definitions => cdefs}
import _root_.servers.pet.{PetHandler, PetResource}
import _root_.servers.{definitions => sdefs}
import akka.http.scaladsl.model._
import akka.http.scaladsl.server._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import clients.Implicits
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.SpanSugar._
import org.scalatest.{EitherValues, FunSuite, Matchers}
import scala.concurrent.Future

class RoundTripTest extends FunSuite with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest {
  override implicit val patienceConfig = PatienceConfig(1000 millis, 1000 millis)

  def traceBuilder[T]: clients.Implicits.TraceBuilder[Either[Throwable,HttpResponse],T] = {
    name => inner => inner(identity _)
  }

  // Placeholder until property testing
  val id: Option[Long] = None
  val categoryId: Option[Long] = None
  val categoryName: Option[String] = None
  val name: String = ""
  val photoUrls: IndexedSeq[String] = IndexedSeq.empty
  val tag1id: Option[Long] = None
  val tag1name: Option[String] = None
  val tag2id: Option[Long] = None
  val tag2name: Option[String] = None
  val tag3id: Option[Long] = None
  val tag3name: Option[String] = None
  val petStatus: Option[String] = Some("pending")

  test("round-trip: definition query, unit response") {
    val httpClient = Route.asyncHandler(PetResource.routes(new PetHandler {
      def addPet(respond: PetResource.addPetResponse.type)(body: sdefs.Pet): Future[PetResource.addPetResponse] = body match {
        case sdefs.Pet(
            `id`,
            Some(sdefs.Category(`categoryId`, `categoryName`)),
            `name`,
            `photoUrls`,
            None,
            Some(sdefs.PetStatus.Pending)
          ) => Future.successful(respond.Created)
        case _ => failTest("Parameters didn't match")
      }
      def deletePet(respond: PetResource.deletePetResponse.type)(petId: Long, apiKey: Option[String] = None) = ???
      def findPetsByStatus(respond: PetResource.findPetsByStatusResponse.type)(status: Iterable[String]) = ???
      def findPetsByStatusEnum(respond: PetResource.findPetsByStatusEnumResponse.type)(status: sdefs.PetStatus) = ???
      def findPetsByTags(respond: PetResource.findPetsByTagsResponse.type)(tags: Iterable[String]) = ???
      def getPetById(respond: PetResource.getPetByIdResponse.type)(petId: Long) = ???
      def updatePet(respond: PetResource.updatePetResponse.type)(body: sdefs.Pet) = ???
      def updatePetWithForm(respond: PetResource.updatePetWithFormResponse.type)(petId: Long, name: Option[String] = None, status: Option[String] = None) = ???
      def uploadFile(respond: PetResource.uploadFileResponse.type)(petId: Long, additionalMetadata: Option[String] = None, file: Option[String] = None) = ???
    }))

    val petClient = PetClient.httpClient(httpClient)

    petClient.addPet(traceBuilder, cdefs.Pet(
      id=id,
      category=Some(cdefs.Category(categoryId, categoryName)),
      name=name,
      photoUrls=photoUrls,
      tags=None,
      status=Some(cdefs.PetStatus.Pending)
    )).value.futureValue should be(Right(clients.Implicits.IgnoredEntity.empty))
  }

  test("round-trip: enum query, Vector of definition response") {
    val httpClient = Route.asyncHandler(PetResource.routes(new PetHandler {
      def findPetsByStatusEnum(respond: PetResource.findPetsByStatusEnumResponse.type)(_status: sdefs.PetStatus): Future[PetResource.findPetsByStatusEnumResponse] = {
        Future.successful(petStatus.fold(IndexedSeq.empty[sdefs.Pet])({ value =>
          if (_status.value == value) {
            IndexedSeq(sdefs.Pet(
              id=id,
              category=Some(sdefs.Category(categoryId, categoryName)),
              name=name,
              photoUrls=photoUrls,
              tags=None,
              status=sdefs.PetStatus.parse(value)
            ))
          } else {
            failTest("Parameters didn't match!")
          }
        }))
      }

      def addPet(respond: PetResource.addPetResponse.type)(body: sdefs.Pet) = ???
      def deletePet(respond: PetResource.deletePetResponse.type)(petId: Long, apiKey: Option[String] = None) = ???
      def findPetsByStatus(respond: PetResource.findPetsByStatusResponse.type)(status: Iterable[String]) = ???
      def findPetsByTags(respond: PetResource.findPetsByTagsResponse.type)(tags: Iterable[String]) = ???
      def getPetById(respond: PetResource.getPetByIdResponse.type)(petId: Long) = ???
      def updatePet(respond: PetResource.updatePetResponse.type)(body: sdefs.Pet) = ???
      def updatePetWithForm(respond: PetResource.updatePetWithFormResponse.type)(petId: Long, name: Option[String] = None, status: Option[String] = None) = ???
      def uploadFile(respond: PetResource.uploadFileResponse.type)(petId: Long, additionalMetadata: Option[String] = None, file: Option[String] = None) = ???
    }))

    val petClient = PetClient.httpClient(httpClient)

    petClient.findPetsByStatusEnum(traceBuilder, cdefs.PetStatus.Pending).value.futureValue should be(Right(Vector(cdefs.Pet(
      id=id,
      category=Some(cdefs.Category(categoryId, categoryName)),
      name=name,
      photoUrls=photoUrls,
      tags=None,
      status=Some(cdefs.PetStatus.Pending)
    ))))
  }

  test("round-trip: 404 response") {
    val httpClient = Route.asyncHandler(PetResource.routes(new PetHandler {
      def findPetsByStatus(respond: PetResource.findPetsByStatusResponse.type)(status: Iterable[String]): Future[PetResource.findPetsByStatusResponse] = {
        Future.successful(respond.NotFound)
      }

      def addPet(respond: PetResource.addPetResponse.type)(body: sdefs.Pet) = ???
      def deletePet(respond: PetResource.deletePetResponse.type)(petId: Long, apiKey: Option[String] = None) = ???
      def findPetsByStatusEnum(respond: PetResource.findPetsByStatusEnumResponse.type)(status: sdefs.PetStatus) = ???
      def findPetsByTags(respond: PetResource.findPetsByTagsResponse.type)(tags: Iterable[String]) = ???
      def getPetById(respond: PetResource.getPetByIdResponse.type)(petId: Long) = ???
      def updatePet(respond: PetResource.updatePetResponse.type)(body: sdefs.Pet) = ???
      def updatePetWithForm(respond: PetResource.updatePetWithFormResponse.type)(petId: Long, name: Option[String] = None, status: Option[String] = None) = ???
      def uploadFile(respond: PetResource.uploadFileResponse.type)(petId: Long, additionalMetadata: Option[String] = None, file: Option[String] = None) = ???
    }))

    val petClient = PetClient.httpClient(httpClient)

    val result = petClient.findPetsByStatus(traceBuilder, Vector("bogus")).value.futureValue
    assert(result.left.value.right.value.status.intValue == 404)
  }
}
