package swagger

import org.scalatest.{EitherValues, FunSuite, Matchers}
import org.scalatest.concurrent.ScalaFutures
import clients.Implicits
import _root_.clients.pet.PetClient
import _root_.clients.{definitions => cdefs}
import _root_.servers.pet.{PetHandler, PetResource}
import _root_.servers.{definitions => sdefs}
import akka.http.scaladsl.model._
import akka.http.scaladsl.server._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import scala.concurrent.Future

class RoundTripTest extends FunSuite with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest {
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
      def addPet(body: sdefs.Pet): Future[Unit] = body match {
				case sdefs.Pet(
						`id`,
						Some(sdefs.Category(`categoryId`, `categoryName`)),
						`name`,
						`photoUrls`,
						None,
						Some(sdefs.PetStatus.Pending)
					) => Future.successful(())
				case _ => failTest("Parameters didn't match")
			}

      def deletePet(petId: Long, apiKey: Option[String] = None): Future[Unit] = ???
      def findPetsByStatus(status: Iterable[String]): Future[IndexedSeq[sdefs.Pet]] = ???
      def findPetsByStatusEnum(_status: sdefs.PetStatus): Future[IndexedSeq[sdefs.Pet]] = ???
      def findPetsByTags(tags: Iterable[String]): Future[IndexedSeq[sdefs.Pet]] = ???
      def getPetById(petId: Long): Future[sdefs.Pet] = ???
      def updatePet(body: sdefs.Pet): Future[Unit] = ???
      def updatePetWithForm(petId: Long, name: Option[String] = None, status: Option[String] = None): Future[Unit] = ???
      def uploadFile(petId: Long, additionalMetadata: Option[String] = None, file: Option[String] = None): Future[sdefs.ApiResponse] = ???
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
      def findPetsByStatusEnum(_status: sdefs.PetStatus): Future[IndexedSeq[sdefs.Pet]] = {
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

      def addPet(body: sdefs.Pet): Future[Unit] = ???
      def deletePet(petId: Long, apiKey: Option[String] = None): Future[Unit] = ???
      def findPetsByStatus(status: Iterable[String]): Future[IndexedSeq[sdefs.Pet]] = ???
      def findPetsByTags(tags: Iterable[String]): Future[IndexedSeq[sdefs.Pet]] = ???
      def getPetById(petId: Long): Future[sdefs.Pet] = ???
      def updatePet(body: sdefs.Pet): Future[Unit] = ???
      def updatePetWithForm(petId: Long, name: Option[String] = None, status: Option[String] = None): Future[Unit] = ???
      def uploadFile(petId: Long, additionalMetadata: Option[String] = None, file: Option[String] = None): Future[sdefs.ApiResponse] = ???
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
}
