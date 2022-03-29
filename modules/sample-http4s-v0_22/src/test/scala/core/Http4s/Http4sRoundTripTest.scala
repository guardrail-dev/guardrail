package core.Http4s

import _root_.examples.client.http4sV022.pet.PetClient
import _root_.examples.client.{ http4sV022 => cdefs }
import _root_.examples.server.http4sV022.pet.PetResource._
import _root_.examples.server.http4sV022.pet._
import _root_.examples.server.{ http4sV022 => sdefs }
import cats.data._
import cats.effect.IO
import cats.effect.IO._
import examples.support.PositiveLong
import fs2.Stream
import org.http4s.Request
import org.http4s.Response
import org.http4s.client.Client
import org.http4s.implicits._
import org.scalatest.EitherValues
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.security.MessageDigest
import java.util.Locale.US
import javax.xml.bind.DatatypeConverter.printHexBinary

class Http4sRoundTripTest extends AnyFunSuite with Matchers with EitherValues {

  // Placeholder until property testing
  val id: Option[Long]             = None
  val categoryId: Option[Long]     = None
  val categoryName: Option[String] = None
  val name: String                 = ""
  val photoUrls: Vector[String]    = Vector.empty
  val tag1id: Option[Long]         = None
  val tag1name: Option[String]     = None
  val tag2id: Option[Long]         = None
  val tag2name: Option[String]     = None
  val tag3id: Option[Long]         = None
  val tag3name: Option[String]     = None
  val petStatus: Option[String]    = Some("pending")

  test("round-trip: definition query, unit response") {
    val httpService = new PetResource().routes(new PetHandler[IO] {
      def addPet(respond: AddPetResponse.type)(body: sdefs.definitions.Pet): IO[sdefs.pet.PetResource.AddPetResponse] =
        body match {
          case sdefs.definitions.Pet(
              `id`,
              Some(sdefs.definitions.Category(`categoryId`, `categoryName`)),
              `name`,
              `photoUrls`,
              None,
              Some(sdefs.definitions.PetStatus.Pending)
              ) =>
            IO.pure(respond.Created)
          case _ => throw new TestFailedException("Parameters didn't match", 11)
        }
      def deletePet(
          respond: DeletePetResponse.type
      )(_petId: Long, includeChildren: Option[Boolean], status: Option[sdefs.definitions.PetStatus], apiKey: Option[String]) = ???
      def findPetsByStatus(respond: FindPetsByStatusResponse.type)(status: Iterable[String])                                 = ???
      def findPetsByStatusEnum(respond: FindPetsByStatusEnumResponse.type)(status: sdefs.definitions.PetStatus)              = ???
      def findPetsByTags(respond: FindPetsByTagsResponse.type)(tags: Iterable[String])                                       = ???
      def getPetById(respond: GetPetByIdResponse.type)(petId: Long)                                                          = ???
      def updatePet(respond: UpdatePetResponse.type)(body: sdefs.definitions.Pet)                                            = ???
      def updatePetWithForm(
          respond: UpdatePetWithFormResponse.type
      )(petId: Long, name: Option[String] = None, status: Option[String] = None) =
        ???
      def uploadFile(respond: UploadFileResponse.type)(
          petId: PositiveLong,
          additionalMetadata: Option[String] = None,
          file: Option[fs2.Stream[IO, Byte]] = None,
          file2: fs2.Stream[IO, Byte],
          file3: fs2.Stream[IO, Byte],
          longValue: Long,
          customValue: PositiveLong,
          customOptionalValue: Option[PositiveLong] = None
      ) = ???
    })

    val petClient = PetClient.httpClient(Client.fromHttpApp(httpService.orNotFound))

    petClient
      .addPet(
        cdefs.definitions.Pet(
          id = id,
          category = Some(cdefs.definitions.Category(categoryId, categoryName)),
          name = name,
          photoUrls = photoUrls,
          tags = None,
          status = Some(cdefs.definitions.PetStatus.Pending)
        )
      )
      .attempt
      .unsafeRunSync() should be(Right(cdefs.pet.AddPetResponse.Created))
  }

  test("round-trip: enum query, Vector of definition response") {
    val httpService = new PetResource().routes(new PetHandler[IO] {
      def findPetsByStatusEnum(
          respond: FindPetsByStatusEnumResponse.type
      )(_status: sdefs.definitions.PetStatus): IO[sdefs.pet.PetResource.FindPetsByStatusEnumResponse] =
        IO.pure(petStatus.fold(Vector.empty[sdefs.definitions.Pet])({ value =>
            if (_status.value == value) {
              Vector(
                sdefs.definitions.Pet(
                  id = id,
                  category = Some(sdefs.definitions.Category(categoryId, categoryName)),
                  name = name,
                  photoUrls = photoUrls,
                  tags = None,
                  status = sdefs.definitions.PetStatus.from(value)
                )
              )
            } else {
              throw new TestFailedException("Parameters didn't match", 11)
            }
          }))
          .map(respond.Ok)

      def addPet(respond: AddPetResponse.type)(body: sdefs.definitions.Pet) = ???
      def deletePet(
          respond: DeletePetResponse.type
      )(_petId: Long, includeChildren: Option[Boolean], status: Option[sdefs.definitions.PetStatus], apiKey: Option[String]) = ???
      def findPetsByStatus(respond: FindPetsByStatusResponse.type)(status: Iterable[String])                                 = ???
      def findPetsByTags(respond: FindPetsByTagsResponse.type)(tags: Iterable[String])                                       = ???
      def getPetById(respond: GetPetByIdResponse.type)(petId: Long)                                                          = ???
      def updatePet(respond: UpdatePetResponse.type)(body: sdefs.definitions.Pet)                                            = ???
      def updatePetWithForm(
          respond: UpdatePetWithFormResponse.type
      )(petId: Long, name: Option[String] = None, status: Option[String] = None) =
        ???
      def uploadFile(respond: UploadFileResponse.type)(
          petId: PositiveLong,
          additionalMetadata: Option[String] = None,
          file: Option[fs2.Stream[IO, Byte]] = None,
          file2: fs2.Stream[IO, Byte],
          file3: fs2.Stream[IO, Byte],
          longValue: Long,
          customValue: PositiveLong,
          customOptionalValue: Option[PositiveLong] = None
      ) = ???
    })

    val petClient = PetClient.httpClient(Client.fromHttpApp(httpService.orNotFound))

    petClient.findPetsByStatusEnum(cdefs.definitions.PetStatus.Pending).attempt.unsafeRunSync() should be(
      Right(
        cdefs.pet.FindPetsByStatusEnumResponse.Ok(
          Vector(
            cdefs.definitions.Pet(
              id = id,
              category = Some(cdefs.definitions.Category(categoryId, categoryName)),
              name = name,
              photoUrls = photoUrls,
              tags = None,
              status = Some(cdefs.definitions.PetStatus.Pending)
            )
          )
        )
      )
    )
  }

  test("round-trip: 404 response") {
    val httpService = new PetResource().routes(new PetHandler[IO] {
      def findPetsByStatus(
          respond: FindPetsByStatusResponse.type
      )(status: Iterable[String]): IO[sdefs.pet.PetResource.FindPetsByStatusResponse] =
        IO.pure(respond.NotFound)

      def addPet(respond: AddPetResponse.type)(body: sdefs.definitions.Pet) = ???
      def deletePet(
          respond: DeletePetResponse.type
      )(_petId: Long, includeChildren: Option[Boolean], status: Option[sdefs.definitions.PetStatus], apiKey: Option[String]) = ???
      def findPetsByStatusEnum(respond: FindPetsByStatusEnumResponse.type)(status: sdefs.definitions.PetStatus)              = ???
      def findPetsByTags(respond: FindPetsByTagsResponse.type)(tags: Iterable[String])                                       = ???
      def getPetById(respond: GetPetByIdResponse.type)(petId: Long)                                                          = ???
      def updatePet(respond: UpdatePetResponse.type)(body: sdefs.definitions.Pet)                                            = ???
      def updatePetWithForm(
          respond: UpdatePetWithFormResponse.type
      )(petId: Long, name: Option[String] = None, status: Option[String] = None) =
        ???
      def uploadFile(respond: UploadFileResponse.type)(
          petId: PositiveLong,
          additionalMetadata: Option[String] = None,
          file: Option[fs2.Stream[IO, Byte]] = None,
          file2: fs2.Stream[IO, Byte],
          file3: fs2.Stream[IO, Byte],
          longValue: Long,
          customValue: PositiveLong,
          customOptionalValue: Option[PositiveLong] = None
      ) = ???
    })

    val petClient = PetClient.httpClient(Client.fromHttpApp(httpService.orNotFound))

    petClient.findPetsByStatus(Vector("bogus")).attempt.unsafeRunSync() should be(Right(cdefs.pet.FindPetsByStatusResponse.NotFound))
  }

  test("round-trip: Raw type parameters") {
    val petId: Long    = 123L
    val apiKey: String = "foobar"
    val httpService = new PetResource().routes(new PetHandler[IO] {
      def deletePet(respond: DeletePetResponse.type)(
          _petId: Long,
          includeChildren: Option[Boolean],
          status: Option[sdefs.definitions.PetStatus],
          _apiKey: Option[String] = None
      ): IO[sdefs.pet.PetResource.DeletePetResponse] =
        if (_petId == petId && _apiKey.contains(apiKey) && status.contains(sdefs.definitions.PetStatus.Pending))
          IO.pure(respond.Ok)
        else IO.pure(respond.NotFound)

      def addPet(respond: AddPetResponse.type)(body: sdefs.definitions.Pet)                                     = ???
      def findPetsByStatus(respond: FindPetsByStatusResponse.type)(status: Iterable[String])                    = ???
      def findPetsByStatusEnum(respond: FindPetsByStatusEnumResponse.type)(status: sdefs.definitions.PetStatus) = ???
      def findPetsByTags(respond: FindPetsByTagsResponse.type)(tags: Iterable[String])                          = ???
      def getPetById(respond: GetPetByIdResponse.type)(petId: Long)                                             = ???
      def updatePet(respond: UpdatePetResponse.type)(body: sdefs.definitions.Pet)                               = ???
      def updatePetWithForm(
          respond: UpdatePetWithFormResponse.type
      )(petId: Long, name: Option[String] = None, status: Option[String] = None) =
        ???
      def uploadFile(respond: UploadFileResponse.type)(
          petId: PositiveLong,
          additionalMetadata: Option[String] = None,
          file: Option[fs2.Stream[IO, Byte]] = None,
          file2: fs2.Stream[IO, Byte],
          file3: fs2.Stream[IO, Byte],
          longValue: Long,
          customValue: PositiveLong,
          customOptionalValue: Option[PositiveLong] = None
      ) = ???
    })

    val petClient = PetClient.httpClient(Client.fromHttpApp(httpService.orNotFound))

    val result = petClient.deletePet(petId, Some(true), Some(cdefs.definitions.PetStatus.Pending), Some(apiKey)).attempt.unsafeRunSync()
    result.left
      .foreach({ err =>
        throw new TestFailedException(err.toString, 11)
      })
  }

  test("round-trip: File uploads") {
    val petId: Long    = 123L
    val apiKey: String = "foobar"
    val httpService = new PetResource().routes(new PetHandler[IO] {
      def addPet(respond: AddPetResponse.type)(body: sdefs.definitions.Pet) = ???
      def deletePet(
          respond: DeletePetResponse.type
      )(_petId: Long, includeChildren: Option[Boolean], status: Option[sdefs.definitions.PetStatus], apiKey: Option[String]) = ???
      def findPetsByStatus(respond: FindPetsByStatusResponse.type)(status: Iterable[String])                                 = ???
      def findPetsByStatusEnum(respond: FindPetsByStatusEnumResponse.type)(status: sdefs.definitions.PetStatus)              = ???
      def findPetsByTags(respond: FindPetsByTagsResponse.type)(tags: Iterable[String])                                       = ???
      def getPetById(respond: GetPetByIdResponse.type)(petId: Long)                                                          = ???
      def updatePet(respond: UpdatePetResponse.type)(body: sdefs.definitions.Pet)                                            = ???
      def updatePetWithForm(
          respond: UpdatePetWithFormResponse.type
      )(petId: Long, name: Option[String] = None, status: Option[String] = None) =
        ???
      def uploadFile(respond: UploadFileResponse.type)(
          petId: PositiveLong,
          additionalMetadata: Option[String] = None,
          file: Option[fs2.Stream[IO, Byte]] = None,
          file2: fs2.Stream[IO, Byte],
          file3: fs2.Stream[IO, Byte],
          longValue: Long,
          customValue: PositiveLong,
          customOptionalValue: Option[PositiveLong] = None
      ) =
        for {
          f1Content <- file.fold(IO.pure(Vector.empty[Byte]))(_.compile.toVector)
          f1Length = if (f1Content.nonEmpty) Some(f1Content.length) else None
          f2Content <- file2.compile.toVector
          f2Length = if (f2Content.nonEmpty) {
            Some(f2Content.length)
          } else None
          f3Content <- file3.compile.toVector
          f3Length = if (f3Content.nonEmpty) {
            Some(f3Content.length)
          } else None

          hash = printHexBinary(MessageDigest.getInstance("SHA-256").digest(f3Content.toArray)).toLowerCase(US)
          _    = assert(hash == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", "Empty file hash does not match")

          code = f1Length.count(_ > 0) + (if (f2Content.nonEmpty) 1 else 0) + (if (f3Content.nonEmpty) 1 else 0)

        } yield respond.Ok(sdefs.definitions.ApiResponse(code = Some(code), message = Some(s"${f1Length} ${f2Length} ${f3Length}")))
    })

    val petClient = PetClient.httpClient(Client.fromHttpApp(httpService.orNotFound))

    val result = petClient
      .uploadFile(
        PositiveLong(petId).get,
        Some("Additional metadata"),
        None,
        ("file2", Stream.empty),
        ("file3", Stream.empty),
        5L,
        PositiveLong(10L).get,
        PositiveLong(20L)
      )
      .attempt
      .unsafeRunSync
    result
      .fold(
        { err =>
          throw new TestFailedException(err.toString, 11)
        }, {
          case cdefs.pet.UploadFileResponse.Ok(value) => assert(value.code.contains(0), "Unexpected number of file uploads!")
        }
      )
  }
}
