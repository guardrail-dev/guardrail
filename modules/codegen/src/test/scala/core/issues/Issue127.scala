package tests.core.issues

import cats.instances.all._
import com.twilio.swagger._
import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.{ Context, Server, Servers }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner

import scala.meta._

class Issue127 extends FunSuite with Matchers with SwaggerSpecRunner {
  val swagger: String = s"""
    |swagger: '2.0'
    |host: localhost:1234
    |schemes:
    |- http
    |paths:
    |  /file:
    |    post:
    |      operationId: uploadFile
    |      parameters:
    |      - name: file
    |        description: File to upload
    |        in: formData
    |        type: file
    |        required: true
    |      consumes:
    |      - multipart/form-data
    |      responses:
    |        '201':
    |          description: Success
    |""".stripMargin

  test("Generate plain array alias definition") {
    val (
      _,
      _,
      Servers(Server(pkg, extraImports, genHandler :: genResource :: Nil) :: Nil)
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)

    val handler = q"""
      trait Handler {
        def uploadFile(respond: Resource.uploadFileResponse.type)(file: (File, Option[String], ContentType)): scala.concurrent.Future[Resource.uploadFileResponse]
        def uploadFileMapFileField(fieldName: String, fileName: Option[String], contentType: ContentType): File
        def uploadFileUnmarshalToFile[F[_]: Functor](hashType: F[String], destFn: (String, Option[String], ContentType) => File)(implicit mat: Materializer): Unmarshaller[Multipart.FormData.BodyPart, (File, Option[String], ContentType, F[String])] = Unmarshaller { implicit executionContext =>
          part => {
            val dest = destFn(part.name, part.filename, part.entity.contentType)
            val messageDigest = hashType.map(MessageDigest.getInstance(_))
            val fileSink: Sink[ByteString, Future[IOResult]] = FileIO.toPath(dest.toPath).contramap[ByteString] { chunk =>
              val _ = messageDigest.map(_.update(chunk.toArray[Byte]))
              chunk
            }
            part.entity.dataBytes.toMat(fileSink)(Keep.right).run().transform({
              case IOResult(_, Success(_)) =>
                val hash = messageDigest.map(md => javax.xml.bind.DatatypeConverter.printHexBinary(md.digest()).toLowerCase(java.util.Locale.US))
                (dest, part.filename, part.entity.contentType, hash)
              case IOResult(_, Failure(t)) =>
                dest.delete()
                throw t
            }, {
              case t =>
                dest.delete()
                t
            })
          }
        }
      }
    """

    val resource = q"""
      object Resource {
        def discardEntity(implicit mat: akka.stream.Materializer): Directive0 = extractRequest.flatMap { req =>
          req.discardEntityBytes().future
          Directive.Empty
        }
        def routes(handler: Handler)(implicit mat: akka.stream.Materializer): Route = {
          (post & path("file") & ({
            object uploadFileParts {
              sealed trait Part
              case class IgnoredPart(unit: Unit) extends Part
              case class file(value: (File, Option[String], ContentType)) extends Part
            }
            val UnmarshalfilePart: Unmarshaller[Multipart.FormData.BodyPart, uploadFileParts.file] = handler.uploadFileUnmarshalToFile[Option](None, handler.uploadFileMapFileField(_, _, _)).map({
              case (v1, v2, v3, v4) =>
                uploadFileParts.file((v1, v2, v3))
            })
            extractExecutionContext.flatMap { implicit executionContext =>
              extractMaterializer.flatMap { implicit mat =>
                val fileReferences = new AtomicReference(List.empty[File])
                (extractSettings.flatMap {
                  settings => handleExceptions(ExceptionHandler({
                    case EntityStreamSizeException(limit, contentLength) =>
                      fileReferences.get().foreach(_.delete())
                      val summary = contentLength match {
                        case Some(cl) =>
                          s"Request Content-Length of $$cl bytes exceeds the configured limit of $$limit bytes"
                        case None =>
                          s"Aggregated data length of request entity exceeds the configured limit of $$limit bytes"
                      }
                      val info = new ErrorInfo(summary, "Consider increasing the value of akka.http.server.parsing.max-content-length")
                      val status = StatusCodes.RequestEntityTooLarge
                      val msg = if (settings.verboseErrorMessages) info.formatPretty else info.summary
                      complete(HttpResponse(status, entity = msg))
                    case e: Throwable =>
                      fileReferences.get().foreach(_.delete())
                      throw e
                  }))
                } & handleRejections { (rejections: scala.collection.immutable.Seq[Rejection]) =>
                  fileReferences.get().foreach(_.delete())
                  None
                } & mapResponse { resp =>
                  fileReferences.get().foreach(_.delete())
                  resp
                } & entity(as[Multipart.FormData])).flatMap { formData =>
                  val collectedPartsF: Future[Either[Throwable, Tuple1[Option[(File, Option[String], ContentType)]]]] = for (results <- formData.parts.mapConcat {
                    part => if (Set[String]("file").contains(part.name)) part :: Nil else {
                      part.entity.discardBytes()
                      Nil
                    }
                  }.mapAsync(1) { part =>
                    {
                      part.name match {
                        case "file" =>
                          SafeUnmarshaller(AccumulatingUnmarshaller(fileReferences, UnmarshalfilePart)(_.value._1)).apply(part)
                        case _ =>
                          SafeUnmarshaller(implicitly[Unmarshaller[Multipart.FormData.BodyPart, Unit]].map(uploadFileParts.IgnoredPart.apply(_))).apply(part)
                      }
                    }
                  }.toMat(Sink.seq[Either[Throwable, uploadFileParts.Part]])(Keep.right).run()) yield {
                    results.toList.sequence.map { successes =>
                      val fileO = successes.collectFirst({
                        case uploadFileParts.file((v1, v2, v3)) =>
                          (v1, v2, v3)
                      })
                      Tuple1(fileO)
                    }
                  }
                  onSuccess(collectedPartsF)
                }
              }
            }.flatMap(_.fold(t => throw t, {
              case Tuple1(fileO) =>
                val maybe: Either[Rejection, Tuple1[(File, Option[String], ContentType)]] = for (file <- fileO.toRight(MissingFormFieldRejection("file"))) yield {
                  Tuple1(file)
                }
                maybe.fold(reject(_), tprovide(_))
            }))
          }: Directive[Tuple1[(File, Option[String], ContentType)]])) {
            file => complete(handler.uploadFile(uploadFileResponse)(file))
          }
        }
        sealed abstract class uploadFileResponse(val statusCode: StatusCode)
        case object uploadFileResponseCreated extends uploadFileResponse(StatusCodes.Created)
        object uploadFileResponse {
          implicit val uploadFileTRM: ToResponseMarshaller[uploadFileResponse] = Marshaller { implicit ec =>
            resp => uploadFileTR(resp)
          }
          implicit def uploadFileTR(value: uploadFileResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r: uploadFileResponseCreated.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => uploadFileResponse): uploadFileResponse = ev(value)
          def Created: uploadFileResponse = uploadFileResponseCreated
        }
      }
    """

    genHandler.structure shouldBe handler.structure
    genResource.structure shouldBe resource.structure
  }
}
