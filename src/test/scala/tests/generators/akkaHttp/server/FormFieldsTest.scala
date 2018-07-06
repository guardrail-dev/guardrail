package tests.generators.akkaHttp.server

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.{ Context, Server, Servers }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import com.twilio.guardrail.tests._

class FormFieldsServerTest extends FunSuite with Matchers with SwaggerSpecRunner {
  import scala.meta._

  val swagger: String = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |paths:
    |  /foo:
    |    put:
    |      operationId: putFoo
    |      consumes:
    |        - multipart/form-data
    |      parameters:
    |        - name: foo
    |          in: formData
    |          type: string
    |          required: true
    |        - name: bar
    |          in: formData
    |          type: integer
    |          format: int64
    |          required: true
    |        - name: baz
    |          in: formData
    |          type: file
    |          required: true
    |          x-scala-file-hash: SHA-512
    |      responses:
    |        200:
    |          description: Success
    |""".stripMargin

  test("Ensure routes are generated") {
    val (
      _,
      _,
      Servers(Server(pkg, extraImports, genHandler :: genResource :: Nil) :: Nil)
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)

    val handler  = q"""
      trait Handler {
        def putFoo(respond: Resource.putFooResponse.type)(foo: String, bar: Long, baz: (File, Option[String], ContentType, String)): scala.concurrent.Future[Resource.putFooResponse]
        def putFooMapFileField(fieldName: String, fileName: Option[String], contentType: ContentType): File
        def putFooUnmarshalToFile[F[_]: Functor](hashType: F[String], destFn: (String, Option[String], ContentType) => File)(implicit mat: Materializer): Unmarshaller[Multipart.FormData.BodyPart, (File, Option[String], ContentType, F[String])] = Unmarshaller { implicit executionContext =>
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
        implicit def jsonFSU[T: io.circe.Decoder]: Unmarshaller[String, T] = Unmarshaller[String, T] { implicit ev =>
          string => io.circe.Json.fromString(string).as[T].left.flatMap(err => io.circe.jawn.parse(string).flatMap(_.as[T])).fold(scala.concurrent.Future.failed _, scala.concurrent.Future.successful _)
        }
        def routes(handler: Handler)(implicit mat: akka.stream.Materializer): Route = {
          (put & path("foo") & ({
            object putFooParts {
              sealed trait Part
              case class IgnoredPart(unit: Unit) extends Part
              case class foo(value: String) extends Part
              case class bar(value: Long) extends Part
              case class baz(value: (File, Option[String], ContentType, String)) extends Part
            }
            val UnmarshalfooPart: Unmarshaller[Multipart.FormData.BodyPart, putFooParts.foo] = Unmarshaller { implicit executionContext =>
              part => {
                val json: Unmarshaller[Multipart.FormData.BodyPart, String] = MFDBPviaFSU(jsonEntityUnmarshaller[String])
                val string: Unmarshaller[Multipart.FormData.BodyPart, String] = MFDBPviaFSU(BPEviaFSU(jsonDecoderUnmarshaller))
                Unmarshaller.firstOf(json, string).apply(part).map(putFooParts.foo.apply)
              }
            }
            val UnmarshalbarPart: Unmarshaller[Multipart.FormData.BodyPart, putFooParts.bar] = Unmarshaller { implicit executionContext =>
              part => {
                val json: Unmarshaller[Multipart.FormData.BodyPart, Long] = MFDBPviaFSU(jsonEntityUnmarshaller[Long])
                val string: Unmarshaller[Multipart.FormData.BodyPart, Long] = MFDBPviaFSU(BPEviaFSU(jsonDecoderUnmarshaller))
                Unmarshaller.firstOf(json, string).apply(part).map(putFooParts.bar.apply)
              }
            }
            val UnmarshalbazPart: Unmarshaller[Multipart.FormData.BodyPart, putFooParts.baz] = handler.putFooUnmarshalToFile[Id]("SHA-512", handler.putFooMapFileField(_, _, _)).map({
              case (v1, v2, v3, v4) =>
                putFooParts.baz((v1, v2, v3, v4))
            })
            val fileReferences = new AtomicReference(List.empty[File])
            extractExecutionContext.flatMap { implicit executionContext =>
              extractMaterializer.flatMap { implicit mat =>
                entity(as[Multipart.FormData]).flatMap { formData =>
                  val collectedPartsF: Future[Either[Throwable, (Option[String], Option[Long], Option[(File, Option[String], ContentType, String)])]] = for (results <- formData.parts.mapConcat {
                    part => if (Set[String]("foo", "bar", "baz").contains(part.name)) part :: Nil else {
                      part.entity.discardBytes()
                      Nil
                    }
                  }.mapAsync(1) { part =>
                    {
                      part.name match {
                        case "foo" =>
                          SafeUnmarshaller(UnmarshalfooPart).apply(part)
                        case "bar" =>
                          SafeUnmarshaller(UnmarshalbarPart).apply(part)
                        case "baz" =>
                          SafeUnmarshaller(AccumulatingUnmarshaller(fileReferences, UnmarshalbazPart)(_.value._1)).apply(part)
                        case _ =>
                          SafeUnmarshaller(implicitly[Unmarshaller[Multipart.FormData.BodyPart, Unit]].map(putFooParts.IgnoredPart.apply(_))).apply(part)
                      }
                    }
                  }.toMat(Sink.seq[Either[Throwable, putFooParts.Part]])(Keep.right).run()) yield {
                    results.toList.sequence.map { successes =>
                      val fooO = successes.collectFirst({
                        case putFooParts.foo(v1) => v1
                      })
                      val barO = successes.collectFirst({
                        case putFooParts.bar(v1) => v1
                      })
                      val bazO = successes.collectFirst({
                        case putFooParts.baz((v1, v2, v3, v4)) =>
                          (v1, v2, v3, v4)
                      })
                      (fooO, barO, bazO)
                    }
                  }
                  handleExceptions(ExceptionHandler({
                    case e: Throwable =>
                      fileReferences.get().foreach(_.delete())
                      throw e
                  })) & handleRejections { (rejections: scala.collection.immutable.Seq[Rejection]) =>
                    fileReferences.get().foreach(_.delete())
                    None
                  } & mapResponse { resp =>
                    fileReferences.get().foreach(_.delete())
                    resp
                  } & onSuccess(collectedPartsF)
                }
              }
            }.flatMap(_.fold(t => throw t, {
              case (fooO, barO, bazO) =>
                val maybe: Either[Rejection, (String, Long, (File, Option[String], ContentType, String))] = for (foo <- fooO.toRight(MissingFormFieldRejection("foo")); bar <- barO.toRight(MissingFormFieldRejection("bar")); baz <- bazO.toRight(MissingFormFieldRejection("baz"))) yield {
                  (foo, bar, baz)
                }
                maybe.fold(reject(_), tprovide(_))
            }))
          }: Directive[(String, Long, (File, Option[String], ContentType, String))])) {
            (foo, bar, baz) => complete(handler.putFoo(putFooResponse)(foo, bar, baz))
          }
        }
        sealed abstract class putFooResponse(val statusCode: StatusCode)
        case object putFooResponseOK extends putFooResponse(StatusCodes.OK)
        object putFooResponse {
          implicit val putFooTRM: ToResponseMarshaller[putFooResponse] = Marshaller { implicit ec =>
            resp => putFooTR(resp)
          }
          implicit def putFooTR(value: putFooResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r: putFooResponseOK.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => putFooResponse): putFooResponse = ev(value)
          def OK: putFooResponse = putFooResponseOK
        }
      }
    """

    genHandler.structure shouldEqual handler.structure
    genResource.structure shouldEqual resource.structure
  }
}
