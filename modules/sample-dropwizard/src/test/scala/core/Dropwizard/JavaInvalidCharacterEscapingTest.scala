package core.Dropwizard

import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.databind.ObjectMapper
import invalidCharacters.client.dropwizard.invalidCharacters.InvalidCharactersClient
import invalidCharacters.server.dropwizard.definitions.{InvalidCharacters, InvalidCharactersEnum}
import io.netty.buffer.Unpooled
import java.net.{SocketAddress, URI, URLDecoder}
import java.util.concurrent.{CompletableFuture, CompletionStage}
import java.util.function
import org.asynchttpclient.Response.ResponseBuilder
import org.asynchttpclient.netty.EagerResponseBodyPart
import org.asynchttpclient.uri.Uri
import org.asynchttpclient.{HttpResponseStatus, Request, Response}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.JavaConverters._

object JavaInvalidCharacterEscapingTest {
  private implicit class RichString(private val s: String) extends AnyVal {
    def dec: String = URLDecoder.decode(s, "UTF-8")
  }

  private object OkStatus extends HttpResponseStatus(Uri.create("http://localhost:1234/foo?foo^bar=query-param")) {
    override def getStatusCode = 200
    override def getStatusText = "OK"
    override def getProtocolName = "HTTP"
    override def getProtocolMajorVersion = 1
    override def getProtocolMinorVersion = 1
    override def getProtocolText = "HTTP/1.1"
    override def getRemoteAddress: SocketAddress = ???
    override def getLocalAddress: SocketAddress = ???
  }
}

class JavaInvalidCharacterEscapingTest extends AnyFreeSpec with Matchers {
  import JavaInvalidCharacterEscapingTest._

  "Invalid characters in Java enums should be escaped" in {
    InvalidCharactersEnum.NORMAL.getName mustBe "normal"
    InvalidCharactersEnum.BANG_MOO_COLON_COW_SEMICOLON.getName mustBe "!moo:cow;"
    InvalidCharactersEnum.POUND_YEAH.getName mustBe "#yeah"
    InvalidCharactersEnum.WEIRD_AT.getName mustBe "weird@"
  }

  "Invalid characters in Java POJO properties should be escaped" in {
    val invChar = new InvalidCharacters.Builder("stuff", InvalidCharactersEnum.POUND_YEAH).build()
    invChar.getCloseSquareBraceMoo mustBe "stuff"
    invChar.getSomeEnumAsteriskCaret mustBe InvalidCharactersEnum.POUND_YEAH

    classOf[InvalidCharacters].getDeclaredField("closeSquareBraceMoo").getAnnotation(classOf[JsonProperty]).value mustBe "]moo"
    classOf[InvalidCharacters].getDeclaredField("someEnumAsteriskCaret").getAnnotation(classOf[JsonProperty]).value mustBe "some-enum*^"
  }

  "Invalid characters in Java operation param names should be escaped" in {
    val httpClient = new function.Function[Request, CompletionStage[Response]] {
      override def apply(request: Request): CompletionStage[Response] = {
        println(request.getUri)
        println(request.getQueryParams.asScala.map(_.getName))
        val qps = request.getQueryParams.asScala.map(p => (p.getName.dec, p.getValue.dec))
        val fps = request.getFormParams.asScala.map(p => (p.getName.dec, p.getValue.dec))
        qps.find(_._1 == "foo^bar").map(_._2) mustBe Some("firstarg")
        fps.find(_._1 == "a*b").map(_._2) mustBe Some("secondarg")
        fps.find(_._1 == "bc?").map(_._2) mustBe Some("thirdarg")
        fps.find(_._1 == "d/c").map(_._2) mustBe Some("fourtharg")
        val response = new ResponseBuilder()
        response.accumulate(OkStatus)
        response.accumulate(new EagerResponseBodyPart(
          Unpooled.copiedBuffer(new ObjectMapper().writeValueAsBytes(new InvalidCharacters.Builder("foo", InvalidCharactersEnum.WEIRD_AT).build())),
          true
        ))
        CompletableFuture.completedFuture(response.build())
      }
    }

    val client = new InvalidCharactersClient.Builder(new URI("http://localhost:1234")).withHttpClient(httpClient).build()
    val response = client.getFoo("firstarg", "secondarg", "thirdarg", "fourtharg").call().toCompletableFuture.get()
    response.fold(
      { invChar =>
        invChar.getCloseSquareBraceMoo mustBe "foo"
        invChar.getSomeEnumAsteriskCaret mustBe invalidCharacters.client.dropwizard.definitions.InvalidCharactersEnum.WEIRD_AT
      }
    )
  }
}
