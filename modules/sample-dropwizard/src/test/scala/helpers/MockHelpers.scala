package helpers

import com.fasterxml.jackson.databind.ObjectMapper
import io.netty.handler.codec.http.EmptyHttpHeaders
import java.io.ByteArrayInputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.util.Collections
import java.util.concurrent.CompletableFuture
import javax.ws.rs.container.AsyncResponse
import org.asynchttpclient.Response
import org.asynchttpclient.uri.Uri
import org.mockito.{ ArgumentMatchersSugar, MockitoSugar }
import org.scalatest.Assertions
import scala.reflect.ClassTag

object MockHelpers extends Assertions with MockitoSugar with ArgumentMatchersSugar {
  def mockAsyncResponse[T](future: CompletableFuture[T])(implicit cls: ClassTag[T]): AsyncResponse = {
    val asyncResponse = mock[AsyncResponse]

    when(asyncResponse.resume(any[T])) thenAnswer [AnyRef] { response =>
      response match {
        case t: Throwable => future.completeExceptionally(t)
        case other: T     => future.complete(other)
        case other        => fail(s"AsyncResponse.resume expected an object of type ${cls.runtimeClass.getName}, but got ${other.getClass.getName} instead")
      }
    }

    asyncResponse
  }

  def mockAHCResponse[T](uri: String, status: Int, maybeBody: Option[T] = None)(implicit mapper: ObjectMapper): Response = {
    val response = mock[Response]
    when(response.getUri) thenReturn Uri.create(uri)
    when(response.hasResponseStatus) thenReturn true
    when(response.getStatusCode) thenReturn status
    when(response.getStatusText) thenReturn "Some Status"
    when(response.hasResponseHeaders) thenReturn true
    when(response.getHeaders) thenReturn EmptyHttpHeaders.INSTANCE
    when(response.getHeader(any)) thenReturn null
    when(response.getHeaders(any)) thenReturn Collections.emptyList()
    maybeBody match {
      case None =>
        when(response.hasResponseBody) thenReturn true
      case Some(body) =>
        val responseBytes = mapper.writeValueAsBytes(body)
        val responseStr   = new String(responseBytes, StandardCharsets.UTF_8)
        when(response.hasResponseBody) thenReturn true
        when(response.getResponseBody(any)) thenReturn responseStr
        when(response.getResponseBody) thenReturn responseStr
        when(response.getResponseBodyAsStream) thenReturn new ByteArrayInputStream(responseBytes)
        when(response.getResponseBodyAsByteBuffer) thenReturn ByteBuffer.wrap(responseBytes)
        when(response.getResponseBodyAsBytes) thenReturn responseBytes
    }
    response
  }

}
