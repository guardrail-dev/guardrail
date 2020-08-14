package helpers

import javax.ws.rs.container.AsyncResponse
import org.mockito.ArgumentMatchersSugar
import org.mockito.scalatest.IdiomaticMockito
import org.scalatest.Assertions
import scala.concurrent.Promise
import scala.reflect.ClassTag

object MockHelpers extends Assertions with IdiomaticMockito with ArgumentMatchersSugar {
  def mockAsyncResponse[T](future: Promise[T])(implicit cls: ClassTag[T]): AsyncResponse = {
    val asyncResponse = mock[AsyncResponse]

    { response: AnyRef =>
      response match {
        case t: Throwable => future.failure(t)
        case other: T     => future.success(other)
        case other        => fail(s"AsyncResponse.resume expected an object of type ${cls.runtimeClass.getName}, but got ${other.getClass.getName} instead")
      }
      true
    } willBe answered by asyncResponse.resume(*)

    asyncResponse
  }
}
