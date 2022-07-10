package core.issues

import issues.issue1532.client.akkaHttp.Client
import issues.issue1532.client.akkaHttp.definitions.Foo
import issues.issue1532.server.akkaHttp.{ Handler, Resource }

class Issue1532CompileTests {
  // No need to actually run the tests, we just want to be sure that these expressions actually compile
  // As a result, we stub out the Client and Handler with ???, then call the functions inside a def as well.
  def client: Client = ???
  def clientTest     = client.doFoo(p1 = 123L)

  def handler: Handler = ???
  def handlerTest      = handler.doFoo(Resource.DoFooResponse)(p1 = 234L)

  def foo = Foo(bar = 345)
}
