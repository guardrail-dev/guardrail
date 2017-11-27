package core.issues

import _root_.io.swagger.parser.SwaggerParser
import cats.instances.all._
import com.twilio.swagger.codegen.generators.AkkaHttp
import com.twilio.swagger.codegen.{ClassDefinition, Client, Clients, Context, ClientGenerator, ProtocolGenerator, RandomType, CodegenApplication, Target}
import org.scalatest.{FunSuite, Matchers}
import scala.meta._

class Issue61 extends FunSuite with Matchers {
  val swagger = new SwaggerParser().parse(s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |definitions:
    |  Foo:
    |    type: array
    |    items:
    |      type: string
    |""".stripMargin)

  test("Generate plain array alias definition") {
    val definitions = Target.unsafeExtract(ProtocolGenerator.fromSwagger[CodegenApplication](swagger).foldMap(AkkaHttp)).elems
    val RandomType(tpe, List(tdef, cdef)) :: Nil = definitions

    tpe.structure shouldBe(t"List[String]".structure)
    tdef.structure shouldBe(q"type Foo = List[String]".structure)
    cdef.structure shouldBe(q"object Foo".structure)
  }
}
