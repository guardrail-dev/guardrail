package tests.core.issues

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
    |  Bar:
    |    type: integer
    |    format: int64
    |""".stripMargin)

  test("Generate plain array alias definition") {
    val definitions = Target.unsafeExtract(ProtocolGenerator.fromSwagger[CodegenApplication](swagger).foldMap(AkkaHttp)).elems
    val RandomType(_, tpe) :: _ :: Nil = definitions

    tpe.structure shouldBe(t"IndexedSeq[String]".structure)
  }

  test("Generate primitive type aliases") {
    val definitions = Target.unsafeExtract(ProtocolGenerator.fromSwagger[CodegenApplication](swagger).foldMap(AkkaHttp)).elems
    val _ :: RandomType(_, tpe) :: Nil = definitions

    tpe.structure shouldBe(t"Long".structure)
  }
}
