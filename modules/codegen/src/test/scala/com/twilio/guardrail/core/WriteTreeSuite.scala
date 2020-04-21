package com.twilio.guardrail.core

import com.twilio.guardrail.WriteTree
import java.nio.file.Files
import scala.concurrent.Future
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class WriteTreeSuite extends AnyFunSuite with Matchers {
  test("Ensure that even if we don't overwrite output files, the path is returned") {
    val path = Files.createTempFile("guardrail-writeTree", ".txt")
    Files.delete(path)

    val contents = "example contents".getBytes

    val (firstLog, firstPath)   = WriteTree.unsafeWriteTreeLogged(WriteTree(path, Future.successful(contents))).run
    val (secondLog, secondPath) = WriteTree.unsafeWriteTreeLogged(WriteTree(path, Future.successful(contents))).run

    val _1 = firstLog shouldBe (Nil)
    val _2 = secondLog shouldBe (Nil)

    val _3 = firstPath shouldBe (secondPath)

    Files.delete(path)
  }
}
