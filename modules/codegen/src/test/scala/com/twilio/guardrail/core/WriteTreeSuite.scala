package com.twilio.guardrail.core

import com.twilio.guardrail.WriteTree
import org.scalatest.FunSuite
import java.nio.file.Files
import org.scalatest.Matchers

class WriteTreeSuite extends FunSuite with Matchers {
  test("Ensure that even if we don't overwrite output files, the path is returned") {
    val path = Files.createTempFile("guardrail-writeTree", ".txt")
    Files.delete(path)

    val contents = "example contents".getBytes

    val (firstLog, firstPath)   = WriteTree.unsafeWriteTreeLogged(WriteTree(path, contents)).run
    val (secondLog, secondPath) = WriteTree.unsafeWriteTreeLogged(WriteTree(path, contents)).run

    val _1 = firstLog shouldBe (Nil)
    val _2 = secondLog shouldBe (Nil)

    val _3 = firstPath shouldBe (secondPath)

    Files.delete(path)
  }
}
