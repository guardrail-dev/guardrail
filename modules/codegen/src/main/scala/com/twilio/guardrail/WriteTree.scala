package com.twilio.guardrail

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{ Files, Path, StandardOpenOption }
import scala.io.AnsiColor
import scala.meta._

case class WriteTree(path: Path, contents: Tree)
object WriteTree {
  val unsafeWriteTree: WriteTree => Path = {
    case WriteTree(path, tree) =>
      val UTF8 = java.nio.charset.Charset.availableCharsets.get("UTF-8")
      val data = tree.syntax.getBytes(UTF8)
      Files.createDirectories(path.getParent)
      if (Files.exists(path)) {
        val exists: Array[Byte] = Files.readAllBytes(path)
        val diffIdx: Option[Int] =
          exists
            .zip(data)
            .zipWithIndex
            .find({ case ((a, b), _) => a != b })
            .map(_._2)
            .orElse(Some(Math.max(exists.length, data.length)))
            .filterNot(Function.const(data.length == exists.length))

        diffIdx.foreach { diffIdx =>
          val existSample = new String(exists, UTF_8)
            .slice(Math.max(diffIdx - 10, diffIdx), Math.max(diffIdx - 10, diffIdx) + 50)
            .replace("\n", "\\n")
          val newSample = new String(data, UTF_8)
            .slice(Math.max(diffIdx - 10, diffIdx), Math.max(diffIdx - 10, diffIdx) + 50)
            .replace("\n", "\\n")

          System.err.println(s"""|
          |${AnsiColor.RED}Warning:${AnsiColor.RESET}
          |  The file $path contained different content than was expected.
          |
          |  Existing file: $existSample
          |  New file     : $newSample
          |""".stripMargin)
        }
      }
      Files.write(path, data, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
  }
}
