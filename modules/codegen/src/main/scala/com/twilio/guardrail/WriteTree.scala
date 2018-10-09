package com.twilio.guardrail

import cats.data.{ Writer, WriterT }
import cats.implicits._
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{ Files, Path, StandardOpenOption }
import scala.io.AnsiColor
import scala.meta._

case class WriteTree(path: Path, contents: Tree)
object WriteTree {
  val unsafeWriteTreeLogged: WriteTree => Writer[List[String], Path] = {
    case WriteTree(path, tree) =>
      val UTF8 = java.nio.charset.Charset.availableCharsets.get("UTF-8")
      val data = tree.syntax.getBytes(UTF8)
      Files.createDirectories(path.getParent)
      for {
        _ <- if (Files.exists(path)) {
          val exists: Array[Byte] = Files.readAllBytes(path)
          val diffIdx: Option[Int] =
            exists
              .zip(data)
              .zipWithIndex
              .find({ case ((a, b), _) => a != b })
              .map(_._2)
              .orElse(Some(Math.max(exists.length, data.length)))
              .filterNot(Function.const(data.length == exists.length))

          diffIdx.traverse { diffIdx =>
            val existSample = new String(exists, UTF_8)
              .slice(Math.max(diffIdx - 10, diffIdx), Math.max(diffIdx - 10, diffIdx) + 50)
              .replace("\n", "\\n")
            val newSample = new String(data, UTF_8)
              .slice(Math.max(diffIdx - 10, diffIdx), Math.max(diffIdx - 10, diffIdx) + 50)
              .replace("\n", "\\n")

            Writer.tell(List(s"""|
            |${AnsiColor.RED}Warning:${AnsiColor.RESET}
            |  The file $path contained different content than was expected.
            |
            |  Existing file: $existSample
            |  New file     : $newSample
            |""".stripMargin))
          }
        } else Writer(List.empty[String], Option.empty[Unit])
      } yield Files.write(path, data, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
  }

  val unsafeWriteTree: WriteTree => Path =
    unsafeWriteTreeLogged.map {
      case WriterT((lines, path)) =>
        lines.foreach(System.err.println(_))
        path
    }
}
