package dev.guardrail

import cats.data.{ Writer, WriterT }
import cats.syntax.all._
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{ Files, Path, StandardOpenOption }
import scala.io.AnsiColor
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration

sealed trait WriteTreeState
case object FileAbsent    extends WriteTreeState
case object FileDifferent extends WriteTreeState
case object FileIdentical extends WriteTreeState
case class WriteTree(path: Path, contents: Future[Target[Array[Byte]]])
object WriteTree {
  val writeTreeLogged: WriteTree => Target[Writer[List[String], Path]] = {
    case WriteTree(path, dataF) =>
      val _ = Files.createDirectories(path.getParent)
      Await
        .result(dataF, Duration.Inf)
        .map { data =>
          for {
            writeState <- if (Files.exists(path)) {
              val exists: Array[Byte] = Files.readAllBytes(path)
              val diffIdx: Option[Int] =
                exists
                  .zip(data)
                  .zipWithIndex
                  .find({ case ((a, b), _) => a != b })
                  .map(_._2)
                  .orElse(Some(Math.max(exists.length, data.length)))
                  .filterNot(Function.const(data.length == exists.length))

              diffIdx.fold[Writer[List[String], WriteTreeState]](Writer.value(FileIdentical))({ diffIdx =>
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
                |""".stripMargin)) >> Writer.value(FileDifferent)
              })
            } else Writer.value[List[String], WriteTreeState](FileAbsent)
          } yield writeState match {
            case FileAbsent    => Files.write(path, data, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
            case FileDifferent => Files.write(path, data, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
            case FileIdentical => path
          }
        }
  }

  val writeTree: WriteTree => Target[Path] =
    writeTreeLogged(_).map {
      case WriterT((lines, path)) =>
        lines.foreach(System.err.println(_))
        path
    }

  val unsafeWriteTreeLogged: WriteTree => Writer[List[String], Path] =
    writeTreeLogged.map(Target.unsafeExtract)

  val unsafeWriteTree: WriteTree => Path =
    writeTree.map(Target.unsafeExtract)
}
