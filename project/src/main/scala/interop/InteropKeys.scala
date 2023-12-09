package dev.guardrail.sbt.interop

object InteropKeys {
  val generateTask = _root_.sbt.taskKey[List[java.io.File]]("A source-code-generating task")
}
