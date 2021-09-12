package dev.guardrail.sbt

case class ExampleFramework(name: String,
                            projectName: String,
                            kinds: List[String] = List("client", "server"),
                            modules: List[String] = Nil)
