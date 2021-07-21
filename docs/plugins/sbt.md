SBT
===

The [`guardrail-dev/sbt-guardrail`](https://github.com/guardrail-dev/sbt-guardrail) plugin installation instructions can be found in that project's README.

By putting the following in your `build.sbt`, generated classes for petstore and github clients and `myserver` server routes are available the next time the `compile` or `guardrail` tasks are run. The motivation for generating servers in your test sources is to facilitate [generating "real" mock downstream servers](https://guardrail.dev/scala/akka-http/generating-a-server#generating-test-only-real-server-mocks-for-unit-tests).

```sbt
Compile / guardrailTasks := List(
  Client(file("petstore.yaml")),
  Client(file("github.yaml"), pkg="com.example.clients.github"),
  Server(file("myserver.yaml"), pkg="com.example.server", tracing=true)
)

Test / guardrailTasks := List(
  Server(file("petstore.yaml")),
  Server(file("github.yaml"), pkg="com.example.tests.github"),
  Client(file("myserver.yaml"), pkg="com.example.tests.server", tracing=true)
)
```

For the curious, generated sources end up in `target/scala-2.12/src_managed/`. These will be overwritten every time `compile` runs, so modification is not possible. If you find the generated source does not fit your needs, please see the section on [guardrail-specific extensions](https://guardrail.dev/scala/akka-http/guardrail-extensions) in the documentation.
