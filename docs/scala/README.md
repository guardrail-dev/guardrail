Scala
===

Installation
---

guardrail is available as a modular core, with both [sbt](https://github.com/guardrail-dev/sbt-guardrail) and [Maven](https://github.com/guardrail-dev/guardrail-maven-plugin) integration. The core can also be run as a stand-alone [CLI](cli.md) application, with full support for all features.

| module  | version  | depends on |
|-----|-----|-----|
| guardrail-scala-support | [![guardrail-scala-support](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-scala-support_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-scala-support_2.12) | core |
| guardrail-scala-akka-http | [![guardrail-scala-akka-http](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-scala-akka-http_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-scala-akka-http_2.12) | scala-support |
| guardrail-scala-dropwizard | [![guardrail-scala-dropwizard](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-scala-dropwizard_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-scala-dropwizard_2.12) | scala-support |
| guardrail-scala-http4s | [![guardrail-scala-http4s](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-scala-http4s_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-scala-http4s_2.12) | scala-support |
| guardrail-cli | [![guardrail-cli](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-cli_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-cli_2.12) | guardrail-core |

### Example SBT configuration

Latest [`sbt-guardrail`](https://github.com/guardrail-dev/sbt-guardrail) version [![sbt-guardrail](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/sbt-guardrail/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:sbt-guardrail) ([Other releases](https://github.com/guardrail-dev/sbt-guardrail/releases))

Put the following in `project/guardrail.sbt`:

```scala
addSbtPlugin("dev.guardrail" % "sbt-guardrail" % "<Please refer to the sbt-guardrail version above>")
libraryDependencies += "dev.guardrail" %% "guardrail-scala-http4s" % "<Please refer to the latest http4s module release above>"
```

... as well as the following in `build.sbt`
```scala
guardrailTasks in Compile := List(
  ScalaClient(file("github.yaml"), pkg="com.example.clients.github", framework="http4s"),
)
```

More configuration options available [here](https://github.com/guardrail-dev/sbt-guardrail).

<details>
<summary>If compiling with Scala < 2.13.x</summary>... you'll need to enable `-Ypartial-unification`:

```scala
scalacOptions += "-Ypartial-unification"
```
</details>

<details>
<summary>If compiling with Scala < 2.12.x</summary>... you'll additionally need the `-Xexperimental` flag:

```scala
scalacOptions += "-Xexperimental"
```
</details>

Frameworks
---

Configuration for the following libraries is available:

- [Akka-Http](scala/akka-http/README.md)
- [Http4s](scala/http4s/README.md)
- [Dropwizard](scala/dropwizard/README.md)

Sample repositories
---

There's a GitHub topic [here](https://github.com/topics/guardrail-sample), but a selection of those have been reproduced here:

- [guardrail-dev/guardrail-sample-sbt-akkahttp](https://github.com/guardrail-dev/guardrail-sample-sbt-akkahttp)
- [guardrail-dev/guardrail-sample-sbt-http4s](https://github.com/guardrail-dev/guardrail-sample-sbt-http4s)
- [guardrail-dev/guardrail-sample-sbt-http4s-zio](https://github.com/guardrail-dev/guardrail-sample-sbt-http4s-zio)


TODO: Figure out what to do with this
Additionally, you will need to manually include dependencies in your project for the following packages:
- `http4s`, dsl, server, and client dependencies
- `http4s-circe` for JSON decoding and encoding support
- `circe-generic` for JSON decoding and encoding support
- `cats-effect` for http4s integration
- `cats-core` for http4s integration

Versions of these libraries should be picked by checking out the [Compatibility Matrix](https://github.com/guardrail-dev/guardrail/blob/master/COMPATIBILITY.md).
