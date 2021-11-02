guardrail [![Build Status](https://github.com/guardrail-dev/guardrail/workflows/CI/badge.svg)](https://github.com/guardrail-dev/guardrail/actions?query=workflow%3A%22CI%22) | [![codecov](https://codecov.io/gh/guardrail-dev/guardrail/branch/master/graph/badge.svg?token=ssLYYkVBgv)](https://codecov.io/gh/guardrail-dev/guardrail) | [![Join the chat at https://gitter.im/guardrail-dev/guardrail](https://badges.gitter.im/guardrail-dev/guardrail.svg)](https://gitter.im/guardrail-dev/guardrail?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
===

guardrail is a code generation tool, capable of reading from OpenAPI/Swagger specification files and generating both Scala and Java source code, targeting various libraries and frameworks listed here:

- Scala: `akka-http` and `http4s`, both backed by `circe`, as well as `dropwizard` backed by `jackson`
- Java: `dropwizard` and `spring-mvc`, both backed by `jackson`

Build tool plugins
------------------

| Plugins | versions | docs |
|---|---|---|
| [`guardrail-dev/sbt-guardrail`](https://github.com/guardrail-dev/sbt-guardrail) | [![sbt-guardrail](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/sbt-guardrail/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:sbt-guardrail) | [docs](docs/plugins/sbt.md) |
| [`guardrail-dev/guardrail-maven-plugin`](https://github.com/guardrail-dev/guardrail-maven-plugin) | [![guardrail-maven-plugin](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-maven-plugin/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-maven-plugin_2.12) | [docs](docs/plugins/maven.md) |
| [`guardrail-dev/guardrail-gradle-plugin`](https://github.com/guardrail-dev/guardrail-gradle-plugin) | `guardrail-gradle-plugin` | [Plugin Portal](https://plugins.gradle.org/plugin/com.twilio.guardrail) | [docs](docs/plugins/gradle.md) |
| [CLI support](./modules/cli) | Latest | [`cs install guardrail`](https://get-coursier.io/docs/cli-install), [docs](docs/plugins/make.md) |

New to guardrail?
---

Check out the [docs](https://guardrail.dev/)!

Compatible library versions are listed in [COMPATIBILITY.md](COMPATIBILITY.md)

guardrail module versions
---

guardrail is modularized, using [`sbt-version-policy`](https://github.com/scalacenter/sbt-version-policy) to ensure binary compatibility between dependent modules.

The dependency chain and versions of published modules are listed below for reference:

| module  | version  | depends on |
|-----|-----|-----|
| guardrail-core | [![guardrail-core](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-core_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-core_2.12) |  |
| guardrail-java-support | [![guardrail-java-support](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-java-support_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-java-support_2.12) | core |
| guardrail-java-async-http | [![guardrail-java-async-http](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-java-async-http_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-java-async-http_2.12) | java-support |
| guardrail-java-dropwizard | [![guardrail-java-dropwizard](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-java-dropwizard_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-java-dropwizard_2.12) | java-support, java-async-http |
| guardrail-java-spring-mvc | [![guardrail-java-spring-mvc](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-java-spring-mvc_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-java-spring-mvc_2.12) | java-support |
| guardrail-scala-support | [![guardrail-scala-support](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-scala-support_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-scala-support_2.12) | core |
| guardrail-scala-akka-http | [![guardrail-scala-akka-http](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-scala-akka-http_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-scala-akka-http_2.12) | scala-support |
| guardrail-scala-dropwizard | [![guardrail-scala-dropwizard](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-scala-dropwizard_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-scala-dropwizard_2.12) | scala-support |
| guardrail-scala-endpoints | [![guardrail-scala-endpoints](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-scala-endpoints_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-scala-endpoints_2.12) | scala-support |
| guardrail-scala-http4s | [![guardrail-scala-http4s](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-scala-http4s_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-scala-http4s_2.12) | scala-support |
| guardrail | [![guardrail](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail_2.12) | core, java-support, java-dropwizard,<br /> java-spring-mvc, java-async-http,<br /> scala-support, scala-akka-http,<br /> scala-dropwizard, scala-endpoints,<br /> scala-http4s |
| guardrail-cli | [![guardrail-cli](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-cli_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-cli_2.12) | guardrail |

Interested in contributing?
---

[CONTRIBUTING.md](CONTRIBUTING.md) provides an overview of how the project is structured, expectations, and information around writing new integration tests.
The [issue tracker](https://github.com/guardrail-dev/guardrail/issues) also has tags for [`help wanted`](https://github.com/guardrail-dev/guardrail/issues?q=is%3Aissue+is%3Aopen+label%3A%22help+wanted%22) and [`good first issue`](https://github.com/guardrail-dev/guardrail/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22).

Adopters
========

- [Avast](https://www.avast.com/)
- [Twilio](https://www.twilio.com/)

Contributors
============

We used to have a list of contributors here, but [github's Contributors page](https://github.com/guardrail-dev/guardrail/graphs/contributors) is much more accurate. Thanks to those who contributed before the project was open sourced!
