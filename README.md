guardrail [![Build Status](https://github.com/guardrail-dev/guardrail/workflows/CI/badge.svg)](https://github.com/guardrail-dev/guardrail/actions?query=workflow%3A%22CI%22) | [![codecov](https://codecov.io/gh/guardrail-dev/guardrail/branch/master/graph/badge.svg?token=ssLYYkVBgv)](https://codecov.io/gh/guardrail-dev/guardrail) | [![maven central](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail_2.12) | [![Join the chat at https://gitter.im/guardrail-dev/guardrail](https://badges.gitter.im/guardrail-dev/guardrail.svg)](https://gitter.im/guardrail-dev/guardrail?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
===

guardrail is a code generation tool, capable of reading from OpenAPI/Swagger specification files and generating both Scala and Java source code, targeting various libraries and frameworks listed here:

- Scala: `akka-http` and `http4s`, both backed by `circe`, as well as `dropwizard` backed by `jackson`
- Java: `dropwizard` and `spring-mvc`, both backed by `jackson`

New to guardrail?
---

Check out the [docs](https://guardrail.dev/)!

Compatible library versions are listed in [COMPATIBILITY.md](COMPATIBILITY.md)

Interested in contributing?
---

[CONTRIBUTING.md](CONTRIBUTING.md) provides an overview of how the project is structured, expectations, and information around writing new integration tests.
The [issue tracker](https://github.com/guardrail-dev/guardrail/issues) also has tags for [`help wanted`](https://github.com/guardrail-dev/guardrail/issues?q=is%3Aissue+is%3Aopen+label%3A%22help+wanted%22) and [`good first issue`](https://github.com/guardrail-dev/guardrail/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22).

Build tool plugins
------------------

- SBT
  - [`guardrail-dev/sbt-guardrail`](https://github.com/guardrail-dev/sbt-guardrail) [docs](docs/plugins/sbt.md)

- Maven
  - [`guardrail-dev/guardrail-maven-plugin`](https://github.com/guardrail-dev/guardrail-maven-plugin) [docs](docs/plugins/maven.md)

- Gradle
  - [`guardrail-dev/guardrail-gradle-plugin`](https://github.com/guardrail-dev/guardrail-gradle-plugin) [docs](docs/plugins/gradle.md)

- Makefile
  - [docs](docs/plugins/make.md)

Adopters
========

- [Avast](https://www.avast.com/)
- [Twilio](https://www.twilio.com/)

Contributors
============

We used to have a list of contributors here, but [github's Contributors page](https://github.com/guardrail-dev/guardrail/graphs/contributors) is much more accurate. Thanks to those who contributed before the project was open sourced!
