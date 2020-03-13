guardrail [![Build Status](https://travis-ci.com/twilio/guardrail.svg?branch=master)](https://travis-ci.com/twilio/guardrail)
===

guardrail is a code generation tool, capable of reading from OpenAPI/Swagger specification files and generating Scala source code, primarily targeting the akka-http and http4s web frameworks, using circe for JSON encoding/decoding.

New to guardrail?
---

Check out the [docs](https://guardrail.dev/)!

Compatible library versions are listed in [COMPATIBILITY.md](COMPATIBILITY.md)

Interested in contributing?
---

[CONTRIBUTING.md](CONTRIBUTING.md) provides an overview of how the project is structured, expectations, and information around writing new integration tests.
The [issue tracker](https://github.com/twilio/guardrail/issues) also has tags for [`help wanted`](https://github.com/twilio/guardrail/issues?q=is%3Aissue+is%3Aopen+label%3A%22help+wanted%22) and [`good first issue`](https://github.com/twilio/guardrail/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22).

Build tool plugins
------------------

- SBT
  - [`twilio/sbt-guardrail`](https://github.com/twilio/sbt-guardrail) [docs](docs/plugins/sbt.md)

- Maven
  - [`twilio/guardrail-maven-plugin`](https://github.com/twilio/guardrail-maven-plugin) [docs](docs/plugins/maven.md)

- Makefile
  - [docs](docs/plugins/make.md)

Adopters
========

- [Avast](https://www.avast.com/)
- [Twilio](https://www.twilio.com/)

Contributors
============

We used to have a list of contributors here, but [github's Contributors page](https://github.com/twilio/guardrail/graphs/contributors) is much more accurate. Thanks to those who contributed before the project was open sourced!
