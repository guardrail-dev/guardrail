Introduction
===

[![Build Status](https://github.com/guardrail-dev/guardrail/workflows/CI/badge.svg)](https://github.com/guardrail-dev/guardrail/actions?query=workflow%3A%22CI%22) | [![codecov](https://codecov.io/gh/guardrail-dev/guardrail/branch/master/graph/badge.svg?token=ssLYYkVBgv)](https://codecov.io/gh/guardrail-dev/guardrail) | [![Matrix chat](https://img.shields.io/matrix/guardrail:matrix.org.svg?label=matrix&server_fqdn=matrix.org)](https://matrix.to/#/#guardrail:matrix.org) | [![Join the chat at https://gitter.im/guardrail-dev/guardrail](https://badges.gitter.im/guardrail-dev/guardrail.svg)](https://gitter.im/guardrail-dev/guardrail?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) | [![Scala Steward badge](https://img.shields.io/badge/Scala_Steward-helping-blue.svg?style=flat&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAMAAAARSr4IAAAAVFBMVEUAAACHjojlOy5NWlrKzcYRKjGFjIbp293YycuLa3pYY2LSqql4f3pCUFTgSjNodYRmcXUsPD/NTTbjRS+2jomhgnzNc223cGvZS0HaSD0XLjbaSjElhIr+AAAAAXRSTlMAQObYZgAAAHlJREFUCNdNyosOwyAIhWHAQS1Vt7a77/3fcxxdmv0xwmckutAR1nkm4ggbyEcg/wWmlGLDAA3oL50xi6fk5ffZ3E2E3QfZDCcCN2YtbEWZt+Drc6u6rlqv7Uk0LdKqqr5rk2UCRXOk0vmQKGfc94nOJyQjouF9H/wCc9gECEYfONoAAAAASUVORK5CYII=)](https://scala-steward.org)

[`guardrail`](https://github.com/guardrail-dev/guardrail) is a code generation tool, capable of reading from OpenAPI/Swagger specification files and generating high quality source code for a variety of languages and frameworks.

Getting started
===

The following build tool plugins are available:

- [`sbt-guardrail`](https://github.com/guardrail-dev/sbt-guardrail)
- [`guardrail-maven-plugin`](https://github.com/guardrail-dev/guardrail-maven-plugin)
- [`guardrail-gradle-plugin`](https://github.com/guardrail-dev/guardrail-gradle-plugin)
- as well as a manual CLI runner available via [coursier](cli.md)

New plugins are straightforward, simply call one of the exposed functions from
[`GuardrailRunner`](https://github.com/guardrail-dev/guardrail/blob/master/modules/core/src/main/scala/dev/guardrail/runner/GuardrailRunner.scala)
with appropriate arguments in order to get a sequence of generated files.

**NB:** Ensure either you or your build tool deduplicates the file sequence, as
they are not guaranteed to be unique across multiple runs of the guardrail core.

Additionally, check out the [`guardrail-sample`](https://github.com/topics/guardrail-sample) topic on GitHub for more examples.

**Consulting**: If you need help getting started, getting migrated, or adding features, please contact [hello@guardrail.dev](mailto:hello@guardrail.dev).
