Project structure
=================

- `root`: Contains tests and nothing else
  - `codegen`: Codegen core. Defines algebras and default interpreters.
  - `sample`: Target for `run`, as well as integration tests for generated code. Can be generated via `run` in the sbt console.

Coding guidelines
=================

- Readability
- Maintainability
- Testability
- Defer effects
- Typesafe accessors for Java function calls
  - [`Extractable`](../modules/codegen/src/main/scala/com/twilio/guardrail/extract/Extractable.scala)
  - [`VendorExtensible`](../modules/codegen/src/main/scala/com/twilio/guardrail/extract/VendorExtension.scala)

Useful commands inside sbt console
==================================

- `testSuite`: Compile, test codegen, run sample codegen, compile sample, run tests inside sample
- `cli`: Useful for scripting: `sbt 'cli --client ...'`

Resources
=========

- Cats' Free Monad: http://typelevel.org/cats/datatypes/freemonad.html
- Scalameta Quasiquotes: https://github.com/scalameta/scalameta/blob/master/notes/quasiquotes.md
