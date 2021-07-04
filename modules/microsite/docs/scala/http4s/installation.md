---
layout: docs
title: "Installation - http4s - scala - guardrail"
---

Installation
============

guardrail is available as a modular core, with both [sbt](https://github.com/twilio/sbt-guardrail) and [Maven](https://github.com/twilio/guardrail-maven-plugin) integration. The core can also be run as a stand-alone [CLI](https://github.com/twilio/guardrail/blob/978a92db3dd46812aa19f05050995f864cbb5bb3/build.sbt#L33-L48) application, with full support for all features.

To generate servers or clients using the `http4s` framework, set `http4s` as the framework in the generation configuration in either sbt or maven.

If compiling with Scala < 2.13.x, you'll need to enable `-Ypartial-unification`:

```scala
scalacOptions += "-Ypartial-unification"
```

If compiling with Scala < 2.12.x, you'll additionally need the `-Xexperimental` flag:

```scala
scalacOptions += "-Xexperimental"
```

Additionally, you will need to manually include dependencies in your project for the following packages:
- `http4s`, dsl, server, and client dependencies
- `http4s-circe` for JSON decoding and encoding support
- `circe-generic` for JSON decoding and encoding support
- `cats-effect` for http4s integration
- `cats-core` for http4s integration

Versions of these libraries should be picked by checking out the [Compatibility Matrix](https://github.com/guardrail-dev/guardrail/blob/master/COMPATIBILITY.md).

<span style="float: left">[Prev: What is guardrail?](what-is-guardrail)</span>
<span style="float: right">[Next: Sample API specification](sample-api-specification)</span>
