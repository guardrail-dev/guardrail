---
layout: docs
title: "guardrail: Java: Installation"
---

Installation
============

guardrail is available as a modular core, with both [sbt](https://github.com/twilio/sbt-guardrail) and [Maven](https://github.com/twilio/guardrail-maven-plugin) integration. The core can also be run as a stand-alone [CLI](https://github.com/twilio/guardrail/blob/978a92db3dd46812aa19f05050995f864cbb5bb3/build.sbt#L33-L48) application, with full support for all features.

If compiling with Scala < 2.13.x, you'll need to enable `-Ypartial-unification`:

```scala
scalacOptions += "-Ypartial-unification"
```

If compiling with Scala < 2.12.x, you'll additionally need the `-Xexperimental` flag:

```scala
scalacOptions += "-Xexperimental"
```
