---
layout: docs
title: "Installation - akka-http - scala - guardrail"
---

Installation
============

guardrail is available as a modular core, with both [sbt](https://github.com/guardrail-dev/sbt-guardrail) and [Maven](https://github.com/guardrail-dev/guardrail-maven-plugin) integration. The core can also be run as a stand-alone [CLI](https://github.com/guardrail-dev/guardrail/blob/978a92db3dd46812aa19f05050995f864cbb5bb3/build.sbt#L33-L48) application, with full support for all features.

If compiling with Scala < 2.13.x, you'll need to enable `-Ypartial-unification`:

```scala
scalacOptions += "-Ypartial-unification"
```

If compiling with Scala < 2.12.x, you'll additionally need the `-Xexperimental` flag:

```scala
scalacOptions += "-Xexperimental"
```

<span style="float: left">[Prev: What is guardrail?](what-is-guardrail)</span>
<span style="float: right">[Next: Sample API specification](sample-api-specification)</span>
