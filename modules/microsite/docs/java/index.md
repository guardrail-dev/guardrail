---
layout: docs
title: "java - guardrail"
---

guardrail can generate Java clients and servers for the following
frameworks:

* [`dropwizard`](dropwizard/)
* [`spring-mvc`](spring-mvc/)

In addition, the Java generation backend supports use of either standard
Java collections types (such as `java.util.Optional` and
`java.util.Map`), or [Vavr](https://vavr.io/) collections types (such as
`io.vavr.control.Option` and `io.vavr.collection.Vector`).  Vavr's
collection types are more internally consistent and attempt to provide
an interface familiar to functional programmers.  Scala developers will
find their APIs especially familiar.

To make use of the Vavr generation, you need to instead use guardrail's
module system.  Instead of specifying a `framework`, instead specify a
series of `module`s that describe the framework, protocol, and
collectsions library generators to use.  For example, if I were using
the guardrail Maven plugin, and wanted to use Vavr with Dropwizard, I'd
include in my `<configuration>`:

```xml
<modules>
  <module>java-vavr</module>
  <module>jackson</module>
  <module>dropwizard</module>
</module>
```

Currently, Vavr is only supported with the `dropwizard` framework.
