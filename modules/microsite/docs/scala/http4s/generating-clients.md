---
layout: docs
title: "Generating Clients - http4s - scala - guardrail"
---

Generating clients
==================

As we've seen in [Generating a Server](generating-a-server), guardrail-generated servers establish a mapping between our business logic and a cordoned off subset of HTTP. This permits us to focus on our business logic, without getting overloaded with the complexities of managing such a large protocol. The same is true with guardrail generated HTTP Clients: from a consumer's standpoint, HTTP calls should look like regular function calls, accepting domain-specific arguments and producing domain-specific results.

By generating minimal clients that only have enough business knowledge to map domain types to and from HTTP, opportunities for logical errors are effectively removed. While this does not eliminate logical errors entirely, establishing a firm boundary between the underlying protocol and hand-written code drastically reduces the scope of possible bugs.

The following is an example from the [http4s](https://github.com/http4s/http4s) client generator:

```scala mdoc:passthrough
import dev.guardrail.docs._
DocsHelpers.renderScalaSnippet("http4s", GeneratingClients)(
  """|// Two constructors are provided, one accepting the `httpClient` and `Async`
     |// implicitly, the other accepting an explicit `httpClient`, but still
     |// accepting the `Async` implicitly
  """.stripMargin,
  ""
)
```

(See it in action: [guardrail-dev/guardrail-sample-http4s](https://github.com/guardrail-dev/guardrail-sample-http4s), [guardrail-dev/guardrail-sample-sbt-http4s-zio](https://github.com/guardrail-dev/guardrail-sample-sbt-http4s-zio))

Separation of protocol-concerns from API-level concerns
-------------------------------------------------------

As guardrail clients are built on top of any Http4s client type, client configuration is done the same way as you are
already familiar with when using Http4s.

Check out the docs for [Http4s Clients](https://http4s.org/v0.23/client/).

<span style="float: left">[Prev: Generating a Server](generating-a-server)</span>
