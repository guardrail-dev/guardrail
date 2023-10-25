---
layout: docs
title: "Generating Clients - akka-http - scala - guardrail"
---

Generating clients
==================

As we've seen in [Generating a Server](generating-a-server), guardrail-generated servers establish a mapping between our business logic and a cordoned off subset of HTTP. This permits us to focus on our business logic, without getting overloaded with the complexities of managing such a large protocol. The same is true with guardrail generated HTTP Clients: from a consumer's standpoint, HTTP calls should look like regular function calls, accepting domain-specific arguments and producing domain-specific results.

By generating minimal clients that only have enough business knowledge to map domain types to and from HTTP, opportunities for logical errors are effectively removed. While this does not eliminate logical errors entirely, establishing a firm boundary between the underlying protocol and hand-written code drastically reduces the scope of possible bugs.

The following is an example from the [akka-http](https://github.com/akka/akka-http) client generator:

```scala mdoc:passthrough
import dev.guardrail.docs._
DocsHelpers.renderScalaSnippet("akka-http", GeneratingClients)(
  """|// Two constructors are provided, one accepting the `httpClient`,
     |// `ExecutionContext`, and `Materializer` implicitly, the other accepting
     |// an explicit `httpClient`, but still accepting the `ExecutionContext` and
     |// `Materializer` as implicits.
  """.stripMargin,
  ""
)
```

(See it in action: [guardrail-dev/guardrail-sample-sbt-akkahttp](https://github.com/guardrail-dev/guardrail-sample-sbt-akkahttp))

Separation of protocol-concerns from API-level concerns
-------------------------------------------------------

As guardrail clients are built on top of the function type `HttpRequest => Future[HttpResponse]`, client configuration is reduced to function composition. Some ideas:

```scala
val singleRequestHttpClient = { (req: HttpRequest) =>
  Http().singleRequest(req)
}

val retryingHttpClient = { nextClient: (HttpRequest => Future[HttpResponse]) =>
    req: HttpRequest => nextClient(req).flatMap(resp => if (resp.status.intValue >= 500) nextClient(req) else Future.successful(resp))
}

val metricsHttpClient = { nextClient: (HttpRequest => Future[HttpResponse]) =>
    req: HttpRequest => {
        val resp = nextClient(req)
        resp.onSuccess { _resp =>
            trackMetrics(req.uri.path, _resp.status)
        }
        resp
    }
}

// Track metrics for every request, even retries
val retryingMetricsClient1: HttpRequest => Future[HttpResponse] = retryingHttpClient(metricsHttpClient(singleRequestHttpClient))

// Only track metrics for requests we didn't have to retry
val retryingMetricsClient2: HttpRequest => Future[HttpResponse] = metricsHttpClient(retryingHttpClient(singleRequestHttpClient))
```

<span style="float: left">[Prev: Generating a Server](generating-a-server)</span>
<span style="float: right">[Next: guardrail Extensions](guardrail-extensions)</span>
