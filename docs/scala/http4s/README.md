## Generating a Server

guardrail-generated servers come in two parts: a `Resource` and a `Handler`. The `Resource` contains all the routing logic, accepting a `Handler` as an argument to the `route` function in order to provide an HTTP service in whichever supported HTTP framework you're hosting your service in.

The following is an example from the [http4s](https://github.com/http4s/http4s) server generator:

```scala mdoc:passthrough
import dev.guardrail.docs._
DocsHelpers.renderScalaSnippet("http4s", GeneratingAServer)("""
    |// The `Handler` trait is fully abstracted from the underlying http framework. As a result, with the exception of some
    |// structural alterations (`F[_]` instead of `Future[_]` as the return type) the same handlers can be used with
    |// different `Resource` implementations from different framework generators. This permits greater compatibility between
    |// different frameworks without changing your business logic.
  """.stripMargin,
  ""
)
```

As all parameters are provided as arguments to the function stubs in the trait, there's no concern of forgetting to extract a query string parameter or introducing a typo in a form parameter name.

The routes and resources generated by guardrail can be hooked up into your HTTP4s server like so:

```scala
import org.http4s.ember.server.EmberServerBuilder

class UserImpl extends UserHandler[IO] { /* Your code here */ }
val userHandler: UserHandler[IO] = new UserImpl
val usersService = new UsersResource[IO]().routes(userHandler)
val httpApp = usersService.orNotFound

// Same basic server setup as in the http4s quickstart
EmberServerBuilder.default[IO]
  .withHttpApp(httpApp)
  .build
  .use(_ => IO.never)
  .as(ExitCode.Success)
```

(See it in action: [guardrail-dev/guardrail-sample-http4s](https://github.com/guardrail-dev/guardrail-sample-http4s), [guardrail-dev/guardrail-sample-sbt-http4s-zio](https://github.com/guardrail-dev/guardrail-sample-sbt-http4s-zio))

Generating clients
==================

As we've seen in [Generating a Server](scala/http4s/README.md#generating-a-server), guardrail-generated servers establish a mapping between our business logic and a cordoned off subset of HTTP. This permits us to focus on our business logic, without getting overloaded with the complexities of managing such a large protocol. The same is true with guardrail generated HTTP Clients: from a consumer's standpoint, HTTP calls should look like regular function calls, accepting domain-specific arguments and producing domain-specific results.

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
