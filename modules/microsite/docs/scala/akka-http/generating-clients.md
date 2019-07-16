---
layout: docs
title: "Generating Clients - akka-http - scala - guardrail"
---

Generating clients
==================

As we've seen in [Generating a Server](), guardrail-generated servers establish a mapping between our business logic and a cordoned off a subset of HTTP. This permits us to focus on our business logic, without getting overloaded with the complexities of managing such a large protocol. The same is true with guardrail generated HTTP Clients: from a consumer's standpoint, HTTP calls should look like regular function calls, accepting domain-specific arguments and producing domain-specific results.

By generating minimal clients that only have enough business knowledge to map domain types to and from HTTP, opportunities for logical errors are effecitvely removed. While this does not eliminate logical errors entirely, establishing a firm boundary between the underlying protocol and hand-written code drastically reduces the scope of possible bugs.

The following is an example from the [akka-http](https://github.com/akka/akka-http) client generator:

```scala
// Two constructors are provided, one accepting the `httpClient`,
// `ExecutionContext`, and `Materializer` implicitly, the other accepting
// an explicit `httpClient`, but still accepting the `ExecutionContext` and
// `Materializer` as implicits.
object UserClient {
  def apply(host: String = "http://petstore.swagger.io")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer): UserClient =
    new UserClient(host = host)(httpClient = httpClient, ec = ec, mat = mat)
  def httpClient(httpClient: HttpRequest => Future[HttpResponse], host: String = "http://petstore.swagger.io")(implicit ec: ExecutionContext, mat: Materializer): UserClient =
    new UserClient(host = host)(httpClient = httpClient, ec = ec, mat = mat)
}
class UserClient(host: String = "http://petstore.swagger.io")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer) {
  val basePath: String = "/v2"
  def createUser(body: User, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
    val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
    makeRequest(HttpMethods.POST, host + basePath + "/user", allHeaders, body, HttpProtocols.`HTTP/1.1`).flatMap(req => wrap[IgnoredEntity](httpClient, req))
  }
  def createUsersWithArrayInput(body: IndexedSeq[User], headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = ...
  def createUsersWithListInput(body: IndexedSeq[User], headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = ...
  def loginUser(username: String, password: String, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], String] = ...
  def logoutUser(headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = ...
  def getUserByName(username: String, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], User] = ...
  def updateUser(username: String, body: User, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = ...
  def deleteUser(username: String, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = ...
}
```

Separation of protocol-concerns from API-level concerns
-------------------------------------------------------

As guardrail clients are built ontop of the function type `HttpRequest => Future[HttpResponse]`, client configuration is reduced to function composition. Some ideas:

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
