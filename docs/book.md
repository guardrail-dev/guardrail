Table of Contents
=================

1. [What is guardrail](#what-is-guardrail)
   1. [Single Point of Truth](#single-point-of-truth)
   1. [Unexpected API changes are compiler errors](#unexpected-api-changes-are-compiler-errors)
   1. [Fewer binary dependencies](#fewer-binary-dependencies)
1. [Installation](#installation)
1. [Sample API specification](#sample-api-specification)
1. [Generating a Server](#generating-a-server)
   1. [Separation of business logic](#separation-of-business-logic)
   1. [API structure slip is impossible](#api-structure-slip-is-impossible)
   1. [Generating test-only (real) server mocks for unit tests](#generating-test-only-real-server-mocks-for-unit-tests)
   1. [A note about scalatest integration](#a-note-about-scalatest-integration)
1. [Generating clients](#generating-clients)
   1. [Separation of protocol-concerns from API-level concerns](#separation-of-protocol-concerns-from-api-level-concerns)
1. [Guardrail Extensions](#guardrail-extensions)

What is guardrail?
==================

Guardrail is a code generation tool, capable of reading from OpenAPI/Swagger specification files and generating Scala source code, primarily targeting the akka-http and http4s web frameworks, using circe for JSON encoding/decoding.

Guardrail has three primary goals:

- Documentation: Single point of truth for the interface to a software system
- Better Servers: Unexpected API changes surface as compiler errors via server routing layer code generation
- Better Clients: Fewer binary dependencies via client library code generation

Describing software is tricky. Incomplete specifications, slippage between specification and implementation, or even additional semantics of infrastructure that aren't easily communicated through static documents; these are only a few challenges you'll face when attempting to write a specification for your API. A reasonable question you may be asking is what motivations are there for going through these cumbersome and often frustrating tasks? We'll investigate some answers to this question in the following sections.

Single Point of Truth
---------------------

By describing the shape of an API statically, there are far fewer variables to worry about. HTTP is a _very_ flexible protocol, with many features. By constraining that protocol to a subset that expresses the interface to our server (or service, or microservice), we drastically reduce the burden of handling the entirety of HTTP to the core terms of our API. Focus on semantics of APIs once the basics (routing, data validation) are figured out.

A secondary benefit of static specifications lies in tooling. Hand-written routing logic can hide security holes, miss best practices, and obscure intent if written incorrectly. This problem is multipled across as many different languages as are supported inside any given company, manifesting as wasted effort implementing the same feature in different languages, or a bug that only occurs 10 percent of the time due to a buggy golang client.

Attempting to derive what the attack surface of a server is from the implementation is often the job of entire teams in large companies, and even that may not be enough. Conversely, with a static specification, those teams can build intelligent traffic analysis tools to detect anomalies or intentionally malicious clients built to inject bad data to find bugs.

Unexpected API changes are compiler errors
------------------------------------------

Once we have a specification, generating traits (or abstract classes) with unimplemented members gives us another powerful tool: New or changed parameters become compiler errors.

After constraining our vocabulary to a subset of HTTP that serves our business need, even saying "This parameter is optional" forces us to contend with the sudden appearance of `Option[T]` parameters in our generated `Handler` methods.

Once specified, `-Ywarn-unused` helpfully points out that we've forgotten to reflect this most recent change in our tests. A win on both fronts!

Fewer binary dependencies
----------------------

Traditionally written and maintained client libraries invariably accumulate cruft. In many cases, this is intended to be helpful: papering over a poorly designed API by providing custom logic, renaming parameters to be more convenient, or including properly configured HTTP clients that express retry and backoff semantics the library author provided based on the business requirements known at the time of writing.

Altering the shape of an API by providing a thick HTTP client reduces the shared terminology between service maintainers and their consumers, or even between consumers coming from different languages.

Additionally, by hardcoding even a well-behaved HTTP client into a client library, now consumers are forced to work around that dependency. This may manifest as learning how to use and configure a brand new HTTP client under time pressure, or writing and maintaining brittle [Application Binary Interface (ABI)](https://en.wikipedia.org/wiki/Application_binary_interface)-compatible adapter layers that attempt to use the configuration already present in the rest of the codebase.

Once these bespoke HTTP client configurations are built, both they and their dependencies are now added to the grab bag of dependency versions that must be maintained through the life of any given piece of infrastructure. This presents hidden barriers for upgrading all dependencies, as the possibility of transitive dependency conflicts increase as dependency trees become deeper.

Installation
============

Guardrail is available as a modular core, with both [sbt](https://github.com/twilio/sbt-guardrail) and [Maven](https://github.com/twilio/guardrail-maven-plugin) integration. The core can also be run as a stand-alone [CLI](https://github.com/twilio/guardrail/blob/978a92db3dd46812aa19f05050995f864cbb5bb3/build.sbt#L33-L48) application, with full support for all features.

If compiling with Scala < 2.13.x, you'll need to enable `-Ypartial-unification`:

```scala
scalacOptions += "-Ypartial-unification"
```

Sample API specification
========================

The following is a complete, annotated OpenAPI specification file: (guardrail extensions are documented in [Guardrail Extensions](#guardrail-extensions))

```yaml
swagger: "2.0"                              # Which version of the OpenAPI/Swagger specification we are following
info:                                       # Primarily for consumption by documentation generation tools
  title: My Service
  version: 0.1.0
host: localhost:1234                        # Default host (and optional port) to connect to for generated clients
schemes:
  - http
paths:                                      # All HTTP paths are direct children of the `paths` field

  /user/{id}:                               # Paths can have variable patterns in paths

    get:                                    # HTTP method

      operationId: getUser                  # Friendly name, ends up as the function name (in clients and servers)

      x-scala-package: users                # Relative package for this client to live in. For convenience, the
                                            # last package parameter is turned into a class name for clients and
                                            # servers. In this case, `UsersClient`.

      parameters:                           # All parameters (including path parameters) are listed here.

      - name: id                            # The field name (case matters!), used to both identify the correct
                                            # field to match, as well as generate a best-guess idiomatic Scala
                                            # parameter name.

        in: path                            # Where to look for the parameter

        required: true                      # Required fields cannot be missing. `required: false` fields are
                                            # represented as `Option[T]`

        type: string                        # One of the primitive types supported in the OpenAPI specification.
                                            # https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#dataTypes

        x-scala-type: CustomString          # Escape hatch to explicitly introduce a custom type. This is an
                                            # advanced technique to introduce completely custom
                                            # marshalling/unmarshalling/validation logic. Keep in mind, everyone
                                            # else will just see a plain string!

      responses:                            # All response codes that are possible are listed here

        200:                                # Each HTTP status code is mapped to the corresponding textual
                                            # representation in guardrail-generated servers.

          schema:                           # The optional `schema` parameter describes what's possible to return
                                            # as the body of a response

            $ref: '#/definitions/User'      # In the generated `UsersHandler` `getUser` function, we can call
                                            # `respond.OK(user)`, where `user: definitions.User`

        404:                                # We must represent our failure cases as well, otherwise we can
                                            # never express failure!

          description: Not found            # The optional `description` parameter is not used in guardrail,
                                            # but is useful here as an indicator that we don't have a response
                                            # body for `404 Not Found` responses.

definitions:                                # All non-primitive structures are defined inside `definitions`

  User:                                     # This identifies a symbolic structure name. Not all names are
                                            # translated into classes when rendered, depending on whether they
                                            # identify classes with structure, or defer to standard classes
                                            # like `IndexedSeq` for `type: array`.

    type: object                            # will generate a `User` case class in the `definitions` package

    required:                               # A list of which parameters are required. This is enforced for
                                            # clients by having non-optional parameters, and for servers by
                                            # ensuring all submitted data to the endpoint validates the schema
                                            # before getting to your `Handler` function.

      - id                                  # These names must match the `properties` names exactly
      - user_addresses

    properties:                             # `object`s are permitted to have `properties`. These are translated
                                            # into fields in the generated case classes.

      id:                                   # Case matters for `properties`! A heuristic determines whether it's
                                            # possible to translate a property name into a unique, non-reserved
                                            # camelCase identifier.

        type: string                        # One of the primitive types supported in the OpenAPI specification.
                                            # https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#dataTypes

      user_addresses:                       # Similar to `id`, though `user_addresses` can be safely transformed into
                                            # `userAddress`, so this is done to expose idiomatic Scala. The underlying
                                            # marshallers and unmarshallers maintain this mapping for you though,
                                            # so no chance of protocol violations.

        $ref: '#/definitions/UserAddresses' # Ensures that the type of `userAddress` will be `IndexedSeq[UserAddress]`

  UserAddresses:
    type: array

    items:                                  # `items` is a special key for `type: array`, identifying the structure of the
                                            # sequence members

      $ref: '#/definitions/UserAddress'     # Primitive types could be listed here, but as we're referring to another class,
                                            # we need to explicitly use a `$ref`. This may change in the future,
                                            # see https://github.com/twilio/guardrail/issues/76

  UserAddress:
    type: object
    properties:
      line1:
        type: string
      line2:
        type: string
      line3:
        type: string
```

Generating a Server
===================

Guardrail-generated servers come in two parts: a `Resource` and a `Handler`. The `Resource` contains all the routing logic, accepting a `Handler` as an argument to the `route` function in order to provide an HTTP service in whichever supported HTTP framework you're hosting your service in.

The following is an example from the [akka-http](https://github.com/akka/akka-http) server generator:

```scala
// The `Handler` trait is fully abstracted from the underlying http framework. As a result, with the exception of some
// structural alterations (`F[_]` instead of `Future[_]` as the return type) the same handlers can be used with
// different `Resource` implementations from different framework generators. This permits greater compatibility between
// different frameworks without changing your business logic.
trait UserHandler {
  def createUser(respond: UserResource.createUserResponse.type)(body: User): scala.concurrent.Future[UserResource.createUserResponse]
  def createUsersWithArrayInput(respond: UserResource.createUsersWithArrayInputResponse.type)(body: IndexedSeq[User]): scala.concurrent.Future[UserResource.createUsersWithArrayInputResponse]
  def createUsersWithListInput(respond: UserResource.createUsersWithListInputResponse.type)(body: IndexedSeq[User]): scala.concurrent.Future[UserResource.createUsersWithListInputResponse]
  def loginUser(respond: UserResource.loginUserResponse.type)(username: String, password: String): scala.concurrent.Future[UserResource.loginUserResponse]
  def logoutUser(respond: UserResource.logoutUserResponse.type)(): scala.concurrent.Future[UserResource.logoutUserResponse]
  def getUserByName(respond: UserResource.getUserByNameResponse.type)(username: String): scala.concurrent.Future[UserResource.getUserByNameResponse]
  def updateUser(respond: UserResource.updateUserResponse.type)(username: String, body: User): scala.concurrent.Future[UserResource.updateUserResponse]
  def deleteUser(respond: UserResource.deleteUserResponse.type)(username: String): scala.concurrent.Future[UserResource.deleteUserResponse]
}
object UserResource {
  def routes(handler: UserHandler)(implicit mat: akka.stream.Materializer): Route = {
    (post & path("v2" / "user") & entity(as[User])) {
      body => complete(handler.createUser(createUserResponse)(body))
    } ~ (post & path("v2" / "user" / "createWithArray") & entity(as[IndexedSeq[User]])) {
      body => complete(handler.createUsersWithArrayInput(createUsersWithArrayInputResponse)(body))
    } ~ (post & path("v2" / "user" / "createWithList") & entity(as[IndexedSeq[User]])) {
      body => complete(handler.createUsersWithListInput(createUsersWithListInputResponse)(body))
    } ~ (get & path("v2" / "user" / "login") & (parameter(Symbol("username").as[String]) & parameter(Symbol("password").as[String])) & discardEntity) {
      (username, password) => complete(handler.loginUser(loginUserResponse)(username, password))
    } ~ (get & path("v2" / "user" / "logout") & discardEntity) {
      complete(handler.logoutUser(logoutUserResponse)())
    } ~ (get & path("v2" / "user" / Segment) & discardEntity) {
      username => complete(handler.getUserByName(getUserByNameResponse)(username))
    } ~ (put & path("v2" / "user" / Segment) & entity(as[User])) {
      (username, body) => complete(handler.updateUser(updateUserResponse)(username, body))
    } ~ (delete & path("v2" / "user" / Segment) & discardEntity) {
      username => complete(handler.deleteUser(deleteUserResponse)(username))
    }
  }
  ...
}
```

As all parameters are provided as arguments to the function stubs in the trait, there's no concern of forgetting to extract a query string parameter, introducing a typo in a form parameter name, or forgetting to close the bytestream for the streaming HTTP Request.

Separation of business logic
----------------------------

Providing an implementating of a function with a well-defined set of inputs and outputs is natural for any developer. By reducing the scope of the interface a developer writes against, implementations are more clear and concise.

Furthermore, by providing business logic as an implementation of an abstract class, unit tests can test the routing layer and business logic independently, by design.

API structure slip is impossible
--------------------------------

As parameters are explicitly provided as arguments to functions in `Handler`s, any alteration to parameters constitute a new function interface that must be implemented. As a result, if providing an implementation for an externally managed specification, the compiler informs when a previously written function is no longer sufficient.

By representing different response codes and structures as members of a sealed trait, it's impossible to return a structure that violates the specification, even for less frequently used response codes.

Finally, describing an endpoint in your specification without providing an implementation for it is a compiler error. This prevents reduction of functionality due to refactors, human error, or miscommunication with other teams.

Generating test-only (real) server mocks for unit tests
-------------------------------------------------------

Often, we'll also want to have mock HTTP clients for use in unit tests. Mocking requires stringent adherence to the specification, otherwise our mock clients are unrepresentative of the production systems they are intending to mock. The following is an example of a "mock" HTTP Client generated by guardrail; it speaks real HTTP, though doesn't need to bind to a port in order to run. This permits parallelized tests to be run without concern of port contention.

```scala
val userRoutes: Route = UserResource.routes(new UserHandler {
  override def getUserByName(respond: UserResource.getUserByNameResponse.type)(username: String): scala.concurrent.Future[UserResource.getUserByNameResponse] = {
    if (username == "foo") {
      Future.successful(respond.OK(User(id=Some(1234L), username=Some("foo"))))
    } else {
      Future.successful(respond.NotFound)
    }
  }
})
val userHttpClient: HttpRequest => Future[HttpResponse] = Route.asyncHandler(userRoutes)
val userClient: UserClient = UserClient.httpCLient(userHttpClient)
val getUserResponse: EitherT[Future, Either[Throwable, HttpResponse], User] = userClient.getUserByName("foo")
val user: User = getUserResponse.value.futureValue.right.value // Unwraps `User(id=Some(1234L), username=Some("foo"))` using scalatest's `ScalaFutures` and `EitherValues` unwrappers.
```

This strategy of mocking ensures we follow the spec, even when the specification changes. This means not only more robust tests, but also tests that communicate failures via compiler errors instead of at runtime. Having a clear separation of where errors can come from permits trusting our tests more. If the tests compile, any and all errors that occur are in the domain of business logic.

One other strategy for testing non-guardrail generated clients is to bind `userRoutes` from above to a port, run tests that use hand-rolled or vendor-supplied HTTP clients, then unbind the port when the test ends:

```scala
val binding: ServerBinding =
  Http().bindAndHandle(userRoutes, "localhost", 1234).futureValue

// run tests

binding.unbind().futureValue
```

A note about scalatest integration
----------------------------------

### akka-http

The default `ExceptionHandler` in akka-http swallows exceptions, so if you intend to `fail()` tests from inside guardrail-generated HTTP Servers, you'll likely want to have the following implicit in scope:

```scala
implicit def exceptionHandler: ExceptionHandler = new ExceptionHandler {
  def withFallback(that: ExceptionHandler): ExceptionHandler = this
  def seal(settings: RoutingSettings): ExceptionHandler = this

  def isDefinedAt(error: Throwable) = error.isInstanceOf[org.scalatest.TestFailedException]
  def apply(error: Throwable) = throw error
}
```

This passes all `TestFailedExceptions` through to the underlying infrastructure. In our tests, when we call:

```scala
val userClient: UserClient = UserClient.httpCLient(userHttpClient)
val getUserResponse: EitherT[Future, Either[Throwable, HttpResponse], User] = userClient.getUserByName("foo")
val user: User = getUserResponse.value.futureValue.right.value
```

`futureValue` will raise the `TestFailedException` with the relevant stack trace.

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
  def createUser(body: User, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
    val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
    makeRequest(HttpMethods.POST, host + basePath + "/user", allHeaders, body, HttpProtocols.`HTTP/1.1`).flatMap(req => wrap[IgnoredEntity](httpClient, req))
  }
  def createUsersWithArrayInput(body: IndexedSeq[User], headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = ...
  def createUsersWithListInput(body: IndexedSeq[User], headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = ...
  def loginUser(username: String, password: String, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], String] = ...
  def logoutUser(headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = ...
  def getUserByName(username: String, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], User] = ...
  def updateUser(username: String, body: User, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = ...
  def deleteUser(username: String, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = ...
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

Guardrail Extensions
====================

Guardrail has [a number of vendor extensions](https://github.com/twilio/guardrail/blob/cbf9acd9e8ff226cc0f4bbf2f278669071126d5e/modules/codegen/src/main/scala/com/twilio/guardrail/extract/package.scala) designed to enhance safety and provide more idiomatic generated code. The following table lists all vendor extensions, contexts where they are applicable, and a short description of how to use them effectively.

<table>
  <thead>
    <tr>
      <th>Extension</th>
      <th>Type</th>
      <th>Contexts</th>
      <th>Description</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td nowrap><code>x-scala-empty-is-null</code></td>
      <td>boolean</td>
      <td>clients/servers, definitions</td>
      <td>
        Instructs the JSON decoder to convert empty strings to <code>null</code> before
        decoding, causing empty strings to not satisfy the <code>required</code> directive,
        or being represented as <code>None</code> instead of <code>Some("")</code>.
      </td>
    </tr>
    <tr>
      <td nowrap><code>x-scala-file-hash</code></td>
      <td>string</td>
      <td nowrap>servers, parameters, file</td>
      <td>
        During a streaming file upload, keep track of the file hash in one of
        the <a href="https://docs.oracle.com/javase/8/docs/technotes/guides/security/StandardNames.html#MessageDigest">supported file hash types</a>.
      </td>
    </tr>
    <tr>
      <td nowrap><code>x-scala-package</code></td>
      <td>string</td>
      <td nowrap>clients/servers, paths</td>
      <td>
        A dot-separated package segment concatenated to the end of the supplied
        <code>packageName</code>. This permits splitting up large specifications
        into smaller, domain-specific <code>Handler</code>s.
      </td>
    </tr>
    <tr>
      <td nowrap><code>x-scala-raw-response</code></td>
      <td>boolean</td>
      <td nowrap>servers, paths</td>
      <td>
        Exposes the underlying HTTP framework's response-building
        infrastructure. Type-safe `respond` wrappers are still generated
        and supplied, though this escape-hatch is intended to work around
        bugs in guardrail itself. This is not recommended for long-term use,
        as no guarantees around compile-time-safe protocol adherence can be
        made.
      </td>
    </tr>
    <tr>
      <td nowrap><code>x-scala-tracing-label</code></td>
      <td>string</td>
      <td nowrap>clients/servers, paths</td>
      <td>
        When <code>tracing</code> is enabled, override the provided function
        label with a custom string. This string will be supplied to your
        supplied <code>trace</code> function in your servers and your supplied
        <code>traceBuilder</code> in your clients.
      </td>
    </tr>
    <tr>
      <td nowrap><code>x-scala-type</code></td>
      <td>string</td>
      <td nowrap>definitions, parameters</td>
      <td>
        Override the primitive types specified in the OpenAPI specification
        with a domain-specific type. This requires the type to have either
        serializers/deserializers in the underlying JSON framework or
        HTTP framework. As this is an advanced feature, it may require use of
        custom <code>imports</code> provided via build tool plugins or at
        the CLI.
      </td>
    </tr>
  </tbody>
</table>
