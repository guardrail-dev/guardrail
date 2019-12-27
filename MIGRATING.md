Migrating to 0.55.0
===================

0.55.0 may contain breaking changes for Scala clients and servers
-----------------------------------------------------------

The Scala type for OpenAPI's `type: array` has been changed from `IndexedSeq` to `Vector`.

This change was made as `IndexedSeq` has no instances for cats typeclasses, as it is not a real datatype.

To aide migrations, adding `x-scala-array-type: IndexedSeq` to definitions and parameters alongside `type: array` will change the generated type back to what it was before, permitting consumers to upgrade when they have time or to choose different implementations (`List`, `NonEmtyVector`, `Chain`, etc) to provide different semantics.

Migrating to 0.49.0
===================

0.49.0 may contain breaking changes for all clients and servers
---------------------------------------------------------------

The case converters (the code that takes identifiers and turns them into camelCase, snake_case, etc.) has been rewritten to fix bugs and inconsistencies. As a result, some identifiers (parameter, property, class, etc. names) may be slightly different from what they were before and will need to be updated.

This is especially true in Scala code, where invalid identifiers were simply backtick-escaped.  For example, the following specification:

```
parameters:
  - name: X-Foo-Bar
    in: header
    type: string
```

which defines a header method parameter, would have generated a method signature such as:

```
def someMethod(`x-Foo-Bar`: Option[String])
```

With this change, the parameter name will be the more idiomatic (and backtick-free) `xFooBar`.

Migrating to 0.42.0
===================

0.42.0 contains a breaking change for akka-http clients
-------------------------------------------------------

Previously, client methods naively picked the "best" success schema, applying that to all success statuses, and did not attempt to handle non-success statuses.

```scala
addressesClient
  .getAddress(traceBuilder, "addressId")
  .fold(_ => respond.NotFound, { address =>
    respond.OK(sdefs.User("1234", sdefs.UserAddress(address.line1, address.line2, address.line3)))
  })
```
_(example taken from modules/sample/src/test/scala/core/AkkaHttp/AkkaHttpFullTracerTest.scala)_

Now, it is explicitly required to handle all response statuses, either by using the mapped names directly:

```scala
addressesClient
  .getAddress(traceBuilder, "addressId")
  .fold(_ => respond.NotFound, {
    case GetAddressResponse.OK(address) => respond.OK(sdefs.User("1234", sdefs.UserAddress(address.line1, address.line2, address.line3)))
    case GetAddressResponse.NotFound => respond.NotFound
  })
```

... or by using the `fold` convenience method:

```scala
addressesClient
  .getAddress(traceBuilder, "addressId")
  .fold(_ => respond.NotFound, _.fold(
    handleOK = address => respond.OK(sdefs.User("1234", sdefs.UserAddress(address.line1, address.line2, address.line3)))
    handleNotFound = respond.NotFound
  ))
```

Unspecified status codes will still be passed through to the error result, so you can still use your existing 3xx or 5xx handlers.
