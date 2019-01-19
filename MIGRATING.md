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

Unspecified status codes (3xx, 5xx) will still be passed through to the error result.
