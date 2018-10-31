v0.40.0
=======

- #43 Missing Support for ComposedModels
- #89 Submitting a list of a form parameter is not supported
- #122 Guardrail doesn't support Array type for Form Params

v0.39.0
=======

- #102 http4s server generation

v0.38.0
=======

- #110 File Accumulator leak causes missing files on filesystem on name collision
- #101 Exposing more error information to build tooling
- #111 Bumping scalameta
- #112 Making `x-scala-type` work for all non-container types instead of just string

v0.37.1
=======

- #88 Adds support for other encoding charsets for JSON payloads
- #88 Using a plain unmarshaller for text/plain
- #88 Removing jsonFSU from clients in favor of the ones in AkkaImplicits
- #88 Returning `NoContextException` from failed unmarshallers instead of the Circe exceptions so we can try the next available unmarshaller

v0.37.1-M1
==========

- First release on Maven Central

v0.37.0
=======

- #74 Adding `Show[java.net.URL]`
- #79 Supporting custom header types (previously only string types were supported)
- #78 Initial version of the [book.md](./docs/book.md)
- #83 Bumping scalameta to get proper backtick escaping, removing backtick escaping hacks

v0.37.0-M1
==========

- Bumping scalameta to get proper backtick escaping, removing backtick escaping hacks
- Abstracting `makeRequest` from `wrap` for clients
- Fixing `text/plain` support, adding a round-trip test
- Consolidating FSUs to be less confusing

v0.36.1
=======

- #73 Fixing multipart decoder for non-string types

v0.36.0
=======

- #62 Bumping http4s
- #63 Bumping circe
- #66 Async multipart form file upload

v0.35.0
=======

- #57 scala 2.12.6
- #59 text/plain support, root path matching bug in servers

v0.34.1
=======

- #56 Altering missing specification file warning
- #55 Fixing NPE

v0.34.0
=======

- #30 Supporting complex server paths
- #32 Initial http4s client generation
- #39 Upgrading dependencies
- #46 Repackaging under `com.twilio.guardrail`
- #47 First bintray release

v0.33.0
=======

- #29 Extending ref unrolling to more types

v0.32.0
=======

- Empty release

v0.31.0
=======

- #28 Unrolling type references, getting rid of `type Foo = Bar` hacks

v0.30.3
=======

- #27 Bugfix to properly encode enumerations in servers

v0.30.1
=======

- #26 Support for enumerations in servers

v0.30.0
=======

- #20 Permitting multiple response types from the server generator
- #22 Adding support for `type: array` definitions
- #23 Adding server raw response escape hatch

v0.29.0
=======

- #11 Upgrade scalameta
- #16 Fixing entity marshalling
- #18 Resolving duplicate parameter names

v0.28.0
=======

- Upgrading scalameta
- Fixing crosspublishing

v0.27.0
=======

- #1 Fix for some terms being double escaped
- #2 Permitting CLI deduplication via --default
- #3 Exposing API for build tools
- #4 `$ref` structures are not permitted for vendor extensions, as per the spec
- #5 Avoiding namespace collisions by referencing enum members as properties of their companion object
- #6, #10 Rewriting the regex URL parser in atto to permit interpolation into URL segment templates (eg: "{file}.txt")
- #7 Permitting extensibility via injecting custom imports into generated code
- #8 2.12 cross-compilation
- #13 Fix for missing JSON bodies

v0.26.0
=======

- First limited public release
