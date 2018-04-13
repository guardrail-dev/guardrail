Example Usage
===

Pre-requisites:
* Install sbt and have it in your path

Client
---

```
./cli.sh --client --specPath modules/sample/src/main/resources/petstore.json --packageName petstore --outputPath /tmp/petstore-client/src/main/scala
```

Server
---

```
./cli.sh --server --specPath modules/sample/src/main/resources/petstore.json --packageName petstore --outputPath /tmp/petstore-server/src/main/scala
```

Overview
===

- Supported Features
  - akka-http
  - Injection points for OpenTracing (Lightstep) tracers
  - JSON req/resp bodies (based on Circe)
  -  path/form/header parameters
    - Partial Validation
  -  Enumerations
  -  Custom, authed, or signed headers
- Missing Features
  - CSV/XML bodies
  - Automatic JSON streaming
  - Constraint validation
  - Free Handler implementation
  - Domain import configuration
  - SBT plugin
  - Precise imports (DTO)
- Known bugs ([Issue tracker](https://github.com/twilio/guardrail/issues))
  - Duplicated class paths

Workflow
===

- New project
  - Find or write swagger spec file for each downstream service
  - Thread through implicit httpClient from a boundary that makes sense for your project
    - Using the same httpClient permits parallel and static functional tests, and separation of concern for reusing functional tests for integration tests
  - Generate clients
  - Generate server
    - Provide implementations for `Handler` operations
    - Pass `Handler` to `Resource`'s `route`
    - Combine routes via `~`
- Existing project
  - See "Find or write swagger specs"
  - See notes about `httpClient`
  - "Generate clients"
  - Isolate business logic as much as possible to ease migration to generated servers
  - akka-http
    - Swap manual routes with generated ones
