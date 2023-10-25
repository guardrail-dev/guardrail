---
layout: docs
title: "Encoding - akka-http - scala - guardrail"
---

guardrail will generate data transfer objects with encoders and decoders for entity bodies of requests and responses, as long as they're specified by a `$ref` reference to either in a components section or in a separate file.

Data transfer objects will be represented as case classes, while encoders and decoders depend on the framework used. For dropwizard for example, guardrail will generate jackson encoders and decoders, while for http4s, guardrail will create encoders and decoders for circe.

When your schemas are defined inline however, guardrail will not build typed DTOs for the schemas, but fall back to a generic json representation. One scenario where this can happen is when your api specification is built as a bundle with swagger-cli. Fortunately, guardrail understands the unbundled representation with ref elements to separate files.
