---
layout: docs
title: "Sample API Specification - java - dropwizard - guardrail"
---

Sample API specification
========================

The following is a complete, annotated OpenAPI specification file: (guardrail extensions are documented in [guardrail Extensions](../../scala/akka-http/guardrail-extensions))


```yaml
openapi: 3.0.0                                       # Version of Swagger/OpenAPI of this file
info:                                                # Primarily for consumption by documentation generation tools
  title: My Service
  version: 0.1.0
servers:                                             # List of hosts (and ports) where generated clients should connect to
  - url: http://localhost:1234
paths:                                               # All HTTP Paths are children of this `paths` field
  "/user/{id}":                                      # Paths can have variables in them
    get:                                             # HTTP Method
      operationId: getUser                           # Friendly name, will be the method name in generated clients and servers
      x-jvm-package: users                           # Relative package for this client to live in
      parameters:                                    # All parameters (including path parameters) are listed here.
        - name: id                                   # Field name, case sensitive
          in: path                                   # where the parameter will be found.
          description: The ID of the user            # description of the parameter. Not used in guardrail, but other tools may consume it
          required: true                             # required fields can not be missing. Optional fields will become `Optional<T>`
          schema:
            type: string                             # One of the primitive types supported in the OpenAPI specification.
          x-java-type: CustomString                  # Escape hatch to explicitly introduce a custom type. This is an
                                                     # advanced technique to introduce completely custom
                                                     # marshalling/unmarshalling/validation logic. Keep in mind, everyone
                                                     # else will just see a plain string!

      responses:                                     # All response codes that are possible for this path are listed here
        "200":                                       # Each HTTP status code is mapped to the corresponding textual representation
          description: ""
          content:
            "application/json":                      # the Content-Type of the return value. Java guardrail will use jackson for converting json
              schema:
                $ref: "#/components/schemas/User"    # a reference to elsewhere in this spec for a definition of the response body
        "404":                                       # failure responses must be declared
          description: Not found                     # description isn't used by guardrail but is useful to indiciate we return no body for this response
components:                                          # reusable sections of the spec can be described under `components`
  schemas:
    User:                                            # This identifies a symbolic structure name. Not all names are
                                                     # translated into classes when rendered, depending on whether they
                                                     # identify classes with structure, or defer to standard classes
                                                     # like `Vector` for `type: array`.
      type: object                                   # will generate a `User` case class in the `definitions` package
      required:                                      # A list of which parameters are required. This is enforced for
                                                     # clients by having non-optional parameters, and for servers by
                                                     # ensuring all submitted data to the endpoint validates the schema
                                                     # before getting to your `Handler` function.
        - id                                         # These names must match the `properties` names exactly
        - user_addresses
      properties:
        id:                                          # Case matters for `properties`! A heuristic determines whether it's
                                                     # possible to translate a property name into a unique, non-reserved
                                                     # camelCase identifier.
          type: string                               # One of the primitive types supported in the OpenAPI specification.
        user_addresses:                              # Similar to `id`, though `user_addresses` can be safely transformed into
                                                     # `userAddress`, so this is done to expose idiomatic Java. The underlying
                                                     # marshallers and unmarshallers maintain this mapping for you though,
                                                     # so no chance of protocol violations.
          $ref: "#/components/schemas/UserAddresses" # Ensures that the type of `userAddress` will be `List<UserAddress>`
    UserAddresses:
      type: array
      items:
        $ref: "#/components/schemas/UserAddress"     # `items` is a special key for `type: array`, identifying the structure of the
                                                     # sequence members
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


<span style="float: left">[Prev: Installation](installation)</span>
<span style="float: right">[Next: Generating a Server](generating-a-server)</span>
