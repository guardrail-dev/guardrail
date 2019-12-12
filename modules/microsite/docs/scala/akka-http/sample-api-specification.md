---
layout: docs
title: "Sample API Specification - akka-http - scala - guardrail"
---

Sample API specification
========================

The following is a complete, annotated OpenAPI specification file: (guardrail extensions are documented in [guardrail Extensions](#guardrail-extensions))

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

      x-jvm-package: users                  # Relative package for this client to live in. For convenience, the
                                            # last package parameter is turned into a class name for clients and
                                            # servers. In this case, `UsersClient`.

      parameters:                           # All parameters (including path parameters) are listed here.

      - name: id                            # The field name (case matters!), used to both identify the correct
                                            # field to match, as well as generate a best-guess idiomatic Scala
                                            # parameter name.

        in: path                            # Where to look for the parameter
        
        description: The ID of the user     # The optional `description` parameter is not used in guardrail,
                                            # but is useful for providing a detailed explanation on what is
                                            # expected as a value for the parameter. For example: 
                                            # `description: User IDs are strings comprised of the concatenation 
                                            # of the two upper-case letters ID and a UUID stripped of any dashes
                                            # i.e. ID4d9b1c54e4664c9d92aba94151a7f59f`

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
                                            # like `Vector` for `type: array`.

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

        $ref: '#/definitions/UserAddresses' # Ensures that the type of `userAddress` will be `Vector[UserAddress]`

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

<span style="float: left">[Prev: Installation](installation)</span>
<span style="float: right">[Next: Generating a Server](generating-a-server)</span>
