## akka-http Jackson

The `akka-http-jackson` framework generates the same server and client
APIs as the [akka-http](../akka-http) framework, except that Jackson is
used for serialization and deserialization (instead of Circe).

Usage-wise, it is very similar to `akka-http`.  Extra information
specific to this framework can be found below.

### guardrail Configuration

You can either specify the `akka-http-jackson` framework, or select the
`akka-http` and `jackson` modules.

### Prerequisites

In addition to the akka-http dependencies, you'll need the following:

* `com.fasterxml.jackson.core:jackson-core`
* `com.fasterxml.jackson.core:jackson-databind`
* `com.fasterxml.jackson.core:jackson-annotations`
* `com.fasterxml.jackson.datatype:jackson-datatype-jsr310` (only if date
  and date-time types are used in your spec)
* `com.fasterxml.jackson.module:jackson-module-scala_${scala.compat.version}`
* `org.hibernate.validator:hibernate-validator` (or
  `org.hibernate:hibernate-validator`)

While testing has been done with Jackson 2.11.x, older 2.x versions
should work as well.  Hibernate Validator 5.4.x and 6.0.x have been
tested and appear to work.

### Server and Client Construction

When constructing your server routes or client instances, you'll need
implicit `ObjectMapper` (Jackson) and `Validator` (Hibernate) instances
available.

### `ObjectMapper`

You must register `DefaultScalaModule` on your object mapper.  If your
spec uses date or date-time string formats, you must also register
`JavaTimeModule`.

### `Validator`

The validator can [be constructed](https://docs.jboss.org/hibernate/validator/5.4/reference/en-US/html_single/#section-retrieving-validator-factory-validator)
in whatever manner you desire.  Hibernate is recommended, but guardrail
does not currently use any Hibernate-specific annotations; however, it
may do so in the future as more strict validations are implemented.

## Caveat

In general, using the Circe-backed `akka-http` is recommended if you
have a choice.  However, if you're required to use Jackson for some
reason, this framework is available.  It's also useful if you have a
Scala project that needs protocol/model objects, and you need to use
Jackson.
