Migrating to 0.62.0
===================

Dependencies
------------

Generated Java code is annotated with `@javax.annotations.Generated` annotation to exclude generated code from test coverage analysis.

For JDK9+ it may be required to add `javax.annotation:javax.annotation-api:1.3.2` dependency to your project. For JDK 8 and below no new dependency is needed.

Configuration
-------------

If you are using guardrail's `module` system (rather than specifying an all-in-one `framework`, you will now need to specify a collections module when building Java projects.  The new options are `java-stdlib` and `java-vavr`.

Migrating to 0.59.0
===================

0.59.0 may contain type and package naming changes that will require changes in consuming code
----------------------------------------------------------------------------------------------

Fixing Java identifier escaping required some core changes in how various names (type, method, method argument, package) are generated.  This cascaded into all of the framework generators, so both Java and Scala clients may be affected.


### Scalafix

There is a Scalafix rule, [`GuardrailScalaResponseTypes`](https://github.com/blast-hardcheese/guardrail-scalafix-rules/blob/master/rules/src/main/scala/fix/GuardrailScalaResponseTypes.scala), that can be used to automate the bulk of these changes.

1. [Install scalafix](https://scalacenter.github.io/scalafix/docs/users/installation#sbt)
1. Invoke scalafix at the SBT console:
    ```
    sbt> scalafix -r https://raw.githubusercontent.com/blast-hardcheese/guardrail-scalafix-rules/master/rules/src/main/scala/fix/GuardrailScalaResponseTypes.scala
    ```
1. If there are issues, please [report them](https://github.com/guardrail-dev/guardrail/issues).
   We do our best to keep breaking changes to a minimum, but as this greatly simplified the casing heuristics, we decided to move forward. Thanks for your understanding.

The most notable differences are:

* The Scala client and server response class types used to be camelCased but are now PascalCased (which now conforms to Scala norms).  Since these types sometimes occur in type signatures of `Handler` classes, some names might need updating.
* Operation IDs with mixed casing are now rewritten to generate nicer names.  For example an operation ID of `fooBar_0` would previously map to a method with the same name, but now the method will be named `fooBar0`.  There is a similar change with package names set using `tags` or `x-jvm-package`.

In general, all names should now conform to the following convention for Java and Scala:

* Package names are camelCased.
* Type (class, interface, trait, enum) names are PascalCased.
* Method names are camelCased.
* Method argument names are camelCased.
* For Java, enum values are UPPER_SNAKE_CASED.  (In Scala, enum values are case objects, which follow the same naming rules as types.)

There is one exception: if generating names in these formats causes a conflict, the original names from the spec will be used if possible.  For example, if an object schema contains two properties, one named `FooBar` and the other named `foo_bar`, guardrail will first try to turn them *both* into `fooBar`.  It will notice the clash and just leave them as `FooBar` and `foo_bar`.

If you find a case where a name is generated in a different way, please file an issue.

Migrating to 0.55.0
===================

0.55.0 may contain breaking changes for Scala clients and servers
-----------------------------------------------------------

The Scala type for OpenAPI's `type: array` has been changed from `IndexedSeq` to `Vector`.

This change was made as `IndexedSeq` has no instances for cats typeclasses, as it is not a real datatype.

To aide migrations, adding `x-scala-array-type: IndexedSeq` to definitions and parameters alongside `type: array` will change the generated type back to what it was before, permitting consumers to upgrade when they have time or to choose different implementations (`List`, `NonEmtyVector`, `Chain`, etc) to provide different semantics.

Migrating to 0.54.0
===================

0.54.0 introduces a module system to compensate for breaking changes in underlying libraries
--------------------------------------------------------------------------------------------

circe 0.12 broke backwards compatibility in two ways:
- no longer publishing the circe-java8 stub library for 2.12, as it was merged into the core and the java8 implicits are just built in now
- moving from the old naming convention of `ObjectEncoder[?]` to the new `Encoder.AsObject[?]`

If you need to use old versions of circe, you may want one or the other of these two configurations:

_does not include the io.circe.java8.\_ import_
```diff
 guardrailTasks in Compile := List(
-  ScalaServer(file("spec.json"), pkg="com.example", framework="akka-http"),
+  ScalaServer(file("spec.json"), pkg="com.example", modules=List("akka-http", "circe-0.11")),
 )
```

_implies circe-0.11_
```diff
 guardrailTasks in Compile := List(
-  ScalaServer(file("spec.json"), pkg="com.example", framework="akka-http"),
+  ScalaServer(file("spec.json"), pkg="com.example", modules=List("akka-http", "circe-java8")),
 )
```

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
