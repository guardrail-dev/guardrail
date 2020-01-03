---
layout: docs
title: "guardrail extensions - akka-http - scala - guardrail"
---

guardrail Extensions
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
      <td nowrap><code>x-empty-is-null</code></td>
      <td>boolean</td>
      <td>clients/servers, definitions</td>
      <td>
        Instructs the JSON decoder to convert empty strings to <code>null</code> before
        decoding, causing empty strings to not satisfy the <code>required</code> directive,
        or being represented as <code>None</code> instead of <code>Some("")</code>.
      </td>
    </tr>
    <tr>
      <td nowrap><code>x-file-hash</code></td>
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
        <code>packageName</code> when generating Scala code. This permits
        splitting up large specifications into smaller, domain-specific
        <code>Handler</code>s. See also <code>x-jvm-package</code>.
      </td>
    </tr>
    <tr>
      <td nowrap><code>x-java-package</code></td>
      <td>string</td>
      <td nowrap>clients/servers, paths</td>
      <td>
        A dot-separated package segment concatenated to the end of the supplied
        <code>packageName</code> when generating Java code. This permits
        splitting up large specifications into smaller, domain-specific
        <code>Handler</code>s. See also <code>x-jvm-package</code>.
      </td>
    </tr>
    <tr>
      <td nowrap><code>x-jvm-package</code></td>
      <td>string</td>
      <td nowrap>clients/servers, paths</td>
      <td>
        A dot-separated package segment concatenated to the end of the supplied
        <code>packageName</code> when generating JVM code. This permits
        splitting up large specifications into smaller, domain-specific
        <code>Handler</code>s. Note that <code>x-scala-package</code> and
        <code>x-java-package</code> take precedence over this property.
      </td>
    </tr>
    <tr>
      <td nowrap><code>x-server-raw-response</code></td>
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
      <td nowrap><code>x-tracing-label</code></td>
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
        with a domain-specific type for generated Scala code. This requires the
        type to have either serializers/deserializers in the underlying JSON
        framework or HTTP framework. As this is an advanced feature, it may
        require use of custom <code>imports</code> provided via build tool
        plugins or at the CLI.
      </td>
    </tr>
    <tr>
      <td nowrap><code>x-java-type</code></td>
      <td>string</td>
      <td nowrap>definitions, parameters</td>
      <td>
        Override the primitive types specified in the OpenAPI specification
        with a domain-specific type for generated Java code. This requires the
        type to have either serializers/deserializers in the underlying JSON
        framework or HTTP framework. As this is an advanced feature, it may
        require use of custom <code>imports</code> provided via build tool
        plugins or at the CLI.
      </td>
    </tr>
    <tr>
      <td nowrap><code>x-jvm-type</code></td>
      <td>string</td>
      <td nowrap>definitions, parameters</td>
      <td>
        Override the primitive types specified in the OpenAPI specification
        with a domain-specific type for generated JVM (Scala and Java) code.
        This requires the type to have either serializers/deserializers in the
        underlying JSON framework or HTTP framework. As this is an advanced
        feature, it may require use of custom <code>imports</code> provided via
        build tool plugins or at the CLI.
      </td>
    </tr>
    <tr>
      <td nowrap><code>x-scala-array-type</code>, <code>x-java-array-type</code></td>
      <td>string</td>
      <td nowrap>definitions, parameters</td>
      <td>
        Override the generated array type from `Vector` to some custom type.
        This requires the type to have either serializers/deserializers in the
        underlying JSON framework or HTTP framework. As this is an advanced
        feature, it may require use of custom <code>imports</code> provided via
        build tool plugins or at the CLI.
      </td>
    </tr>
    <tr>
      <td nowrap><code>x-scala-map-type</code>, <code>x-java-map-type</code></td>
      <td>string</td>
      <td nowrap>definitions, parameters</td>
      <td>
        Override the generated array type from `Map` to some custom type.
        This requires the type to have either serializers/deserializers in the
        underlying JSON framework or HTTP framework. As this is an advanced
        feature, it may require use of custom <code>imports</code> provided via
        build tool plugins or at the CLI.
      </td>
    </tr>
  </tbody>
</table>

<span style="float: left">[Prev: Generating clients](generating-clients)</span>
