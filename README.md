guardrail [![Build Status](https://travis-ci.com/twilio/swagger-codegen.svg?token=fw5GtT8qEoQYxsq4HYES&branch=master)](https://travis-ci.com/twilio/swagger-codegen)
===

Already onboard? [docs/QUICKSTART.md](./docs/QUICKSTART.md)! Interested in contributing? [docs/CONTRIBUTING.md](./docs/CONTRIBUTING.md)!

The Little Big Book of `guardrail`
===

## Motivation

### Documentation

Documentation is good. Having a common language to express an API specification allows discussion at a high level, and a greater level of visibility into the implementation (architecture/security review), all without looking at the code at all.

### Static analysis

By specifying your protocol completely in a specification, a linter could be used to determine whether or not your new protocol is compatible with existing clients, preventing accidental slip-ups and rework.

### Enforced protocol adherence

With a sufficiently powerful typesystem, code that does not follow the specification should not compile.

### Breaking binary dependence

The fewer binary dependencies between projects, the easier it is to manage so-called "dependency hell". When dealing with libraries internal to a company, this can be avoided with appropriate tooling. When integrating with external libraries, however, there are fewer options available for resolving conflicting dependencies. Generated code only depends on what's already in your project, freeing you up to integrate with as many downstreams as you like, no matter what they are written in.

### Automatic best practices

#### DTO mapping

Boilerplate reduction by automatically mapping requests and responses to Data Transfer Objects (DTOs, Protocols) for requests and responses in both clients and servers.

#### Business logic isolation

Especially considering the akka-http routing DSL, it's very convenient to mix business logic in your routing layer. Despite making this difficult to read, it also poses a business risk. Libraries, styles, and protocols come and go, but the business logic stays largely the same. If unpacking the request/response at the boundary of business logic, you are free to change your implementation at any point (possibly by regenerating your clients or servers with different parameters!)

#### Enhanced testing

Generating downstream services as static HTTP clients, functional tests can be written entirely statically, permitting high parallelism. With many tests, this becomes a significant win, due to removing the need for port binding or maintaining state between tests.

#### Generated boilerplate

'nuff said

#### Automated input validation

By permitting business domain types for input parameters (for example, `AccountSid` vs `String`), services become more resilient to failure or data leaks by doing input validation as soon as possible in the pipeline.

### Generate idiomatic clients per language from the same spec

Generating backend servers and clients from the same specification enables tight (and correct) coupling between services, almost entirely for free.
