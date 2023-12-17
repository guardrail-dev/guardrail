What is guardrail?
==================

guardrail is a code generation tool, capable of reading from OpenAPI/Swagger specification files and generating source code in different languages with a focus on type safety and ergonomics in each target language.

guardrail has three primary goals:

- Documentation: Single point of truth for the interface to a software system
- Better Servers: Unexpected API changes surface as compiler errors via server routing layer code generation
- Better Clients: Fewer binary dependencies via client library code generation

Describing software is tricky. Incomplete specifications, slippage between specification and implementation, or even additional semantics of infrastructure that aren't easily communicated through static documents; these are only a few challenges you'll face when attempting to write a specification for your API. A reasonable question you may be asking is what motivations are there for going through these cumbersome and often frustrating tasks? We'll investigate some answers to this question in the following sections.

Single Point of Truth
---------------------

By describing the shape of an API statically, there are far fewer variables to worry about. HTTP is a _very_ flexible protocol, with many features. By constraining that protocol to a subset that expresses the interface to our server (or service, or microservice), we drastically reduce the burden of handling the entirety of HTTP to the core terms of our API. Focus on semantics of APIs once the basics (routing, data validation) are figured out.

A secondary benefit of static specifications lies in tooling. Hand-written routing logic can hide security holes, miss best practices, and obscure intent if written incorrectly. This problem is multipled across as many different languages as are supported inside any given company, manifesting as wasted effort implementing the same feature in different languages, or a bug that only occurs 10 percent of the time due to a buggy hand-written client.

Attempting to derive what the attack surface of a server is from the implementation is often the job of entire teams in large companies, and even that may not be enough. Conversely, with a static specification, those teams can build intelligent traffic analysis tools to detect anomalies or intentionally malicious clients built to inject bad data to find bugs.

Unexpected API changes are compiler errors
------------------------------------------

Once we have a specification, generating traits (or abstract classes) with unimplemented members gives us another powerful tool: New or changed parameters become compiler (or linting) errors.

After constraining our vocabulary to a subset of HTTP that serves our business need, even saying "This parameter is optional" forces us to contend with the appearance of optional parameters in our generated `Handler` methods.

Once specified, turning on "unused" warnings helpfully points out that we've forgotten to reflect this most recent change in our tests. A win on both fronts!

Separation of business logic
----------------------------

Providing an implementation of a function with a well-defined set of inputs and outputs is natural for any developer. By reducing the scope of the interface a developer writes against, implementations are more clear and concise.

Furthermore, by providing business logic as an implementation of an abstract class, unit tests can test the routing layer and business logic independently, by design.

API structure slip is impossible
--------------------------------

As parameters are explicitly provided as arguments to functions in `Handler`s, any alteration to parameters constitute a new function interface that must be implemented. As a result, if providing an implementation for an externally managed specification, the compiler informs when a previously written function is no longer sufficient.

By representing different response codes and structures as members of an enumeration of valid choices, it's impossible to return a structure that violates the specification, even for less frequently used responses.

Finally, describing an endpoint in your specification without providing an implementation for it is an error. This prevents reduction of functionality due to refactors, human error, or miscommunication with other teams.

Fewer binary dependencies
----------------------

Bespoke, hand-written and maintained client libraries invariably accumulate cruft. In many cases, this is intended to be helpful: papering over a poorly designed API by providing custom logic, renaming parameters to be more convenient, or including properly configured HTTP clients that express retry and backoff semantics the library author provided based on the business requirements known at the time of writing.

This alters the shape of an API, reducing the shared terminology between service maintainers and their consumers, or even between consumers coming from different languages.

Additionally, by hardcoding even a well-behaved HTTP client into a client library, now consumers are forced to work around that dependency. In the worst case, this could result in a misconfigured underlying library silently falling back to default parameters, or writing and maintaining brittle [Application Binary Interface (ABI)](https://en.wikipedia.org/wiki/Application_binary_interface)-compatible adapter layers that attempt to use the configuration already present in the rest of the codebase.

Once these bespoke HTTP client configurations are built, both they and their dependencies are now added to the grab bag of dependency versions that must be maintained through the life of any given piece of infrastructure. This presents hidden barriers for upgrading all dependencies, as the possibility of transitive dependency conflicts increase as dependency trees become deeper.
