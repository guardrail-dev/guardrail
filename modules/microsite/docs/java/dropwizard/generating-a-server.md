---
layout: docs
title: "Generating a Server - java - dropwizard - guardrail"
---

Generating a Server
===================

Setup
-----

As we saw in [Installation](installation), guardrail is run as part of maven. It hooks into the `generate-sources` goal. This means our generated server and client is always up-to-date with the specfificaion file.

Lets take another look at the maven config for guardrail:

```xml
<plugin>
    <groupId>dev.guardrail</groupId>
    <artifactId>guardrail-maven-plugin_2.12</artifactId>
    <version>0.62.0</version>
    <executions>
            <id>generate-simple-server</id>           <!-- to identify this execution. there can be more than one. -->
            <goals>
                <goal>generate-sources</goal>         <!-- always hook to generate-sources goal -->
            </goals>
            <configuration>
                <language>java</language>             <!-- source code language to generate -->
                <framework>dropwizard</framework>     <!-- framework code to generate -->
                <kind>server</kind>                   <!-- to generate server routes or a http client -->
                <specPath>simple-spec.yaml</specPath> <!-- the Swagger/OpenAPI file for input -->
                <packageName>MyPackage</packageName>  <!-- all generated code will be in this jvm package -->
            </configuration>
        </execution>
    </executions>
</plugin>
```

**NB**: if you prefer to use Vavr instead of Java's standard library collections, see the [generic Java instructions](../) for appropriate configuration options.

Once configured, you can manually invoke guardrail with `mvn generate-sources`.

Server Handlers, Resources
--------------------------

guardrail-generated servers come in two parts: a `Resource` and a `Handler`. The `Resource` contains all the JAX-RS routing logic, accepting a `Handler` as a definion of the logic to perform for the routes. The `Handler` is generated as an interface. You will write the implementation of the `Handler` and pass it to the `Resource` in the bootstrapping of the Dropwizard service.

Consider the sample OpenAPI spec file we saw back in [Sample API Specification](sample-api-specification). That specified one path. When guardrail generates the server components for us, there will be a `UserHandler` with one method of the following signature:

```java
public CompletionStage<GetUserResponse> getUser(String id)
```

Breaking this down, we can see that for us to implement this route, we will be given the `id` from the path parameter. Give the ID, we will will return a value of type `GetUserResponse` wrapped in a `CompletionStage`. The special type `GetUserResponse` is also generated for us by guardrail. It is our interface to the responses defined in the OpenAPI spec file. Also, guardrail has set us to return a `CompletionStage` allowing us to compute our result asynchronous.

A simple implementation would be:

```java
public class SimpleHandlerImpl implements UsersHandler {
    @Override
    public CompletionStage<GetUserResponse> getUser(String id) {
        return CompletableFuture.supplyAsync(() -> {
            UserAddress address = (new UserAddress.Builder())
                    .withLine1("375 Beale st")
                    .withLine2("San Francisco, CA")
                    .build();
            List<UserAddress> userAddressList = new ArrayList<>();
            userAddressList.add(address);
            User user = (new User.Builder("Jane Doe"))
                    .withUserAddresses(userAddressList)
                    .build();
            return GetUserResponse.Ok(user);
        });
    }
}
```

Now we can examine the generated `Resource`. The resource companion class is generated with all the needed JAX-RS routing and parameter validation. In our Dropwizard Application class, where all resources are registerd with jersey, we can create the Resource and pass it our handler.

```java
    @Override
    public void run(final demowizardConfiguration configuration,
                    final Environment environment) {

        environment.jersey().register(new UsersResource(new SimpleHandlerImpl()));
    }
```

This illistrates that while guardrail has helped us with much of the setup related to Dropwizard, we are still in controll of all the configuration of the Dropwizard service. Only the HTTP edge layer has been automated for us.

(See it in action: [guardrail-dev/guardrail-sample-maven-dropwizard](https://github.com/guardrail-dev/guardrail-sample-maven-dropwizard))

Separation of business logic
----------------------------

Providing an implementating of a function with a well-defined set of inputs and outputs is natural for any developer. By reducing the scope of the interface a developer writes against, implementations are more clear and concise.

Furthermore, by providing business logic as an implementation of an abstract class, unit tests can test the routing layer and business logic independently, by design.

API structure slip is impossible
--------------------------------

As parameters are explicitly provided as arguments to functions in `Handler`s, any alteration to parameters constitute a new function interface that must be implemented. As a result, if providing an implementation for an externally managed specification, the compiler informs when a previously written function is no longer sufficient.

By representing different response codes and structures as members of a sealed trait, it's impossible to return a structure that violates the specification, even for less frequently used response codes.

Finally, describing an endpoint in your specification without providing an implementation for it is a compiler error. This prevents reduction of functionality due to refactors, human error, or miscommunication with other teams.

<span style="float: left">[Prev: Sample API specification](sample-api-specification)</span>
<span style="float: right">[Next: Generating clients](generating-clients)</span>
