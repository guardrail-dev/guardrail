Gradle
===

The [`guardrail-dev/guardrail-gradle-plugin`](https://github.com/guardrail-dev/guardrail-gradle-plugin) plugin installation instructions can be found in that project's README.

By putting the following in your `build.gradle`, generated classes for petstore and github clients and `myserver` server routes are available the next time the `compile` task is run. The motivation for generating servers in your test sources is to facilitate [generating "real" mock downstream servers](https://guardrail.dev/scala/akka-http/generating-a-server#generating-test-only-real-server-mocks-for-unit-tests).

```
guardrail {
    petstoreServer {
        inputFile = file('src/main/resources/petstore.yaml')
        gen {
            packageName = 'example.server'
            kind = 'server'
            language = 'java'
            framework = 'dropwizard'
        }
    }

    githubClient {
        inputFile = file('src/main/resources/guardrail.yaml')
        gen {
            packageName = 'com.example.clients.github'
            kind = 'client'
            language = 'java'
            framework = 'dropwizard'
        }
    }
}
```

For the curious, generated sources end up in `build/guardrail-sources`. These will be overwritten every time `compile` runs, so modification is not possible. If you find the generated source does not fit your needs, please see the section on [guardrail-specific extensions](https://guardrail.dev/scala/akka-http/guardrail-extensions) in the documentation.
