Maven
=====

The [`guardrail-dev/guardrail-maven-plugin`](https://github.com/guardrail-dev/guardrail-maven-plugin) installation instructions can be found in that project's README.

By putting the following in your `pom.xml`, generated classes for petstore and github clients and `myserver` server routes are available the next time the `compile` or `generate-sources` goals are run. The motivation for generating servers in your test sources is to facilitate [generating "real" mock downstream servers](https://guardrail.dev/scala/akka-http/generating-a-server#generating-test-only-real-server-mocks-for-unit-tests).

```xml
<build>
  <plugins>
    ...
    <plugin>
      <groupId>dev.guardrail</groupId>
      <artifactId>guardrail-maven-plugin</artifactId>
      <version>0.62.0</version>
      <executions>
        <execution>
          <id>generate-petstore-client</id>
          <goals>
            <goal>generate-sources</goal>
          </goals>
          <configuration>
            <specPath>${project.basedir}/petstore.yaml</specPath>
          </configuration>
        </execution>
        <execution>
          <id>generate-github-client</id>
          <goals>
            <goal>generate-sources</goal>
          </goals>
          <configuration>
            <specPath>${project.basedir}/github.yaml</specPath>
            <packageName>com.example.clients.github</packageName>
          </configuration>
        </execution>
        <execution>
          <id>generate-server</id>
          <goals>
            <goal>generate-sources</goal>
          </goals>
          <configuration>
            <kind>server</kind>
            <specPath>${project.basedir}/myserver.yaml</specPath>
            <packageName>com.example.server</packageName>
            <tracing>true</tracing>
          </configuration>
        </execution>

        <execution>
          <id>generate-petstore-server</id>
          <goals>
            <goal>generate-test-sources</goal>
          </goals>
          <configuration>
            <kind>server</kind>
            <specPath>${project.basedir}/petstore.yaml</specPath>
          </configuration>
        </execution>
        <execution>
          <id>generate-github-server</id>
          <goals>
            <goal>generate-test-sources</goal>
          </goals>
          <configuration>
            <kind>server</kind>
            <specPath>${project.basedir}/github.yaml</specPath>
            <packageName>com.example.tests.github</packageName>
          </configuration>
        </execution>
        <execution>
          <id>generate-client</id>
          <goals>
            <goal>generate-test-sources</goal>
          </goals>
          <configuration>
            <specPath>${project.basedir}/myserver.yaml</specPath>
            <packageName>com.example.tests.server</packageName>
            <tracing>true</tracing>
          </configuration>
        </execution>
      </executions>
    </plugin>
    ...
  </plugins>
</build>
```

For the curious, generated sources end up in `target/generated-sources/`. These will be overwritten every time `compile` runs, so modification is not possible. If you find the generated source does not fit your needs, please see the section on [guardrail-specific extensions](https://guardrail.dev/scala/akka-http/guardrail-extensions) in the documentation.
