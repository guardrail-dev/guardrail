Installation
---

guardrail is available as a modular core, with [Maven](https://github.com/guardrail-dev/guardrail-maven-plugin) and [Gradle](https://github.com/guardrail-dev/guardrail-gradle-plugin) integration. The core can also be run as a stand-alone [CLI](cli.md) application, with full support for all features.

| module  | version  | depends on |
|-----|-----|-----|
| guardrail-core | [![guardrail-core](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-core_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-core_2.12) |  |
| guardrail-java-support | [![guardrail-java-support](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-java-support_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-java-support_2.12) | core |
| guardrail-java-async-http | [![guardrail-java-async-http](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-java-async-http_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-java-async-http_2.12) | java-support |
| guardrail-java-dropwizard | [![guardrail-java-dropwizard](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-java-dropwizard_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-java-dropwizard_2.12) | java-support, java-async-http |
| guardrail-java-spring-mvc | [![guardrail-java-spring-mvc](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-java-spring-mvc_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-java-spring-mvc_2.12) | java-support |
| guardrail-cli | [![guardrail-cli](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-cli_2.12/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-cli_2.12) | guardrail-core |

### Example Maven configuration

Latest [`guardrail-maven-plugin`](https://github.com/guardrail-dev/guardrail-maven-plugin) version [![guardrail-maven-plugin](https://maven-badges.herokuapp.com/maven-central/dev.guardrail/guardrail-maven-plugin/badge.svg)](https://search.maven.org/search?q=g:dev.guardrail%20a:guardrail-maven-plugin) ([Other releases](https://github.com/guardrail-dev/guardrail-maven-plugin/releases))

guardrail for Dropwizard is generally set up using the maven plugin. This will generate your server or client at build time.
The following is an example invocation in a `pom.xml` file:

```xml
<build>
    <plugins>
        <plugin>
            <groupId>dev.guardrail</groupId>
            <artifactId>guardrail-maven-plugin_2.12</artifactId>
            <version><!-- Please see above for the latest version! --></version>
            <executions>
                <execution>
                    <id>generate-app-server</id>
                    <goals>
                        <goal>generate-sources</goal>
                    </goals>
                    <configuration>
                        <language>java</language>
                        <framework>dropwizard</framework>
                        <kind>server</kind>
                        <specPath>server-spec.yaml</specPath>
                        <packageName>example.generated</packageName>
                    </configuration>
                </execution>
            </executions>
        </plugin>
        (...)
    </plugins>
</build>
```

For JDK9+ you also need to add [`javax.annotation:javax.annotation-api`](https://repo1.maven.org/maven2/javax/annotation/javax.annotation-api/) dependency:

```xml
<dependencies>
    <dependency>
        <groupId>javax.annotation</groupId>
        <artifactId>javax.annotation-api</artifactId>
        <version>1.3.2</version>
    </dependency>
</dependencies>
```

## Vavr Support

In addition, the Java generation backend supports use of either standard
Java collections types (such as `java.util.Optional` and
`java.util.Map`), or [Vavr](https://vavr.io/) collections types (such as
`io.vavr.control.Option` and `io.vavr.collection.Vector`).  Vavr's
collection types are more internally consistent and attempt to provide
an interface familiar to functional programmers.  Scala developers will
find their APIs especially familiar.

To make use of the Vavr generation, you need to instead use guardrail's
module system.  Instead of specifying a `framework`, instead specify a
series of `module`s that describe the framework, protocol, and
collections library generators to use.

For example, to use Vavr with Dropwizard, the following
`<configuration>` can be used:

```xml
<modules>
  <module>java-vavr</module>
  <module>jackson</module>
  <module>dropwizard</module>
</module>
```

Currently, Vavr is only supported with the `dropwizard` framework.

Frameworks
---

Configuration for the following libraries is available:

- [Dropwizard](java/dropwizard/README.md)
- [spring-mvc](java/springMvc/README.md)

Sample repositories
---

There's a GitHub topic [here](https://github.com/topics/guardrail-sample), but a selection of those have been reproduced here:

- [guardrail-dev/guardrail-sample-gradle-springmvc](https://github.com/guardrail-dev/guardrail-sample-gradle-springmvc)
