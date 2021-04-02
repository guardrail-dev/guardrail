---
layout: docs
title: "Installation - java - dropwizard - guardrail"
---

Installation
============

guardrail is available as a modular core, with both [sbt](https://github.com/guardrail-dev/sbt-guardrail) and [Maven](https://github.com/guardrail-dev/guardrail-maven-plugin) integration. The core can also be run as a stand-alone [CLI](https://github.com/guardrail-dev/guardrail/blob/978a92db3dd46812aa19f05050995f864cbb5bb3/build.sbt#L33-L48) application, with full support for all features.

guardrail for Dropwizard is generally set up using the maven plugin. This will generate your server or client at build time.
The following is an example invocation in a `pom.xml` file:

```xml
<build>
    <plugins>
        <plugin>
            <groupId>dev.guardrail</groupId>
            <artifactId>guardrail-maven-plugin_2.12</artifactId>
            <version>0.62.0</version>
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
                        <packageName>demowizard.generated</packageName>
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

<span style="float: left">[Prev: What is guardrail?](what-is-guardrail)</span>
<span style="float: right">[Next: Sample API specification](sample-api-specification)</span>
