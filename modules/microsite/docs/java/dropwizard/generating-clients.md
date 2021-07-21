---
layout: docs
title: "Generating Clients - java - dropwizard - guardrail"
---

To generate client code with maven please include following plugin to your pom.xml:
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
                        <kind>client</kind>
                        <specPath>spec.oas3.yaml</specPath>
                        <packageName>demowizard.client.generated</packageName>
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

(See it in action: [guardrail-dev/guardrail-sample-maven-dropwizard](https://github.com/guardrail-dev/guardrail-sample-maven-dropwizard))
