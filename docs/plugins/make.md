Makefile
========

A simple `Makefile` using [coursier](https://get-coursier.io/) (`cs`) could be as follows:

```
GUARDRAIL_CLI_VERSION=1.0.0-M1
GUARDRAIL_AKKA_HTTP_VERSION=1.0.0-M1
SLF4J_JDK14_VERSION=2.0.9

CODEGEN_TARGET=src/main/scala/swagger
CODEGEN_TEST_TARGET=src/test/scala/swagger

clean-clients:
	git clean -fdx $(CODEGEN_TARGET)
	git clean -fdx $(CODEGEN_TEST_TARGET)

clients: clean-clients
	cs launch --repository https://s01.oss.sonatype.org/content/repositories/releases \
		dev.guardrail:guardrail-cli_2.13:$(GUARDRAIL_CLI_VERSION) \
			dev.guardrail:guardrail-scala-akka-http_2.13:$(GUARDRAIL_AKKA_HTTP_VERSION) \
			org.slf4j:slf4j-jdk14:$(SLF4J_JDK14_VERSION) \
		-- \
			java \
			--default --framework akka-http --packageName boop \
			--client --packageName example.clients.account      --specPath external/swagger/account-service.yaml --outputPath $(CODEGEN_TARGET) \
			--server --packageName example.test.servers.account --specPath external/swagger/account-service.yaml --outputPath $(CODEGEN_TEST_TARGET) \
			--client --packageName example.clients.account      --specPath external/swagger/billing-service.yaml --outputPath $(CODEGEN_TARGET) \
			--server --packageName example.test.servers.account --specPath external/swagger/billing-service.yaml --outputPath $(CODEGEN_TEST_TARGET)
```

An important thing to note is that you don't want to modfy generated code. The CLI adapter is only intended to provide a baseline for environments that are not supported by other build tool plugins, providing a low barrier to entry before attempting to write a plugin for your environment.
