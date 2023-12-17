Makefile
========

A simple `Makefile` follows:

```
GUARDRAIL_VERSION=0.62.0
SWAGGER_TARGET=src/main/scala/swagger
SWAGGER_TEST_TARGET=src/test/scala/swagger

clean-clients:
	git clean -fdx $(SWAGGER_TARGET)
	git clean -fdx $(SWAGGER_TEST_TARGET)

clients: clean-clients
	java -jar guardrail-$(SWAGGER_CODEGEN_VERSION).jar \
		--client --specPath external/swagger/account-service.yaml --outputPath $(SWAGGER_TARGET)      --packageName dev.guardrail.test.clients.account --tracing \
		--server --specPath external/swagger/account-service.yaml --outputPath $(SWAGGER_TEST_TARGET) --packageName dev.guardrail.test.clients.account \
		--client --specPath external/swagger/billing-service.yaml --outputPath $(SWAGGER_TARGET)      --packageName dev.guardrail.test.clients.billing --tracing \
		--server --specPath external/swagger/billing-service.yaml --outputPath $(SWAGGER_TEST_TARGET) --packageName dev.guardrail.test.clients.billing
```

An important thing to note is that you don't want to modfy generated code. The CLI adapter is only intended to provide a baseline for environments that are not supported by other build tool plugins, providing a low barrier to entry before attempting to write a plugin for your environment.
