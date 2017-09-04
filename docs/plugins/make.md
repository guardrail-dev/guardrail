Makefile
========

A simple `Makefile` follows:

```
SWAGGER_CODEGEN_VERSION=0.1.2
SWAGGER_TARGET=src/main/scala/swagger
SWAGGER_TEST_TARGET=src/test/scala/swagger

clean-clients:
	git clean -fdx $(SWAGGER_TARGET)
	git clean -fdx $(SWAGGER_TEST_TARGET)

clients: clean-clients
	java -jar scala-codegen-$(SWAGGER_CODEGEN_VERSION).jar \
		--client --specPath external/swagger-api-definitions/account-service.yaml --outputPath $(SWAGGER_TARGET)      --packageName com.twilio.test.clients.account --tracing \
		--server --specPath external/swagger-api-definitions/account-service.yaml --outputPath $(SWAGGER_TEST_TARGET) --packageName com.twilio.test.clients.account \
		--client --specPath external/swagger-api-definitions/billing-events-api.yaml --outputPath $(SWAGGER_TARGET)      --packageName com.twilio.test.clients.billing --tracing \
		--server --specPath external/swagger-api-definitions/billing-events-api.yaml --outputPath $(SWAGGER_TEST_TARGET) --packageName com.twilio.test.clients.billing
```
