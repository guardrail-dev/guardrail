package core.Dropwizard;

import io.dropwizard.testing.junit.ResourceTestRule;
import org.glassfish.jersey.test.TestProperties;
import org.glassfish.jersey.test.grizzly.GrizzlyTestContainerFactory;
import org.junit.ClassRule;
import org.junit.Test;
import security.server.dropwizard.Handler;
import security.server.dropwizard.HttpSecurityUtils;
import security.server.dropwizard.Resource;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Optional;
import java.util.concurrent.CompletionStage;

import static org.assertj.core.api.Assertions.assertThat;
import static java.util.concurrent.CompletableFuture.completedFuture;

public class DropwizardSecurityTest {
    static {
        System.setProperty(TestProperties.CONTAINER_PORT, "0");
    }

    private static final String API_KEY = "sekrit-api-key";
    private static final String BASIC_USERNAME = "some-user";
    private static final String BASIC_PASSWORD = "my-sekrit-password";
    private static final String BEARER_TOKEN = "my-sekret-token";

    private static final Handler handler = new Handler() {
        @Override
        public CompletionStage<ApiKeyHeaderResponse> apiKeyHeader(Optional<String> xApiKey, Optional<Integer> foo) {
            assertThat(xApiKey.get()).isEqualTo(API_KEY);
            return completedFuture(ApiKeyHeaderResponse.Ok);
        }

        @Override
        public CompletionStage<ApiKeyQueryResponse> apiKeyQuery(Optional<String> apiKey, Optional<Integer> foo) {
            assertThat(apiKey.get()).isEqualTo(API_KEY);
            return completedFuture(ApiKeyQueryResponse.Ok);
        }

        @Override
        public CompletionStage<HttpBasicAuthResponse> httpBasicAuth(Optional<HttpSecurityUtils.HttpBasicCredentials> httpBasicCredentials, Optional<Integer> foo) {
            assertThat(httpBasicCredentials.get().getUsername()).isEqualTo(BASIC_USERNAME);
            assertThat(httpBasicCredentials.get().getPassword()).isEqualTo(BASIC_PASSWORD);
            return completedFuture(HttpBasicAuthResponse.Ok);
        }

        @Override
        public CompletionStage<HttpBearerAuthResponse> httpBearerAuth(Optional<HttpSecurityUtils.HttpBearerCredentials> httpBearerCredentials, Optional<Integer> foo) {
            assertThat(httpBearerCredentials.get().getToken()).isEqualTo(BEARER_TOKEN);
            return completedFuture(HttpBearerAuthResponse.Ok);
        }

        @Override
        public CompletionStage<MultipleAuthAndResponse> multipleAuthAnd(Optional<String> xApiKey, Optional<HttpSecurityUtils.HttpBearerCredentials> httpBearerCredentials) {
            assertThat(xApiKey.get()).isEqualTo(API_KEY);
            assertThat(httpBearerCredentials.get().getToken()).isEqualTo(BEARER_TOKEN);
            return completedFuture(MultipleAuthAndResponse.Ok);
        }

        @Override
        public CompletionStage<MultipleAuthOrResponse> multipleAuthOr(Optional<String> apiKey, Optional<HttpSecurityUtils.HttpBasicCredentials> httpBasicCredentials, Optional<HttpSecurityUtils.HttpBearerCredentials> httpBearerCredentials) {
            assertThat(apiKey.get()).isEqualTo(API_KEY);
            assertThat(httpBasicCredentials.get().getUsername()).isEqualTo(BASIC_USERNAME);
            assertThat(httpBasicCredentials.get().getPassword()).isEqualTo(BASIC_PASSWORD);
            assertThat(httpBearerCredentials.isPresent()).isFalse();
            return completedFuture(MultipleAuthOrResponse.Ok);
        }
    };

    @ClassRule
    public static final ResourceTestRule resources = ResourceTestRule.builder()
            .setTestContainerFactory(new GrizzlyTestContainerFactory())
            .addResource(new Resource(handler))
            .build();

    @Test
    public void testApiKeyQueryAuth() {
        assertThat(
                resources
                        .target("/api-key-query")
                        .queryParam("ApiKey", API_KEY)
                        .request()
                        .get()
                        .getStatus()
        ).isEqualTo(200);
    }

    @Test
    public void testApiKeyHeaderAuth() {
        assertThat(
                resources
                        .target("/api-key-header")
                        .request()
                        .header("x-api-key", API_KEY)
                        .get()
                        .getStatus()
        ).isEqualTo(200);
    }

    @Test
    public void testHttpBasicAuth() {
        assertThat(
                resources
                        .target("/http-basic")
                        .request()
                        .header("authorization", createHttpBasicHeader(BASIC_USERNAME, BASIC_PASSWORD))
                        .get()
                        .getStatus()
        ).isEqualTo(200);
    }

    @Test
    public void testHttpBearerAuth() {
        assertThat(
                resources
                        .target("/http-bearer")
                        .request()
                        .header("authorization", createHttpBearerHeader(BEARER_TOKEN))
                        .get()
                        .getStatus()
        ).isEqualTo(200);
    }

    @Test
    public void testMultipleOr() {
        assertThat(
                resources
                        .target("/multiple-or")
                        .queryParam("ApiKey", API_KEY)
                        .request()
                        .header("authorization", createHttpBasicHeader(BASIC_USERNAME, BASIC_PASSWORD))
                        .get()
                        .getStatus()
        ).isEqualTo(200);
    }

    @Test
    public void testMultipleAnd() {
        assertThat(
                resources
                        .target("/multiple-and")
                        .request()
                        .header("x-api-key", API_KEY)
                        .header("authorization", createHttpBearerHeader(BEARER_TOKEN))
                        .get()
                        .getStatus()
        ).isEqualTo(200);
    }

    private static String createHttpBasicHeader(final String username, final String password) {
        return "Basic " + Base64.getEncoder().encodeToString((username + ":" + password).getBytes(StandardCharsets.UTF_8));
    }

    private static String createHttpBearerHeader(final String token) {
        return "Bearer " + token;
    }
}
