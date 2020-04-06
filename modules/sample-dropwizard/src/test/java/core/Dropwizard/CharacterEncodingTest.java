package core.Dropwizard;

import charEncoding.requestStream.server.dropwizard.charEncReqStream.CharEncReqStreamHandler;
import charEncoding.requestStream.server.dropwizard.charEncReqStream.CharEncReqStreamResource;
import charEncoding.requestStream.server.dropwizard.definitions.Pojo;
import charEncoding.responseStream.client.dropwizard.charEncRespStream.CharEncRespStreamClient;
import charEncoding.responseStream.client.dropwizard.charEncRespStream.SendPojoResponse;
import charEncoding.responseStream.client.dropwizard.charEncRespStream.SendTextPlainResponse;
import io.dropwizard.testing.junit.ResourceTestRule;
import org.glassfish.jersey.test.TestProperties;
import org.glassfish.jersey.test.grizzly.GrizzlyTestContainerFactory;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ExecutionException;

import static org.assertj.core.api.Assertions.assertThat;

public class CharacterEncodingTest {
    static {
        System.setProperty(TestProperties.CONTAINER_PORT, "0");
    }

    private static final String STRING_DATA = "áéíóúñàèìòç";

    private static final int[] STRING_DATA_UTF8 = new int[] {
            0xc3, 0xa1, 0xc3, 0xa9, 0xc3, 0xad, 0xc3, 0xb3,
            0xc3, 0xba, 0xc3, 0xb1, 0xc3, 0xa0, 0xc3, 0xa8,
            0xc3, 0xac, 0xc3, 0xb2, 0xc3, 0xa7
    };

    private static String testString(final InputStream stream) {
        try {
            final byte[] charBuf = new byte[STRING_DATA_UTF8.length];
            for (int i = 0; i <  STRING_DATA_UTF8.length; ++i) {
                final int input = stream.read();
                charBuf[i] = (byte) input;
            }
            assertThat(charBuf).isEqualTo(Arrays.stream(STRING_DATA_UTF8).boxed().map(Integer::byteValue).toArray());
            assertThat(stream.read()).isEqualTo(-1);  // should have hit the end

            final String receivedStr = new String(charBuf, StandardCharsets.UTF_8);
            assertThat(receivedStr).isEqualTo(STRING_DATA);

            return receivedStr;
        } catch (final IOException e) {
            throw new UncheckedIOException(e.getMessage(), e);
        }
    }

    private static final CharEncReqStreamHandler handler = new CharEncReqStreamHandler() {
        @Override
        public CompletionStage<SendFormDataResponse> sendFormData(final String str) {
            if (STRING_DATA.equals(str)) {
                return CompletableFuture.completedFuture(SendFormDataResponse.Ok(str));
            } else {
                return CompletableFuture.completedFuture(SendFormDataResponse.InternalServerError(
                        String.format("Strings did not match: expected %s, got %s", STRING_DATA, str)));
            }
        }

        @Override
        public CompletionStage<SendTextPlainResponse> sendTextPlain(final InputStream stream) {
            try {
                return CompletableFuture.completedFuture(SendTextPlainResponse.Ok(testString(stream)));
            } catch (final Throwable t) {
                return CompletableFuture.completedFuture(SendTextPlainResponse.InternalServerError(t.getMessage()));
            }
        }

        @Override
        public CompletionStage<SendPojoResponse> sendPojo(final Pojo body) {
            if (STRING_DATA.equals(body.getStr())) {
                return CompletableFuture.completedFuture(SendPojoResponse.Ok(new Pojo.Builder(body.getStr()).build()));
            } else {
                return CompletableFuture.completedFuture(SendPojoResponse.InternalServerError(
                        String.format("Strings did not match: expected %s, got %s", STRING_DATA, body.getStr())));
            }
        }
    };

    @ClassRule
    public static ResourceTestRule resources = ResourceTestRule.builder()
            .setTestContainerFactory(new GrizzlyTestContainerFactory())
            .addResource(new CharEncReqStreamResource(handler))
            .build();

    private static CharEncRespStreamClient createClient() throws URISyntaxException {
        final URI clientUri = new URI(resources.target("").getUri().toString().replaceAll("/$", ""));
        return new CharEncRespStreamClient.Builder(clientUri).build();
    }

    @Test
    public void testClientSendsCorrectBytesTextPlain() throws URISyntaxException, ExecutionException, InterruptedException {
        final CharEncRespStreamClient client = createClient();
        final SendTextPlainResponse response = client.sendTextPlain(STRING_DATA).call().toCompletableFuture().get();
        response.fold(CharacterEncodingTest::testString, error -> {
            throw new RuntimeException(error);
        });
    }

    @Test
    public void testRoundTripJson() throws URISyntaxException, ExecutionException, InterruptedException {
        final CharEncRespStreamClient client = createClient();
        final SendPojoResponse response = client.sendPojo(
                new charEncoding.responseStream.client.dropwizard.definitions.Pojo.Builder(STRING_DATA).build()
        ).call().toCompletableFuture().get();
        response.fold(
                pojo -> assertThat(pojo.getStr()).isEqualTo(STRING_DATA),
                error -> { throw new RuntimeException(error); }
        );
    }
}
