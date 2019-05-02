package core.Dropwizard;

import io.dropwizard.testing.junit.ResourceTestRule;
import multipartFormData.server.dropwizard.GuardrailJerseySupport;
import multipartFormData.server.dropwizard.definitions.Foo;
import multipartFormData.server.dropwizard.foo.FooHandler;
import multipartFormData.server.dropwizard.foo.FooResource;
import org.glassfish.jersey.media.multipart.FormDataMultiPart;
import org.glassfish.jersey.media.multipart.MultiPartFeature;
import org.glassfish.jersey.test.TestProperties;
import org.glassfish.jersey.test.grizzly.GrizzlyTestContainerFactory;
import org.junit.ClassRule;
import org.junit.Test;

import javax.ws.rs.client.Entity;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

import static org.assertj.core.api.Assertions.assertThat;

public class DropwizardMultiPartTest {
    static {
        System.setProperty(TestProperties.CONTAINER_PORT, "0");
    }

    private static final FooHandler fooHandler = new FooHandler() {
        @Override
        public CompletionStage<DoFooResponse> doFoo(long id, OffsetDateTime date, Optional<OffsetDateTime> optionalDate) {
            final Foo foo = new Foo.Builder(id, date)
                    .withOptionalDate(optionalDate)
                    .build();
            return CompletableFuture.completedFuture(FooHandler.DoFooResponse.Created(foo));
        }
    };

    @ClassRule
    public static final ResourceTestRule resources = ResourceTestRule.builder()
            .setTestContainerFactory(new GrizzlyTestContainerFactory())
            .addProvider(new MultiPartFeature())
            .addProvider(new GuardrailJerseySupport.Jsr310.Binder())
            .addResource(new FooResource(fooHandler))
            .build();

    @Test
    public void testJava8TimeMultiPartFormData() {
        final OffsetDateTime now = OffsetDateTime.now().withOffsetSameInstant(ZoneOffset.UTC);
        final FormDataMultiPart multiPart = new FormDataMultiPart()
                .field("id", "42")
                .field("date", now.toString())
                .field("optional_date", now.toString());
        final Foo response = resources
                .target("/foo")
                .register(new MultiPartFeature())
                .request()
                .post(Entity.entity(multiPart, multiPart.getMediaType()))
                .readEntity(Foo.class);
        assertThat(response.getId()).isEqualTo(42);
        assertThat(response.getDate()).isEqualTo(now);
        assertThat(response.getOptionalDate().get()).isEqualTo(now);
    }
}
