package core.Dropwizard;

import helpers.JerseyTestHelpers;
import io.dropwizard.testing.junit.ResourceTestRule;
import multipartFormData.server.dropwizard.GuardrailJerseySupport;
import multipartFormData.server.dropwizard.definitions.Foo;
import multipartFormData.server.dropwizard.foo.FooHandler;
import multipartFormData.server.dropwizard.foo.FooResource;
import org.glassfish.jersey.media.multipart.FormDataMultiPart;
import org.glassfish.jersey.media.multipart.MultiPartFeature;
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
        JerseyTestHelpers.setRandomJerseyTestContainerPort();
    }

    private static final FooHandler fooHandler = new FooHandler() {
        @Override
        public CompletionStage<DoFooResponse> doFoo(long id, OffsetDateTime date, Optional<OffsetDateTime> optionalDate) {
            final Foo.Builder builder = Foo.builder(id, date);
            optionalDate.ifPresent(builder::withOptionalDate);
            return CompletableFuture.completedFuture(FooHandler.DoFooResponse.Created(builder.build()));
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
