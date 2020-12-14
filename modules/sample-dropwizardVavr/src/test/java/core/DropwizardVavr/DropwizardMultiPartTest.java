package core.DropwizardVavr;

import io.dropwizard.testing.junit.ResourceTestRule;
import io.vavr.concurrent.Future;
import io.vavr.control.Option;
import multipartFormData.server.dropwizardVavr.GuardrailJerseySupport;
import multipartFormData.server.dropwizardVavr.definitions.Foo;
import multipartFormData.server.dropwizardVavr.foo.FooHandler;
import multipartFormData.server.dropwizardVavr.foo.FooResource;
import org.glassfish.jersey.media.multipart.FormDataMultiPart;
import org.glassfish.jersey.media.multipart.MultiPartFeature;
import org.glassfish.jersey.test.TestProperties;
import org.junit.ClassRule;
import org.junit.Test;
import support.VavrHelpers;

import javax.ws.rs.client.Entity;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;

import static org.assertj.core.api.Assertions.assertThat;

public class DropwizardMultiPartTest {
    static {
        System.setProperty(TestProperties.CONTAINER_PORT, "0");
    }

    private static final FooHandler fooHandler = new FooHandler() {
        @Override
        public Future<DoFooResponse> doFoo(long id, OffsetDateTime date, Option<OffsetDateTime> optionalDate) {
            final Foo foo = new Foo.Builder(id, date)
                    .withOptionalDate(optionalDate)
                    .build();
            return Future.successful(FooHandler.DoFooResponse.Created(foo));
        }
    };

    @ClassRule
    public static final ResourceTestRule resources = VavrHelpers.newResourceTestRuleBuilder()
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
