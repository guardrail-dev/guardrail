package core.DropwizardVavr;

import com.fasterxml.jackson.databind.ObjectMapper;
import form.server.dropwizardVavr.foo.FooHandler;
import form.server.dropwizardVavr.foo.FooResource;
import io.dropwizard.jersey.validation.Validators;
import io.dropwizard.testing.junit.ResourceTestRule;
import io.dropwizard.vavr.jersey.*;
import io.dropwizard.vavr.validation.ValueValidatedValueUnwrapper;
import io.vavr.collection.Vector;
import io.vavr.concurrent.Future;
import io.vavr.control.Option;
import io.vavr.control.Try;
import io.vavr.jackson.datatype.VavrModule;
import org.glassfish.jersey.test.grizzly.GrizzlyTestContainerFactory;
import org.junit.ClassRule;
import org.junit.Test;
import org.slf4j.LoggerFactory;

import javax.ws.rs.client.Entity;
import javax.ws.rs.core.Form;
import javax.ws.rs.core.Response;
import javax.ws.rs.ext.ExceptionMapper;

import static org.assertj.core.api.Assertions.assertThat;

public class DropwizardVavrRoundTripTest {
    private static final FooHandler handler = new FooHandler() {
        @Override
        public Future<DoFooResponse> doFoo(final String status, final String description) {
            return Future.successful(Try.<DoFooResponse>of(() -> {
                assertThat(status).isEqualTo("bad");
                assertThat(description).isEqualTo("this is bad");
                return DoFooResponse.Ok;
            }).getOrElse(DoFooResponse.NotAcceptable));
        }

        @Override
        public Future<DoBarResponse> doBar(final Option<String> status, final Option<String> description) {
            LoggerFactory.getLogger(getClass()).info("status: {}, description: {}", status, description);
            System.out.printf("status: %s, description: %s%n", status, description);
            return Future.successful(Try.<DoBarResponse>of(() -> {
                assertThat(status).containsExactly("good");
                assertThat(description).isEmpty();
                return DoBarResponse.Ok;
            }).getOrElse(DoBarResponse.NotAcceptable));
        }

        @Override
        public Future<DoBazResponse> doBaz(final Vector<String> status, final Vector<String> description) {
            return Future.successful(Try.<DoBazResponse>of(() -> {
                assertThat(status).containsExactly("good", "bad", "indifferent");
                assertThat(description).containsExactly("this is good", "this is bad", "this is indifferent");
                return DoBazResponse.Ok;
            }).getOrElse(DoBazResponse.NotAcceptable));
        }
    };

    public static class FooExceptionMapper implements ExceptionMapper<Exception> {
        @Override
        public Response toResponse(final Exception exception) {
            LoggerFactory.getLogger(getClass()).error("got exception: {}", exception, exception);
            return Response.serverError().build();
        }
    }

    @ClassRule
    public static final ResourceTestRule resources = new ResourceTestRule.Builder()
        .setTestContainerFactory(new GrizzlyTestContainerFactory())
        .setMapper(new ObjectMapper().registerModule(new VavrModule()))
        .setValidator(Validators.newConfiguration().addValidatedValueHandler(new ValueValidatedValueUnwrapper()).buildValidatorFactory().getValidator())
        .addProvider(new EmptyValueExceptionMapper())
        .addProvider(new LazyParamFeature())
        .addProvider(new OptionParamFeature())
        .addProvider(new EitherMessageBodyWriter())
        .addProvider(new ValueMessageBodyWriter())
        .addProvider(new CollectionParamFeature())
        .addResource(new FooResource(handler))
        .build();

    @Test
    public void testFormParams() {
        final Form form = new Form()
            .param("Status", "bad")
            .param("description", "this is bad");
        assertThat(
            resources
                .target("/foo")
                .request()
                .post(Entity.form(form))
                .getStatus()
        ).isEqualTo(200);
    }

    @Test
    public void testOptionFormParams() {
        final Form form = new Form()
            .param("Status", "good");
        assertThat(
            resources
                .target("/bar")
                .request()
                .post(Entity.form(form))
                .getStatus()
        ).isEqualTo(200);
    }

    @Test
    public void testVectorFormParams() {
        final Form form = new Form()
            .param("Status", "good")
            .param("Status", "bad")
            .param("Status", "indifferent")
            .param("description", "this is good")
            .param("description", "this is bad")
            .param("description", "this is indifferent");
        assertThat(
            resources
                .target("/baz")
                .request()
                .post(Entity.form(form))
                .getStatus()
        ).isEqualTo(200);
    }
}
