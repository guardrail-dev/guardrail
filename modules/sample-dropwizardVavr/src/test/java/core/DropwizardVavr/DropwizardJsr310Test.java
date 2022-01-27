package core.DropwizardVavr;

import dateTime.server.dropwizardVavr.dateTime.DateTimeHandler;
import dateTime.server.dropwizardVavr.dateTime.DateTimeResource;
import io.dropwizard.testing.junit.ResourceTestRule;
import io.vavr.concurrent.Future;
import io.vavr.control.Option;
import org.glassfish.jersey.test.TestProperties;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;
import support.VavrHelpers;

import java.time.LocalDate;
import java.time.OffsetDateTime;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

public class DropwizardJsr310Test {
    static {
        System.setProperty(TestProperties.CONTAINER_PORT, "0");
    }

    private static final DateTimeHandler handler = mock(DateTimeHandler.class);

    @ClassRule
    public static final ResourceTestRule resources = VavrHelpers.newResourceTestRuleBuilder()
            .addResource(new DateTimeResource(handler))
            .build();

    @Before
    public void before() {
        reset(handler);

        when(handler.getSomething(any(), any(), any(), any()))
                .thenReturn(Future.successful(DateTimeHandler.GetSomethingResponse.NoContent));
    }

    @Test
    public void testAllDateTimesPresent() {
        assertThat(
                resources
                        .target("/foo")
                        .queryParam("dateTime", OffsetDateTime.now().toString())
                        .queryParam("optionalDateTime", OffsetDateTime.now().toString())
                        .queryParam("date", LocalDate.now().toString())
                        .queryParam("optionalDate", LocalDate.now().toString())
                        .request()
                        .get()
                        .getStatus()
        ).isEqualTo(204);
        verify(handler, times(1)).getSomething(notNull(), notNull(), notNull(), notNull());
    }

    @Test
    public void testOptionalDateTimesMissing() {
        assertThat(
                resources
                        .target("/foo")
                        .queryParam("dateTime", OffsetDateTime.now().toString())
                        .queryParam("date", LocalDate.now().toString())
                        .request()
                        .get()
                        .getStatus()
        ).isEqualTo(204);
        verify(handler, times(1)).getSomething(notNull(), eq(Option.none()), notNull(), eq(Option.none()));
    }

    @Test
    public void testAllDateTimesMissing() {
        assertThat(
                resources
                        .target("/foo")
                        .request()
                        .get()
                        .getStatus()
        ).isEqualTo(400);
        verify(handler, never()).getSomething(any(), any(), any(), any());
    }

    @Test
    public void testUnparseableDateTimes() {
        assertThat(
                resources
                        .target("/foo")
                        .queryParam("dateTime", "oasdpajdsapojdapsdja")
                        .queryParam("date", "asoidajdo290ewjdk")
                        .request()
                        .get()
                        .getStatus()
        ).isEqualTo(400);
        verify(handler, never()).getSomething(any(), any(), any(), any());
    }
}
