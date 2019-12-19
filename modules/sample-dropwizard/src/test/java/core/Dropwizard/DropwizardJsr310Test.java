package core.Dropwizard;

import dateTime.server.dropwizard.dateTime.DateTimeHandler;
import dateTime.server.dropwizard.dateTime.DateTimeResource;
import io.dropwizard.testing.junit.ResourceTestRule;
import org.glassfish.jersey.test.grizzly.GrizzlyTestContainerFactory;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;

import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.notNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class DropwizardJsr310Test {
    private static final DateTimeHandler handler = mock(DateTimeHandler.class);

    @ClassRule
    public static final ResourceTestRule resources = ResourceTestRule.builder()
            .setTestContainerFactory(new GrizzlyTestContainerFactory())
            .addResource(new DateTimeResource(handler))
            .build();

    @Before
    public void before() {
        reset(handler);

        when(handler.getSomething(any(), any(), any(), any()))
                .thenReturn(CompletableFuture.completedFuture(DateTimeHandler.GetSomethingResponse.NoContent));
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
        verify(handler, times(1)).getSomething(notNull(), eq(Optional.empty()), notNull(), eq(Optional.empty()));
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
