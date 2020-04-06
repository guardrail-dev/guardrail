package core.Dropwizard;

import examples.client.dropwizard.AsyncHttpClientUtils;
import org.asynchttpclient.Response;
import org.junit.Test;

import java.nio.charset.StandardCharsets;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class AsyncHttpClientUtilsTest {
    @Test
    public void testCharsetNoContentType() {
        final Response response = mock(Response.class);
        when(response.getHeader(eq("Content-Type"))).thenReturn(null);

        assertThat(AsyncHttpClientUtils.getResponseCharset(response)).isEmpty();
    }

    @Test
    public void testCharsetNone() {
        final Response response = mock(Response.class);
        when(response.getHeader(eq("Content-Type"))).thenReturn("text/plain");

        assertThat(AsyncHttpClientUtils.getResponseCharset(response)).isEmpty();
    }

    @Test
    public void testCharsetAtEnd() {
        final Response response = mock(Response.class);
        when(response.getHeader(eq("Content-Type"))).thenReturn("text/plain; charset=utf-8");

        assertThat(AsyncHttpClientUtils.getResponseCharset(response)).contains(StandardCharsets.UTF_8);
    }

    @Test
    public void testCharsetInMiddle() {
        final Response response = mock(Response.class);
        when(response.getHeader(eq("Content-Type"))).thenReturn("text/plain; charset=utf-8; foobar=baz");

        assertThat(AsyncHttpClientUtils.getResponseCharset(response)).contains(StandardCharsets.UTF_8);
    }
}
