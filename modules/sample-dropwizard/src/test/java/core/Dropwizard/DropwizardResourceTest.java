package core.Dropwizard;

import examples.server.dropwizard.definitions.User;
import examples.server.dropwizard.user.UserHandler;
import examples.server.dropwizard.user.UserHandler.CreateUserResponse;
import examples.server.dropwizard.user.UserHandler.GetUserByNameResponse;
import examples.server.dropwizard.user.UserHandler.LoginUserResponse;
import examples.server.dropwizard.user.UserResource;
import io.dropwizard.testing.junit.ResourceTestRule;
import org.glassfish.jersey.test.TestProperties;
import org.glassfish.jersey.test.grizzly.GrizzlyTestContainerFactory;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;

import javax.ws.rs.BadRequestException;
import javax.ws.rs.NotFoundException;
import javax.ws.rs.client.Entity;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class DropwizardResourceTest {
    static {
        System.setProperty(TestProperties.CONTAINER_PORT, "0");
    }

    private static final String USERNAME = "foobar";
    private static final String PASSWORD = "sekrit";
    private static final String TOKEN = "abc123";
    private static final User USER = new User.Builder()
            .withId(1)
            .withUsername(USERNAME)
            .withPassword(PASSWORD)
            .withFirstName("Blast")
            .withLastName("Hardcheese")
            .build();

    private static final UserHandler userHandler = mock(UserHandler.class);

    @ClassRule
    public static final ResourceTestRule resources = ResourceTestRule.builder()
            .setTestContainerFactory(new GrizzlyTestContainerFactory())
            .addResource(new UserResource(userHandler))
            .build();

    @Before
    public void setup() {
        reset(userHandler);

        when(userHandler.getUserByName(anyString())).thenReturn(completedFuture(GetUserByNameResponse.NotFound));
        when(userHandler.getUserByName(eq(" "))).thenReturn(completedFuture(GetUserByNameResponse.BadRequest));
        when(userHandler.getUserByName(eq(USERNAME))).thenReturn(completedFuture(GetUserByNameResponse.Ok(USER)));

        when(userHandler.createUser(eq(USER))).thenReturn(completedFuture(CreateUserResponse.Ok));

        when(userHandler.loginUser(anyString(), anyString())).thenReturn(completedFuture(LoginUserResponse.BadRequest));
        when(userHandler.loginUser(eq(USERNAME), eq(PASSWORD))).thenReturn(completedFuture(LoginUserResponse.Ok(TOKEN)));
    }

    @Test
    public void testGetUserByName() {
        assertThat(resources.target("/v2/user/" + USERNAME).request().get(User.class)).isEqualTo(USER);
        verify(userHandler).getUserByName(USERNAME);
    }

    @Test(expected = NotFoundException.class)
    public void testUnknownGetUserByName() {
        resources.target("/v2/user/nope").request().get(User.class);
    }

    @Test(expected = BadRequestException.class)
    public void testInvalidGetUserByName() {
        resources.target("/v2/user/ ").request().get(User.class);
    }

    @Test
    public void testCreateUser() {
        assertThat(resources.target("/v2/user").request().post(Entity.json(USER)).getStatus()).isEqualTo(200);
        verify(userHandler).createUser(USER);
    }

    @Test
    public void testMissingBodyCreateUser() {
        assertThat(resources
                .target("/v2/user")
                .request()
                .build("POST")
                .invoke()
                .getStatus()
        ).isEqualTo(422);
        verify(userHandler, never()).createUser(any());
    }

    @Test
    public void testLoginUser() {
        assertThat(resources
                .target("/v2/user/login")
                .queryParam("username", USERNAME)
                .queryParam("password", PASSWORD)
                .request()
                .get(String.class)
        ).isEqualTo(TOKEN);
        verify(userHandler).loginUser(USERNAME, PASSWORD);
    }

    @Test(expected = BadRequestException.class)
    public void testInvalidLoginUser() {
        resources
                .target("/v2/user/login")
                .queryParam("username", "moo")
                .queryParam("password", "")
                .request()
                .get(String.class);
    }
}
