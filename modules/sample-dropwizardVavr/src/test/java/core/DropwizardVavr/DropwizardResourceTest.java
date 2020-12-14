package core.DropwizardVavr;

import examples.server.dropwizardVavr.definitions.User;
import examples.server.dropwizardVavr.user.UserHandler;
import examples.server.dropwizardVavr.user.UserHandler.CreateUserResponse;
import examples.server.dropwizardVavr.user.UserHandler.GetUserByNameResponse;
import examples.server.dropwizardVavr.user.UserHandler.LoginUserResponse;
import examples.server.dropwizardVavr.user.UserResource;
import io.dropwizard.testing.junit.ResourceTestRule;
import io.vavr.concurrent.Future;
import org.glassfish.jersey.test.TestProperties;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;
import support.VavrHelpers;

import javax.ws.rs.BadRequestException;
import javax.ws.rs.NotFoundException;
import javax.ws.rs.client.Entity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

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
    public static final ResourceTestRule resources = VavrHelpers.newResourceTestRuleBuilder()
            .addResource(new UserResource(userHandler))
            .build();

    @Before
    public void setup() {
        reset(userHandler);

        when(userHandler.getUserByName(anyString())).thenReturn(Future.successful(GetUserByNameResponse.NotFound));
        when(userHandler.getUserByName(eq(" "))).thenReturn(Future.successful(GetUserByNameResponse.BadRequest));
        when(userHandler.getUserByName(eq(USERNAME))).thenReturn(Future.successful(GetUserByNameResponse.Ok(USER)));

        when(userHandler.createUser(eq(USER))).thenReturn(Future.successful(CreateUserResponse.Ok));

        when(userHandler.loginUser(anyString(), anyString())).thenReturn(Future.successful(LoginUserResponse.BadRequest));
        when(userHandler.loginUser(eq(USERNAME), eq(PASSWORD))).thenReturn(Future.successful(LoginUserResponse.Ok(TOKEN)));
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
