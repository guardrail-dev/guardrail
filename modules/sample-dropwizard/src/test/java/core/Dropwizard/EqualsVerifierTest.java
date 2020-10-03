package core.Dropwizard;

import nl.jqno.equalsverifier.EqualsVerifier;

import org.junit.Test;

public class EqualsVerifierTest {

    @Test
    public void testEqualsContract() {
        // client classes
        verify(examples.client.dropwizard.definitions.ApiResponse.class);
        verify(examples.client.dropwizard.definitions.OrderStatus.class);
        verify(examples.client.dropwizard.definitions.Pet.class);
        verify(examples.client.dropwizard.definitions.User.class);

        // server classes
        verify(examples.server.dropwizard.definitions.ApiResponse.class);
        verify(examples.server.dropwizard.definitions.OrderStatus.class);
        verify(examples.server.dropwizard.definitions.Pet.class);
        verify(examples.server.dropwizard.definitions.User.class);
    }

    private static void verify(Class clazz) {
        EqualsVerifier.forClass(clazz)
                .usingGetClass()
                .verify();
    }
}
