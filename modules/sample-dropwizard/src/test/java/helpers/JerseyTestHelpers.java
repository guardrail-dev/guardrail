package helpers;

import org.glassfish.jersey.test.TestProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.util.Random;

public abstract class JerseyTestHelpers {
    private static final Logger logger = LoggerFactory.getLogger(JerseyTestHelpers.class);
    private static final Random random = new Random();

    public static void setRandomJerseyTestContainerPort() {
        // Default container port is 9998, which isn't always available.  This is racy, of course, but
        // there's no way to tell Jersey to use a random port and to retry on other ports if it's in use.
        int attemptsLeft = 100;
        while (attemptsLeft > 0) {
            final int port = random.nextInt(20000) + 20000;
            try {
                final ServerSocket socket = new ServerSocket();
                socket.setReuseAddress(true);
                socket.bind(new InetSocketAddress(InetAddress.getLoopbackAddress(), port));
                socket.close();
                System.setProperty(TestProperties.CONTAINER_PORT, String.valueOf(port));
                logger.info("Selected port {} for the Jersey test container", port);
                return;
            } catch (final IOException e) {
                attemptsLeft -= 1;
            }
        }

        throw new IllegalStateException("Failed to find unusued random port for the Jersey test container");
    }

    private JerseyTestHelpers() {}
}

