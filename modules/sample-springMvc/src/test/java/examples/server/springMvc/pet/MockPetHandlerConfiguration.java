package examples.server.springMvc.pet;

import org.mockito.Mockito;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MockPetHandlerConfiguration {
    @Bean
    public PetHandler nameService() {
        return Mockito.mock(PetHandler.class);
    }
}
