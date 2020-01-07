package examples.server.springMvc.pet

import org.mockito.Mockito
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

@Configuration class MockPetHandlerConfiguration {
  @Bean def nameService: PetHandler = Mockito.mock(classOf[PetHandler])
}
