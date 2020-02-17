package dateTime.server.springMvc.dateTime

import org.mockito.Mockito
import org.springframework.context.annotation.{Bean, Configuration}

@Configuration class MockDateTimeHandlerConfiguration {
  @Bean def nameService: DateTimeHandler = Mockito.mock(classOf[DateTimeHandler])
}
