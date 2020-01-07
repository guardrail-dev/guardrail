package alias.server.springMvc.foo

import org.mockito.Mockito
import org.springframework.context.annotation.{ Bean, Configuration }

@Configuration class MockFooHandlerConfiguration {
  @Bean def nameService: FooHandler = Mockito.mock(classOf[FooHandler])
}
