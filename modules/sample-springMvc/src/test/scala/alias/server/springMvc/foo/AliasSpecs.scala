package alias.server.springMvc.foo

import java.util.concurrent.CompletableFuture

import org.junit.runner.RunWith
import org.mockito.{ArgumentMatchersSugar, MockitoSugar}
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.autoconfigure.EnableAutoConfiguration
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.context.annotation.ComponentScan
import org.springframework.http.MediaType
import org.springframework.test.context.TestContextManager
import org.springframework.test.context.junit4.SpringRunner
import org.springframework.test.web.servlet.MockMvc
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders.{asyncDispatch, get, post}
import org.springframework.test.web.servlet.result.MockMvcResultHandlers.print
import org.springframework.test.web.servlet.result.MockMvcResultMatchers.{request, status}
import spring.test.TestApplication

@RunWith(classOf[SpringRunner])
@SpringBootTest(classes = Array(classOf[TestApplication]))
@AutoConfigureMockMvc
@ComponentScan
@EnableAutoConfiguration
class AliasSpecs extends FreeSpec with Matchers with BeforeAndAfterAll with MockitoSugar with ArgumentMatchersSugar {
  @Autowired var mvc: MockMvc               = _
  @Autowired var handlerMock: FooHandler = _

  new TestContextManager(this.getClass).prepareTestInstance(this)

  "test alias.foo" - {
    "optional body handled with alias param" in {

      when(handlerMock.doFoo(java.util.Optional.of(1L), java.util.Optional.of(42L)))
        .thenReturn(CompletableFuture.completedFuture(FooHandler.DoFooResponse.Created(42L)))

      val mvcResult = mvc
        .perform(
          post("/foo?long=1")
            .contentType(MediaType.APPLICATION_JSON)
            .content("42")
        )
        .andExpect(request.asyncStarted)
        .andReturn

      mvc.perform(asyncDispatch(mvcResult)).andDo(print()).andExpect(status.isCreated)
    }
  }
}
