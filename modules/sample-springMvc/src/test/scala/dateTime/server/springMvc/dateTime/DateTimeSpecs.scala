package dateTime.server.springMvc.dateTime

import java.time.{ LocalDate, OffsetDateTime }
import java.util.concurrent.CompletableFuture

import org.junit.runner.RunWith
import org.mockito.{ ArgumentMatchersSugar, MockitoSugar }
import org.scalatest.{ BeforeAndAfterAll, FreeSpec, Matchers }
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.autoconfigure.EnableAutoConfiguration
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.context.annotation.ComponentScan
import org.springframework.http.MediaType
import org.springframework.test.context.TestContextManager
import org.springframework.test.context.junit4.SpringRunner
import org.springframework.test.web.servlet.MockMvc
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders.{ asyncDispatch, post, get}
import org.springframework.test.web.servlet.result.MockMvcResultHandlers.print
import org.springframework.test.web.servlet.result.MockMvcResultMatchers.{ request, status }
import spring.test.TestApplication

@RunWith(classOf[SpringRunner])
@SpringBootTest(classes = Array(classOf[TestApplication]))
@AutoConfigureMockMvc
@ComponentScan
@EnableAutoConfiguration
class DateTimeSpecs extends FreeSpec with Matchers with BeforeAndAfterAll with MockitoSugar with ArgumentMatchersSugar {
  @Autowired var mvc: MockMvc                    = _
  @Autowired var handlerMock: DateTimeHandler = _

  new TestContextManager(this.getClass).prepareTestInstance(this)

  "test jsre310 stuff" - {
    "dates everywhere" in {

      val offsetDtNow = OffsetDateTime.now
      val localDtNow = LocalDate.now

      when(
        handlerMock.getSomething(
          eqTo(offsetDtNow),
          eqTo(java.util.Optional.of(offsetDtNow)),
          eqTo(localDtNow),
          eqTo(java.util.Optional.of(localDtNow))
        )
      ).thenReturn(CompletableFuture.completedFuture(DateTimeHandler.GetSomethingResponse.NoContent))

      val mvcResult = mvc
        .perform(
          get("/foo")
            .param("dateTime", offsetDtNow.toString)
            .param("optionalDateTime", offsetDtNow.toString)
            .param("date", localDtNow.toString)
            .param("optionalDate", localDtNow.toString)
            .contentType(MediaType.APPLICATION_JSON)
        )
        .andExpect(request.asyncStarted)
        .andReturn

      mvc.perform(asyncDispatch(mvcResult)).andDo(print()).andExpect(status.isNoContent)
    }
  }
}
