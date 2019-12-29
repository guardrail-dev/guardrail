package examples.server.springMvc.pet

import org.junit.Assert.assertTrue
import org.junit.runner.RunWith
import org.scalatest.{FreeSpec, Matchers}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.autoconfigure.EnableAutoConfiguration
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.context.annotation.ComponentScan
import org.springframework.http.MediaType
import org.springframework.test.context.TestContextManager
import org.springframework.test.context.junit4.SpringRunner
import org.springframework.test.web.servlet.MockMvc
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders.{asyncDispatch, get}
import org.springframework.test.web.servlet.result.MockMvcResultHandlers.print
import org.springframework.test.web.servlet.result.MockMvcResultMatchers.{request, status}

@RunWith(classOf[SpringRunner])
@SpringBootTest(classes = Array(classOf[TestApplication]))
@AutoConfigureMockMvc
@ComponentScan
@EnableAutoConfiguration
class TestPetSpecs extends FreeSpec with Matchers  {
  @Autowired var mvc: MockMvc           = _
  @Autowired var petHandler: PetHandler = _

  new TestContextManager(this.getClass)
    .prepareTestInstance(this)

  "let's test pet-store generation" - {
    "should be able to execute simple get" in {
      val mvcResult = mvc
        .perform(
          get("/v2/pet/findByStatus?status=1")
            .accept(MediaType.APPLICATION_JSON)
        )
        .andExpect(request.asyncStarted)
        .andReturn

      mvc.perform(asyncDispatch(mvcResult)).andDo(print()).andExpect(status.isOk)

      val content = mvcResult.getResponse.getContentAsString
      assertTrue(content.contains("cat"))
      assertTrue(content.contains("mouse"))
    }
  }
}
