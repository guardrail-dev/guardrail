import com.twilio.guardrail.generators.AkkaHttpServerGenerator
import com.twilio.guardrail.protocol.terms.server.RenderClass

def renderGenerator =
  AkkaHttpServerGenerator.ServerTermInterp(RenderClass("Foo", "Bar", List.empty, List.empty, List.empty, List.empty, List.empty))
    .value
    .run(com.twilio.swagger.core.StructuredLogger.apply(List.empty))
    ._2
    .right
    .get
    .head
