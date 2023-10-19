package core.issues

import com.github.javaparser.ast.Node
import dev.guardrail.Context
import dev.guardrail.generators.java.JavaGeneratorMappings.javaInterpreter
import dev.guardrail.generators.{ Clients, Server, Servers }
import org.scalactic.source
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import support.SwaggerSpecRunner

import scala.language.existentials

class Issue314 extends AnyFunSpec with Matchers with SwaggerSpecRunner {

  private def swagger(config: String) =
    s"""
       |swagger: '2.0'
       |host: localhost:1234
       |paths:
       |  /user/{id}:
       |    get:
       |      operationId: getUser
       |      $config
       |      consumes:
       |        - application/json
       |      produces:
       |        - application/json
       |      parameters:
       |      - name: id
       |        in: path
       |        type: string
       |      responses:
       |        200:
       |          description: Successful
       |""".stripMargin

  describe("Java") {

    it("should be possible to define an arbitrary class name") {
      verify(
        s"""
           |      x-java-package: users
           |      x-java-class-prefix: hello
             """.stripMargin,
        "Hello"
      )
    }

    it("should use the last component of a package name as a class name") {
      verify(
        s"""
           |      x-java-package: users
           """.stripMargin,
        "Users"
      )
    }

    it("should fallback to tags in order to determine class name") {
      verify(
        s"""
           |      tags: [hello]
         """.stripMargin,
        "Hello"
      )
    }

    it("should be possible to override a class name") {
      verify(
        s"""
           |      tags: [whatever]
           |      x-java-package: users
           |      x-java-class-prefix: hello
             """.stripMargin,
        "Hello"
      )
    }

    def verify(config: String, expectedClassPrefix: String)(implicit pos: source.Position): Unit = {
      verifyClient(config, expectedClassPrefix)
      verifyServer(config, expectedClassPrefix)
    }

    def verifyClient(config: String, expectedClassPrefix: String)(implicit pos: source.Position): Unit = {

      val (_, Clients(clt :: _, _), _) =
        runSwaggerSpec(javaInterpreter)(swagger(config))(Context.empty.copy(tagsBehaviour = Context.PackageFromTags), "dropwizard")
      val cls = clt.client.head.getOrElse(fail("Client does not contain a ClassDefinition"))

      verifyNode(cls, client(expectedClassPrefix))
    }

    def verifyServer(config: String, expectedClassPrefix: String)(implicit pos: source.Position): Unit = {
      val (
        _,
        _,
        Servers(Server(_, _, genHandler, genResource :: Nil) :: Nil, _)
      ) = runSwaggerSpec(javaInterpreter)(swagger(config))(Context.empty.copy(tagsBehaviour = Context.PackageFromTags), "dropwizard")

      verifyNode(genHandler, handler(expectedClassPrefix))
      verifyNode(genResource, resource(expectedClassPrefix))
    }

    def verifyNode(node: Node, expectedSyntax: String)(implicit pos: source.Position): Unit =
      normalized(node.toString) shouldBe normalized(expectedSyntax)

    def client(prefix: String) =
      s"""public class ${prefix}Client {
         |
         |    public static class Builder {
         |
         |        private static final URI DEFAULT_BASE_URL = URI.create("http://localhost:1234");
         |
         |        private URI baseUrl = DEFAULT_BASE_URL;
         |
         |        private java.util.Optional<Function<Request, java.util.concurrent.CompletionStage<Response>>> httpClient = java.util.Optional.empty();
         |
         |        private java.util.Optional<ObjectMapper> objectMapper = java.util.Optional.empty();
         |
         |        public Builder() {
         |        }
         |
         |        public Builder withBaseUrl(final URI baseUrl) {
         |            this.baseUrl = requireNonNull(baseUrl, "baseUrl is required");
         |            return this;
         |        }
         |
         |        public Builder withHttpClient(final Function<Request, java.util.concurrent.CompletionStage<Response>> httpClient) {
         |            this.httpClient = java.util.Optional.ofNullable(httpClient);
         |            return this;
         |        }
         |
         |        public Builder withObjectMapper(final ObjectMapper objectMapper) {
         |            this.objectMapper = java.util.Optional.ofNullable(JacksonSupport.configureObjectMapper(objectMapper));
         |            return this;
         |        }
         |
         |        private Function<Request, java.util.concurrent.CompletionStage<Response>> getHttpClient() {
         |            return this.httpClient.orElseGet(() -> AsyncHttpClientSupport.createHttpClient(AsyncHttpClientSupport.createDefaultAsyncHttpClient()));
         |        }
         |
         |        private ObjectMapper getObjectMapper() {
         |            return this.objectMapper.orElseGet(() -> JacksonSupport.configureObjectMapper(new ObjectMapper()));
         |        }
         |
         |        public ${prefix}Client build() {
         |            return new ${prefix}Client(this);
         |        }
         |    }
         |
         |    public static class GetUserCallBuilder {
         |
         |        private final RequestBuilder builder;
         |
         |        private final Function<Request, java.util.concurrent.CompletionStage<Response>> httpClient;
         |
         |        private final ObjectMapper objectMapper;
         |
         |        private GetUserCallBuilder(final RequestBuilder builder, final Function<Request, java.util.concurrent.CompletionStage<Response>> httpClient, final ObjectMapper objectMapper) {
         |            this.builder = builder;
         |            this.httpClient = httpClient;
         |            this.objectMapper = objectMapper;
         |        }
         |
         |        public GetUserCallBuilder withHeader(final String name, final String value) {
         |            this.builder.addHeader(name, value);
         |            return this;
         |        }
         |
         |        public java.util.concurrent.CompletionStage<GetUserResponse> call() throws ClientException {
         |            return this.httpClient.apply(builder.build()).thenApply(response -> {
         |                switch(response.getStatusCode()) {
         |                    case 200 -> { return new GetUserResponse.Ok(); }
         |                    default -> throw new HttpError(response);
         |                }
         |            });
         |        }
         |    }
         |
         |    private final URI baseUrl;
         |
         |    private final Function<Request, java.util.concurrent.CompletionStage<Response>> httpClient;
         |
         |    private final ObjectMapper objectMapper;
         |
         |    private ${prefix}Client(final Builder builder) {
         |        this.baseUrl = builder.baseUrl;
         |        this.httpClient = builder.getHttpClient();
         |        this.objectMapper = builder.getObjectMapper();
         |    }
         |
         |    public GetUserCallBuilder getUser(final String id) {
         |        final RequestBuilder builder = new RequestBuilder("GET").setUrl(this.baseUrl.toString() + "/user/" + Shower.getInstance().show(id));
         |        return new GetUserCallBuilder(builder, this.httpClient, this.objectMapper);
         |    }
         |}""".stripMargin

    def handler(prefix: String) =
      s"""public interface ${prefix}Handler {
         |
         |    @javax.annotation.Generated(value = "dev.guardrail.generators.java.dropwizard.DropwizardServerGenerator")
         |    abstract class GetUserResponse {
         |
         |        private final int statusCode;
         |
         |        GetUserResponse(final int statusCode) {
         |            this.statusCode = statusCode;
         |        }
         |
         |        public int getStatusCode() {
         |            return this.statusCode;
         |        }
         |
         |        @javax.annotation.Generated(value = "dev.guardrail.generators.java.dropwizard.DropwizardServerGenerator")
         |        public static class Ok extends GetUserResponse {
         |
         |            private Ok() {
         |                super(200);
         |            }
         |        }
         |
         |        public static final Ok Ok = new Ok();
         |    }
         |
         |    java.util.concurrent.CompletionStage<${prefix}Handler.GetUserResponse> getUser(final String id);
         |}""".stripMargin

    def resource(prefix: String) =
      s"""@Path("/user/{id}")
         |public class ${prefix}Resource {
         |
         |    private static final Logger logger = LoggerFactory.getLogger(${prefix}Resource.class);
         |
         |    private final ${prefix}Handler handler;
         |
         |    @Inject
         |    public ${prefix}Resource(final ${prefix}Handler handler) {
         |        this.handler = handler;
         |    }
         |
         |    @GET
         |    public void getUser(@NotNull @PathParam("id") final String id, @Suspended final AsyncResponse asyncResponse) {
         |        this.handler.getUser(id).whenComplete((result, err) -> {
         |            if (err != null) {
         |                logger.error("${prefix}Handler.getUser threw an exception ({}): {}", err.getClass().getName(), err.getMessage(), err);
         |                asyncResponse.resume(Response.status(500).build());
         |            } else {
         |                final Response.ResponseBuilder builder = Response.status(result.getStatusCode());
         |                asyncResponse.resume(builder.build());
         |            }
         |        });
         |    }
         |}""".stripMargin

  }

  private def normalized(s: String): String = s.replaceAll("(?s)\\s+", " ").trim
}
