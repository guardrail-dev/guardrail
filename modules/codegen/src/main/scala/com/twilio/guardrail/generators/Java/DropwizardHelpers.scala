package com.twilio.guardrail.generators.Java

import com.twilio.guardrail.generators.syntax.Java.loadSupportDefinitionFromString
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.{SupportDefinition, Target}

object DropwizardHelpers {
  def authPrincipalSupportDef(classPrefix: String): Target[SupportDefinition[JavaLanguage]] = loadSupportDefinitionFromString(
    s"${classPrefix}AuthPrincipal",
    s"""
      import java.security.Principal;
      import java.util.Objects;

      import static java.util.Objects.requireNonNull;

      public class ${classPrefix}AuthPrincipal<T> implements Principal {
          private final String name;
          private final T data;

          public ${classPrefix}AuthPrincipal(final String name, final T data) {
              this.name = requireNonNull(name);
              this.data = requireNonNull(data);
          }

          public T getData() {
              return this.data;
          }

          @Override
          public String getName() {
              return this.name;
          }

          @Override
          public boolean equals(Object o) {
              if (this == o) return true;
              if (o == null || getClass() != o.getClass()) return false;
              final ${classPrefix}AuthPrincipal<?> that = (${classPrefix}AuthPrincipal<?>) o;
              return this.name.equals(that.name) &&
                      this.data.equals(that.data);
          }

          @Override
          public int hashCode() {
              return Objects.hash(this.name, this.data);
          }

          @Override
          public String toString() {
              return this.name;
          }
      }
    """
  )

  def apiKeyQueryAuthPrincipalSupportDef: Target[SupportDefinition[JavaLanguage]] = authPrincipalSupportDef("ApiKeyQuery")
  def apiKeyHeaderAuthPrincipalSupportDef: Target[SupportDefinition[JavaLanguage]] = authPrincipalSupportDef("ApiKeyHeader")
  def apiKeyCookieAuthPrincipalSupportDef: Target[SupportDefinition[JavaLanguage]] = authPrincipalSupportDef("ApiKeyCookie")
  def httpBasicAuthPrincipalSupportDef: Target[SupportDefinition[JavaLanguage]] = authPrincipalSupportDef("HttpBasic")
  def httpBearerAuthPrincipalSupportDef: Target[SupportDefinition[JavaLanguage]] = authPrincipalSupportDef("HttpBearer")
  def oauthAuthPrincipalSupportDef: Target[SupportDefinition[JavaLanguage]] = authPrincipalSupportDef("OAuth")
  def openIdConnectAuthPrincipalSupportDef: Target[SupportDefinition[JavaLanguage]] = authPrincipalSupportDef("OpenIdConnect")

  def apiKeyAuthFilterSupportDef: Target[SupportDefinition[JavaLanguage]] = loadSupportDefinitionFromString(
    "ApiKeyAuthFilter",
    """
      import io.dropwizard.auth.AuthFilter;
      import javax.ws.rs.WebApplicationException;
      import javax.ws.rs.container.ContainerRequestContext;
      import javax.ws.rs.core.Cookie;
      import java.io.IOException;
      import java.security.Principal;
      import java.util.Optional;

      import static java.util.Objects.requireNonNull;

      public abstract class ApiKeyAuthFilter<P extends Principal> extends AuthFilter<String, P> {
          protected enum In {
              QUERY,
              HEADER,
              COOKIE
          }

          protected final String name;
          protected final In in;

          protected ApiKeyAuthFilter(final String name, final In in) {
              this.name = requireNonNull(name);
              this.in = requireNonNull(in);
          }

          @Override
          public void filter(final ContainerRequestContext requestContext) throws IOException {
              final Optional<String> apiKey = getApiKey(requestContext);
              if (!authenticate(requestContext, apiKey.orElse(null), "apiKey")) {
                  throw new WebApplicationException(unauthorizedHandler.buildResponse(prefix, realm));
              }
          }

          private Optional<String> getApiKey(final ContainerRequestContext requestContext) {
              switch (this.in) {
                  case QUERY:
                      return Optional.ofNullable(requestContext.getUriInfo().getQueryParameters().getFirst(this.name));
                  case HEADER:
                      return Optional.ofNullable(requestContext.getHeaders().getFirst(this.name));
                  case COOKIE:
                      return Optional.ofNullable(requestContext.getCookies().get(this.name)).map(Cookie::getValue);
                  default:
                      throw new IllegalStateException("This should never happen");
              }
          }

          public static abstract class ApiKeyAuthFilterBuilder<P extends Principal, T extends ApiKeyAuthFilter<P>>
                  extends AuthFilterBuilder<String, P, T>
          {
              public ApiKeyAuthFilterBuilder() {
                  super();
                  setPrefix("apiKey");
              }
          }
      }
    """
  )
}
