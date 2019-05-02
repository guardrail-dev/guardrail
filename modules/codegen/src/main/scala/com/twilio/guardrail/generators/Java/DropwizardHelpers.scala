package com.twilio.guardrail.generators.Java

import com.twilio.guardrail.{ SupportDefinition, Target }
import com.twilio.guardrail.generators.syntax.Java.loadSupportDefinitionFromString
import com.twilio.guardrail.languages.JavaLanguage

object DropwizardHelpers {
  def httpSecurityUtilsSupportDef: Target[SupportDefinition[JavaLanguage]] = loadSupportDefinitionFromString(
    "HttpSecurityUtils",
    """
      import java.nio.charset.StandardCharsets;
      import java.util.Base64;
      import java.util.Optional;
      import java.util.Locale;

      public class HttpSecurityUtils {
          public static class HttpBasicCredentials {
              public static Optional<HttpBasicCredentials> parse(final Optional<String> authHeader) {
                  return authHeader.flatMap(hdr -> {
                      final String[] parts = hdr.trim().split("\\s+");
                      if (parts.length == 2) {
                          if ("basic".equals(parts[0].toLowerCase(Locale.US))) {
                              final String userPass = new String(Base64.getDecoder().decode(parts[1].trim()), StandardCharsets.UTF_8);
                              final String[] userPassParts = userPass.split(":", 2);
                              if (userPassParts.length == 2) {
                                  return Optional.of(new HttpBasicCredentials(userPassParts[0], userPassParts[1]));
                              } else {
                                  return Optional.of(new HttpBasicCredentials(userPassParts[0], ""));
                              }
                          } else {
                              return Optional.empty();
                          }
                      } else {
                          return Optional.empty();
                      }
                  });
              }

              private final String username;
              private final String password;

              private HttpBasicCredentials(final String username, final String password) {
                  this.username = username;
                  this.password = password;
              }

              public String getUsername() {
                  return this.username;
              }

              public String getPassword() {
                  return this.password;
              }
          }

          public static class HttpBearerCredentials {
              public static Optional<HttpBearerCredentials> parse(final Optional<String> authHeader) {
                  return authHeader.flatMap(hdr -> {
                      final String[] parts = hdr.trim().split("\\s+");
                      if (parts.length == 2) {
                          if ("bearer".equals(parts[0].toLowerCase(Locale.US))) {
                              return Optional.of(new HttpBearerCredentials(parts[1]));
                          } else {
                              return Optional.empty();
                          }
                      } else {
                          return Optional.empty();
                      }
                  });
              }

              private final String token;

              private HttpBearerCredentials(final String token) {
                  this.token = token;
              }

              public String getToken() {
                  return this.token;
              }
          }

          private HttpSecurityUtils() {}
      }
    """
  )
}
