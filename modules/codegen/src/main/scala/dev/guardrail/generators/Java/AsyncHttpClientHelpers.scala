package dev.guardrail.generators.Java

import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import dev.guardrail.{ SupportDefinition, Target }
import dev.guardrail.generators.syntax.Java._
import dev.guardrail.languages.JavaLanguage

object AsyncHttpClientHelpers {
  val DEFAULT_ASYNC_HTTP_CLIENT_CONFIG_BUILDER_TYPE: ClassOrInterfaceType = StaticJavaParser.parseClassOrInterfaceType("DefaultAsyncHttpClientConfig.Builder")
  val DEFAULT_ASYNC_HTTP_CLIENT_TYPE: ClassOrInterfaceType                = StaticJavaParser.parseClassOrInterfaceType("DefaultAsyncHttpClient")
  val ASYNC_HTTP_CLIENT_TYPE: ClassOrInterfaceType                        = StaticJavaParser.parseClassOrInterfaceType("AsyncHttpClient")
  val ASYNC_HTTP_CLIENT_CONFIG_TYPE: ClassOrInterfaceType                 = StaticJavaParser.parseClassOrInterfaceType("AsyncHttpClientConfig")
  val REQUEST_BUILDER_TYPE: ClassOrInterfaceType                          = StaticJavaParser.parseClassOrInterfaceType("RequestBuilder")
  val REQUEST_TYPE: ClassOrInterfaceType                                  = StaticJavaParser.parseClassOrInterfaceType("Request")
  val RESPONSE_TYPE: ClassOrInterfaceType                                 = StaticJavaParser.parseClassOrInterfaceType("Response")
  val INPUT_STREAM_PART_TYPE: ClassOrInterfaceType                        = StaticJavaParser.parseClassOrInterfaceType("InputStreamPart")
  val STRING_PART_TYPE: ClassOrInterfaceType                              = StaticJavaParser.parseClassOrInterfaceType("StringPart")

  def asyncHttpClientUtilsSupportDef: Target[SupportDefinition[JavaLanguage]] = loadSupportDefinitionFromString(
    "AsyncHttpClientUtils",
    """
      import org.asynchttpclient.Response;

      import java.nio.charset.Charset;
      import java.util.Locale;
      import java.util.Optional;

      public class AsyncHttpClientUtils {
          public static Optional<Charset> getResponseCharset(final Response response) {
              return Optional.ofNullable(response.getHeader("Content-Type")).flatMap(contentType -> {
                  final int charsetStart = contentType.toLowerCase(Locale.US).indexOf("charset=");
                  if (charsetStart != -1) {
                      final int charsetEnd = contentType.indexOf(";", charsetStart + 8);
                      final String charsetStr = contentType.substring(
                              charsetStart + 8,
                              charsetEnd != -1 ? charsetEnd : contentType.length());
                      try {
                          return Optional.of(Charset.forName(charsetStr));
                      } catch (final Exception e) {
                          return Optional.empty();
                      }
                  } else {
                      return Optional.empty();
                  }
              });
          }
      }
    """
  )
}
