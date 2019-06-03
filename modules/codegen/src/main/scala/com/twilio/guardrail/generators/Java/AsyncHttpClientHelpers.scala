package com.twilio.guardrail.generators.Java

import com.github.javaparser.JavaParser
import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.twilio.guardrail.{ SupportDefinition, Target }
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.generators.syntax.Java.loadSupportDefinitionFromString

object AsyncHttpClientHelpers {
  val DEFAULT_ASYNC_HTTP_CLIENT_CONFIG_BUILDER_TYPE: ClassOrInterfaceType = JavaParser.parseClassOrInterfaceType("DefaultAsyncHttpClientConfig.Builder")
  val DEFAULT_ASYNC_HTTP_CLIENT_TYPE: ClassOrInterfaceType                = JavaParser.parseClassOrInterfaceType("DefaultAsyncHttpClient")
  val ASYNC_HTTP_CLIENT_TYPE: ClassOrInterfaceType                        = JavaParser.parseClassOrInterfaceType("AsyncHttpClient")
  val ASYNC_HTTP_CLIENT_CONFIG_TYPE: ClassOrInterfaceType                 = JavaParser.parseClassOrInterfaceType("AsyncHttpClientConfig")
  val REQUEST_BUILDER_TYPE: ClassOrInterfaceType                          = JavaParser.parseClassOrInterfaceType("RequestBuilder")
  val REQUEST_TYPE: ClassOrInterfaceType                                  = JavaParser.parseClassOrInterfaceType("Request")
  val RESPONSE_TYPE: ClassOrInterfaceType                                 = JavaParser.parseClassOrInterfaceType("Response")
  val FILE_PART_TYPE: ClassOrInterfaceType                                = JavaParser.parseClassOrInterfaceType("FilePart")
  val INPUT_STREAM_PART_TYPE: ClassOrInterfaceType                        = JavaParser.parseClassOrInterfaceType("InputStreamPart")
  val STRING_PART_TYPE: ClassOrInterfaceType                              = JavaParser.parseClassOrInterfaceType("StringPart")

  def inputStreamPartDef: Target[SupportDefinition[JavaLanguage]] = loadSupportDefinitionFromString(
    "InputStreamPart",
    """
      import java.io.ByteArrayOutputStream;
      import java.io.FileInputStream;
      import java.io.IOException;
      import java.io.InputStream;
      import java.nio.charset.Charset;
      import java.util.Arrays;
      import org.asynchttpclient.request.body.multipart.ByteArrayPart;

      /**
       * An AsyncHttpClient multipart part that takes an input stream.
       *
       * Unfortunately, AHC doesn't let us register custom part types, so we have
       * to base this on ByteArrayPart, which means we don't actually stream the
       * part; it has to be completely buffered into memory before sending.
       *
       * Unfortunately, this also means that the constructor can block the calling
       * thread if the passed InputStream can block on read.
       */
      public class InputStreamPart extends ByteArrayPart {
          private static final int BUFFER_SIZE = 1024 * 1024;
          private static final int BUFFER_SIZE_MAX = BUFFER_SIZE * 50;

          private static byte[] streamToBytes(final InputStream stream) {
              try {
                  if (stream instanceof FileInputStream) {
                      final long length = ((FileInputStream) stream).getChannel().size();
                      if (length > 0 && length <= Integer.MAX_VALUE) {
                          final byte[] buf = new byte[(int) length];
                          final int actualLength = stream.read(buf);
                          if (actualLength != length) {
                              return Arrays.copyOf(buf, actualLength);
                          } else {
                              return buf;
                          }
                      }
                  }

                  final ByteArrayOutputStream accum = new ByteArrayOutputStream();

                  final int bufferSize;
                  if (stream.available() > BUFFER_SIZE) {
                      bufferSize = Math.min(BUFFER_SIZE_MAX, stream.available());
                  } else {
                      bufferSize = BUFFER_SIZE;
                  }
                  final byte[] buf = new byte[bufferSize];

                  while (true) {
                      final int read = stream.read(buf);
                      if (read == -1) {
                          break;
                      }
                      accum.write(buf, 0, read);
                  }

                  return accum.toByteArray();
              } catch (final IOException e) {
                  throw new IllegalArgumentException("Unable to read from input stream: " + e.getMessage(), e);
              }
          }

          public InputStreamPart(final String name, final InputStream stream) {
              super(name, streamToBytes(stream));
          }

          public InputStreamPart(final String name, final InputStream stream, final String contentType) {
              super(name, streamToBytes(stream), contentType);
          }

          public InputStreamPart(final String name, final InputStream stream, final String contentType, final Charset charset) {
              super(name, streamToBytes(stream), contentType, charset);
          }

          public InputStreamPart(final String name, final InputStream stream, final String contentType, final Charset charset, final String fileName) {
              super(name, streamToBytes(stream), contentType, charset, fileName);
          }

          public InputStreamPart(final String name, final InputStream stream, final String contentType, final Charset charset, final String fileName, final String contentId) {
              super(name, streamToBytes(stream), contentType, charset, fileName, contentId);
          }

          public InputStreamPart(final String name, final InputStream stream, final String contentType, final Charset charset, final String fileName, final String contentId, final String transferEncoding) {
              super(name, streamToBytes(stream), contentType, charset, fileName, contentId, transferEncoding);
          }
      }
     """
  )
}
