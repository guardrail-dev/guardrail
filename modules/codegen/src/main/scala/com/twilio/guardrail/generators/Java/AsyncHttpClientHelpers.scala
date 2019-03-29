package com.twilio.guardrail.generators.Java

import com.github.javaparser.JavaParser
import com.github.javaparser.ast.`type`.ClassOrInterfaceType

object AsyncHttpClientHelpers {
  val DEFAULT_ASYNC_HTTP_CLIENT_CONFIG_BUILDER_TYPE: ClassOrInterfaceType = JavaParser.parseClassOrInterfaceType("DefaultAsyncHttpClientConfig.Builder")
  val DEFAULT_ASYNC_HTTP_CLIENT_TYPE: ClassOrInterfaceType                = JavaParser.parseClassOrInterfaceType("DefaultAsyncHttpClient")
  val ASYNC_HTTP_CLIENT_TYPE: ClassOrInterfaceType                        = JavaParser.parseClassOrInterfaceType("AsyncHttpClient")
  val ASYNC_HTTP_CLIENT_CONFIG_TYPE: ClassOrInterfaceType                 = JavaParser.parseClassOrInterfaceType("AsyncHttpClientConfig")
  val REQUEST_BUILDER_TYPE: ClassOrInterfaceType                          = JavaParser.parseClassOrInterfaceType("RequestBuilder")
  val REQUEST_TYPE: ClassOrInterfaceType                                  = JavaParser.parseClassOrInterfaceType("Request")
  val RESPONSE_TYPE: ClassOrInterfaceType                                 = JavaParser.parseClassOrInterfaceType("Response")
  val FILE_PART_TYPE: ClassOrInterfaceType                                = JavaParser.parseClassOrInterfaceType("FilePart")
  val STRING_PART_TYPE: ClassOrInterfaceType                              = JavaParser.parseClassOrInterfaceType("StringPart")
}
