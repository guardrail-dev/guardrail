package com.twilio.guardrail.generators.Java

import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.`type`.ClassOrInterfaceType

object AsyncHttpClientHelpers {
  val DEFAULT_ASYNC_HTTP_CLIENT_CONFIG_BUILDER_TYPE: ClassOrInterfaceType = StaticJavaParser.parseClassOrInterfaceType("DefaultAsyncHttpClientConfig.Builder")
  val DEFAULT_ASYNC_HTTP_CLIENT_TYPE: ClassOrInterfaceType                = StaticJavaParser.parseClassOrInterfaceType("DefaultAsyncHttpClient")
  val ASYNC_HTTP_CLIENT_TYPE: ClassOrInterfaceType                        = StaticJavaParser.parseClassOrInterfaceType("AsyncHttpClient")
  val ASYNC_HTTP_CLIENT_CONFIG_TYPE: ClassOrInterfaceType                 = StaticJavaParser.parseClassOrInterfaceType("AsyncHttpClientConfig")
  val REQUEST_BUILDER_TYPE: ClassOrInterfaceType                          = StaticJavaParser.parseClassOrInterfaceType("RequestBuilder")
  val REQUEST_TYPE: ClassOrInterfaceType                                  = StaticJavaParser.parseClassOrInterfaceType("Request")
  val RESPONSE_TYPE: ClassOrInterfaceType                                 = StaticJavaParser.parseClassOrInterfaceType("Response")
  val FILE_PART_TYPE: ClassOrInterfaceType                                = StaticJavaParser.parseClassOrInterfaceType("FilePart")
  val STRING_PART_TYPE: ClassOrInterfaceType                              = StaticJavaParser.parseClassOrInterfaceType("StringPart")
}
