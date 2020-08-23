package helpers

import com.datasift.dropwizard.scala.jersey.inject.{ EitherMessageBodyWriter, OptionMessageBodyWriter, ScalaInjectionBinder, TryMessageBodyWriter }
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.introspect.{ AnnotationIntrospectorPair, JacksonAnnotationIntrospector }
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.introspect.ScalaAnnotationIntrospector

object ScalaHelpers {
  implicit class ResourceTestSupportBuilderExtensions(private val rtsb: ResourceTestSupport.Builder) extends AnyVal {
    def addScalaProviders(): ResourceTestSupport.Builder =
      rtsb
        .addProvider(new OptionMessageBodyWriter)
        .addProvider(new TryMessageBodyWriter)
        .addProvider(new EitherMessageBodyWriter)
        .addProvider(new ScalaInjectionBinder)
  }

  def createObjectMapper(): ObjectMapper =
    new ObjectMapper()
      .registerModule(DefaultScalaModule)
      .registerModule(new JavaTimeModule)
      .setAnnotationIntrospector(new AnnotationIntrospectorPair(ScalaAnnotationIntrospector, new JacksonAnnotationIntrospector))
}
