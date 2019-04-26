package com.twilio.guardrail.generators.Java

import com.twilio.guardrail.{ SupportDefinition, Target }
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.languages.JavaLanguage

object SerializationHelpers {
  def showerSupportDef: Target[SupportDefinition[JavaLanguage]] = loadSupportDefinitionFromString(
    "Shower",
    """
      public class Shower {
          @SuppressWarnings("serial")
          public static class UnshowableInstanceException extends RuntimeException {
              public UnshowableInstanceException(final Object instance) {
                  super("Instance of type " + instance.getClass().getName() + " is not showable");
              }
          }

          public interface Showable<T> {
              String show(T value);
          }

          private static final Shower instance = new Shower();

          public static Shower getInstance() {
              return instance;
          }

          private final java.util.Map<Class<?>, Showable<?>> showables = new java.util.concurrent.ConcurrentHashMap<>();

          private Shower() {
              registerDefaultInstances();
          }

          public <T> void register(final Class<T> cls, final Showable<T> showable) {
              this.showables.put(cls, showable);
          }

          @SuppressWarnings("unchecked")
          public String show(final Object value) {
              return show(value, (Class<Object>)value.getClass());
          }

          public boolean canShow(final Class<?> cls) {
              if (this.showables.containsKey(cls)) {
                  return true;
              } else {
                  final Class<?> superclass = cls.getSuperclass();
                  if (superclass != null) {
                      return canShow(superclass);
                  } else {
                      return false;
                  }
              }
          }

          @SuppressWarnings("unchecked")
          private String show(final Object value, final Class<Object> cls) {
              if (this.showables.containsKey(cls)) {
                  final Showable<Object> showable = (Showable<Object>)this.showables.get(cls);
                  return showable.show(value);
              } else {
                  final Class<Object> superclass = cls.getSuperclass();
                  if (superclass != null) {
                      return show(value, superclass);
                  } else {
                      throw new UnshowableInstanceException(value);
                  }
              }
          }

          private void registerDefaultInstances() {
              register(Boolean.class, String::valueOf);
              register(Byte.class, String::valueOf);
              register(Character.class, String::valueOf);
              register(Short.class, String::valueOf);
              register(Integer.class, String::valueOf);
              register(Long.class, String::valueOf);
              register(java.math.BigInteger.class, java.math.BigInteger::toString);
              register(Float.class, String::valueOf);
              register(Double.class, String::valueOf);
              register(java.math.BigDecimal.class, java.math.BigDecimal::toString);
              register(String.class, value -> value);
              register(java.time.LocalDate.class, value -> value.format(java.time.format.DateTimeFormatter.ISO_DATE));
              register(java.time.OffsetDateTime.class, value -> value.format(java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME));
              register(java.net.URL.class, java.net.URL::toString);
              register(java.net.URI.class, java.net.URI::toString);
          }
      }
    """
  )

  def guardrailJerseySupportDef: Target[SupportDefinition[JavaLanguage]] = loadSupportDefinitionFromString(
    "GuardrailJerseySupport",
    """
      import io.dropwizard.setup.Bootstrap;
      import io.dropwizard.setup.Environment;
      import org.glassfish.hk2.api.Factory;
      import org.glassfish.hk2.api.InjectionResolver;
      import org.glassfish.hk2.api.ServiceLocator;
      import org.glassfish.hk2.api.TypeLiteral;
      import org.glassfish.hk2.utilities.binding.AbstractBinder;
      import org.glassfish.jersey.internal.inject.ExtractorException;
      import org.glassfish.jersey.server.ParamException;
      import org.glassfish.jersey.server.internal.inject.AbstractContainerRequestValueFactory;
      import org.glassfish.jersey.server.internal.inject.AbstractValueFactoryProvider;
      import org.glassfish.jersey.server.internal.inject.MultivaluedParameterExtractor;
      import org.glassfish.jersey.server.internal.inject.MultivaluedParameterExtractorProvider;
      import org.glassfish.jersey.server.internal.inject.ParamInjectionResolver;
      import org.glassfish.jersey.server.model.Parameter;

      import javax.inject.Inject;
      import javax.inject.Singleton;
      import javax.ws.rs.FormParam;
      import javax.ws.rs.HeaderParam;
      import javax.ws.rs.ProcessingException;
      import javax.ws.rs.QueryParam;
      import javax.ws.rs.WebApplicationException;
      import javax.ws.rs.core.Form;
      import javax.ws.rs.core.MultivaluedHashMap;
      import javax.ws.rs.core.MultivaluedMap;
      import javax.ws.rs.ext.ParamConverter;
      import javax.ws.rs.ext.ParamConverterProvider;
      import java.lang.annotation.Annotation;
      import java.lang.reflect.Type;
      import java.time.Duration;
      import java.time.Instant;
      import java.time.LocalDate;
      import java.time.LocalDateTime;
      import java.time.LocalTime;
      import java.time.OffsetDateTime;
      import java.time.OffsetTime;
      import java.time.ZonedDateTime;
      import java.time.format.DateTimeFormatter;
      import java.time.temporal.TemporalAccessor;
      import java.util.Collections;
      import java.util.HashMap;
      import java.util.Map;
      import java.util.Optional;
      import java.util.function.Function;

      public class GuardrailJerseySupport {
          public static class Jsr310 {
              private static class ParameterExtractor<T> implements MultivaluedParameterExtractor<T> {
                  private final String name;
                  private final ParamConverter<T> converter;

                  ParameterExtractor(final String name, final ParamConverter<T> converter) {
                      this.name = name;
                      this.converter = converter;
                  }

                  @Override
                  public String getName() {
                      return this.name;
                  }

                  @Override
                  public String getDefaultValueString() {
                      return null;
                  }

                  @Override
                  public T extract(final MultivaluedMap<String, String> parameters) {
                      final Optional<String> value = Optional.ofNullable(parameters.getFirst(getName()));
                      try {
                          return value.map(converter::fromString).orElse(null);
                      } catch (final WebApplicationException | ProcessingException ex) {
                          throw ex;
                      } catch (final Exception ex) {
                          throw new ExtractorException(ex);
                      }
                  }
              }

              private static class ParamFactoryProviders {
                  private static abstract class BaseParamFactoryProvider extends AbstractValueFactoryProvider {
                      @Inject
                      BaseParamFactoryProvider(final MultivaluedParameterExtractorProvider mpep,
                                               final ServiceLocator locator,
                                               final Parameter.Source source) {
                          super(mpep, locator, source);
                      }

                      @Override
                      protected Factory<?> createValueFactory(final Parameter parameter) {
                          return Optional.ofNullable(parameter.getSourceName())
                                  .filter(name -> !name.isEmpty())
                                  .flatMap(name -> buildExtractor(parameter.getRawType(), name))
                                  .map(extractor -> buildFactory(extractor, !parameter.isEncoded()))
                                  .orElse(null);
                      }

                      private Optional<MultivaluedParameterExtractor<?>> buildExtractor(final Class<?> cls, final String name) {
                          return Optional.ofNullable(PARAM_CONVERTERS.get(cls))
                                  .map(conv -> new ParameterExtractor<>(name, conv));
                      }

                      protected abstract AbstractContainerRequestValueFactory<?> buildFactory(final MultivaluedParameterExtractor<?> extractor, final boolean decode);
                  }

                  private static class QueryParamFactoryProvider extends BaseParamFactoryProvider {
                      protected QueryParamFactoryProvider(MultivaluedParameterExtractorProvider mpep, ServiceLocator locator, Parameter.Source source) {
                          super(mpep, locator, source);
                      }

                      @Override
                      protected AbstractContainerRequestValueFactory<?> buildFactory(final MultivaluedParameterExtractor<?> extractor, final boolean decode) {
                          return new ValueFactories.QueryParamValueFactory(extractor, decode);
                      }
                  }

                  private static class FormParamFactoryProvider extends BaseParamFactoryProvider {
                      protected FormParamFactoryProvider(MultivaluedParameterExtractorProvider mpep, ServiceLocator locator, Parameter.Source source) {
                          super(mpep, locator, source);
                      }

                      @Override
                      protected AbstractContainerRequestValueFactory<?> buildFactory(final MultivaluedParameterExtractor<?> extractor, final boolean decode) {
                          return new ValueFactories.FormParamValueFactory(extractor);
                      }
                  }

                  private static class HeaderParamFactoryProvider extends BaseParamFactoryProvider {
                      protected HeaderParamFactoryProvider(MultivaluedParameterExtractorProvider mpep, ServiceLocator locator, Parameter.Source source) {
                          super(mpep, locator, source);
                      }

                      @Override
                      protected AbstractContainerRequestValueFactory<?> buildFactory(final MultivaluedParameterExtractor<?> extractor, final boolean decode) {
                          return new ValueFactories.HeaderParamValueFactory(extractor);
                      }
                  }
              }

              private static class ValueFactories {
                  private static class QueryParamValueFactory extends AbstractContainerRequestValueFactory<Object> {
                      private final MultivaluedParameterExtractor<?> extractor;
                      private final boolean decode;

                      QueryParamValueFactory(final MultivaluedParameterExtractor<?> extractor, final boolean decode) {
                          this.extractor = extractor;
                          this.decode = decode;
                      }

                      @Override
                      public Object provide() {
                          try {
                              return this.extractor.extract(getContainerRequest().getUriInfo().getQueryParameters(decode));
                          } catch (final ProcessingException e) {
                              throw new ParamException.QueryParamException(e.getCause(), this.extractor.getName(), this.extractor.getDefaultValueString());
                          }
                      }
                  }

                  private static class FormParamValueFactory extends AbstractContainerRequestValueFactory<Object> {
                      private final MultivaluedParameterExtractor<?> extractor;

                      FormParamValueFactory(final MultivaluedParameterExtractor<?> extractor) {
                          this.extractor = extractor;
                      }

                      @Override
                      public Object provide() {
                          try {
                              getContainerRequest().bufferEntity();
                              final Form form = getContainerRequest().readEntity(Form.class);
                              return extractor.extract(form.asMap());
                          } catch (final ProcessingException e) {
                              throw new ParamException.FormParamException(e.getCause(), this.extractor.getName(), this.extractor.getDefaultValueString());
                          }
                      }
                  }

                  private static class HeaderParamValueFactory extends AbstractContainerRequestValueFactory<Object> {
                      private final MultivaluedParameterExtractor<?> extractor;

                      HeaderParamValueFactory(final MultivaluedParameterExtractor<?> extractor) {
                          this.extractor = extractor;
                      }

                      @Override
                      public Object provide() {
                          try {
                              return extractor.extract(getContainerRequest().getHeaders());
                          } catch (final ProcessingException e) {
                              throw new ParamException.HeaderParamException(e.getCause(), this.extractor.getName(), this.extractor.getDefaultValueString());
                          }
                      }
                  }
              }

              private static class ParamInjectionResolvers {
                  private static class QueryParamInjectionResolver extends ParamInjectionResolver<QueryParam> {
                      public QueryParamInjectionResolver() {
                          super(ParamFactoryProviders.QueryParamFactoryProvider.class);
                      }
                  }

                  private static class FormParamInjectionResolver extends ParamInjectionResolver<FormParam> {
                      public FormParamInjectionResolver() {
                          super(ParamFactoryProviders.FormParamFactoryProvider.class);
                      }
                  }

                  private static class HeaderParamInjectionResolver extends ParamInjectionResolver<HeaderParam> {
                      public HeaderParamInjectionResolver() {
                          super(ParamFactoryProviders.HeaderParamFactoryProvider.class);
                      }
                  }
              }

              private static class Jsr310ParamConverter<T extends TemporalAccessor> implements ParamConverter<T> {
                  private final Function<String, T> parser;
                  private final DateTimeFormatter formatter;

                  Jsr310ParamConverter(final Function<String, T> parser, final DateTimeFormatter formatter) {
                      this.parser = parser;
                      this.formatter = formatter;
                  }

                  @Override
                  public T fromString(final String value) {
                      return parser.apply(value);
                  }

                  @Override
                  public String toString(final T value) {
                      return formatter.format(value);
                  }
              }

              private static class Jsr310DurationParamConverter implements ParamConverter<Duration> {
                  @Override
                  public Duration fromString(final String value) {
                      return Duration.parse(value);
                  }

                  @Override
                  public String toString(final Duration value) {
                      return value.toString();
                  }
              }

              private static final Map<Class<?>, ParamConverter<?>> PARAM_CONVERTERS;

              static {
                  final Map<Class<?>, ParamConverter<?>> paramConverters = new HashMap<>();
                  paramConverters.put(Instant.class, new Jsr310ParamConverter<>(
                          Instant::parse,
                          DateTimeFormatter.ISO_INSTANT
                  ));
                  paramConverters.put(OffsetDateTime.class, new Jsr310ParamConverter<>(
                          OffsetDateTime::parse,
                          DateTimeFormatter.ISO_OFFSET_DATE_TIME
                  ));
                  paramConverters.put(ZonedDateTime.class, new Jsr310ParamConverter<>(
                          ZonedDateTime::parse,
                          DateTimeFormatter.ISO_ZONED_DATE_TIME
                  ));
                  paramConverters.put(LocalDateTime.class, new Jsr310ParamConverter<>(
                          LocalDateTime::parse,
                          DateTimeFormatter.ISO_LOCAL_DATE_TIME
                  ));
                  paramConverters.put(LocalDate.class, new Jsr310ParamConverter<>(
                          LocalDate::parse,
                          DateTimeFormatter.ISO_LOCAL_DATE
                  ));
                  paramConverters.put(LocalTime.class, new Jsr310ParamConverter<>(
                          LocalTime::parse,
                          DateTimeFormatter.ISO_TIME
                  ));
                  paramConverters.put(OffsetTime.class, new Jsr310ParamConverter<>(
                          OffsetTime::parse,
                          DateTimeFormatter.ISO_OFFSET_TIME
                  ));
                  paramConverters.put(Duration.class, new Jsr310DurationParamConverter());
                  PARAM_CONVERTERS = Collections.unmodifiableMap(paramConverters);
              }

              private static class ParamConvertersProvider implements ParamConverterProvider {
                  @Override
                  @SuppressWarnings("unchecked")
                  public <T> ParamConverter<T> getConverter(final Class<T> rawType, final Type genericType, final Annotation[] annotations) {
                      return (ParamConverter<T>) PARAM_CONVERTERS.get(rawType);
                  }
              }

              public static class Binder extends AbstractBinder {
                  @Override
                  protected void configure() {
                      bind(ParamInjectionResolvers.QueryParamInjectionResolver.class)
                              .to(new TypeLiteral<InjectionResolver<QueryParam>>() {
                              })
                              .in(Singleton.class);

                      bind(ParamInjectionResolvers.FormParamInjectionResolver.class)
                              .to(new TypeLiteral<InjectionResolver<FormParam>>() {
                              })
                              .in(Singleton.class);

                      bind(ParamInjectionResolvers.HeaderParamInjectionResolver.class)
                              .to(new TypeLiteral<InjectionResolver<HeaderParam>>() {
                              })
                              .in(Singleton.class);

                      bind(ParamConvertersProvider.class)
                              .to(ParamConverterProvider.class)
                              .in(Singleton.class);
                  }
              }

              public static class Bundle implements io.dropwizard.Bundle {
                  @Override
                  public void initialize(final Bootstrap<?> bootstrap) {
                  }

                  @Override
                  public void run(final Environment environment) {
                      environment.jersey().register(new Binder());
                  }
              }
          }

          private GuardrailJerseySupport() {}
      }
    """
  )
}
