package dev.guardrail.generators.Java

import dev.guardrail.{ SupportDefinition, Target }
import dev.guardrail.generators.syntax.Java._
import dev.guardrail.languages.JavaLanguage

object SerializationHelpers {
  def showerSupportDef: Target[SupportDefinition[JavaLanguage]] = loadSupportDefinitionFromString(
    "Shower",
    s"""
      ${generatedAnnotationString(getClass)}
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
              register(java.util.UUID.class, java.util.UUID::toString);
          }
      }
    """
  )

  def guardrailJerseySupportDef: Target[SupportDefinition[JavaLanguage]] = loadSupportDefinitionFromString(
    "GuardrailJerseySupport",
    s"""
      import io.dropwizard.jersey.params.AbstractParam;
      import io.dropwizard.setup.Bootstrap;
      import io.dropwizard.setup.Environment;
      import org.glassfish.hk2.utilities.binding.AbstractBinder;

      import javax.annotation.Nullable;
      import javax.ws.rs.BadRequestException;
      import java.time.Duration;
      import java.time.Instant;
      import java.time.LocalDate;
      import java.time.LocalDateTime;
      import java.time.LocalTime;
      import java.time.OffsetDateTime;
      import java.time.OffsetTime;
      import java.time.ZonedDateTime;
      import java.util.Objects;

      ${generatedAnnotationString(getClass)}
      public class GuardrailJerseySupport {
          public static class Jsr310 {
              public abstract static class GuardrailAbstractParam<T> extends AbstractParam<T> {
                  private final T value;

                  @SuppressWarnings("unused")
                  protected GuardrailAbstractParam(@Nullable final String input) {
                      this(input, "Parameter");
                  }

                  protected GuardrailAbstractParam(@Nullable final String input, final String parameterName) {
                      super(input, parameterName);
                      try {
                          this.value = realParse(input);
                      } catch (final Exception e) {
                          throw new BadRequestException(String.format("%s is invalid: %s", parameterName, input));
                      }
                  }

                  @Override
                  protected T parse(@Nullable final String input) {
                      return null;
                  }

                  protected abstract T realParse(@Nullable final String input) throws Exception;

                  @Override
                  public T get() {
                      return this.value;
                  }

                  @Override
                  public boolean equals(final Object obj) {
                      if (this == obj) {
                          return true;
                      } else if (getClass() != obj.getClass()) {
                          return false;
                      } else {
                          return this.value.equals(((GuardrailAbstractParam<?>) obj).value);
                      }
                  }

                  @Override
                  public int hashCode() {
                      return Objects.hashCode(this.value);
                  }

                  @Override
                  public String toString() {
                      return this.value != null ? this.value.toString() : "(null)";
                  }
              }

              @SuppressWarnings("unused")
              public static class InstantParam extends GuardrailAbstractParam<Instant> {
                  public InstantParam(@Nullable final String input, final String parameterName) {
                      super(input, parameterName);
                  }

                  @Override
                  protected Instant realParse(final String input) {
                      return Instant.parse(input);
                  }
              }

              @SuppressWarnings("unused")
              public static class OffsetDateTimeParam extends GuardrailAbstractParam<OffsetDateTime> {
                  public OffsetDateTimeParam(@Nullable final String input, final String parameterName) {
                      super(input, parameterName);
                  }

                  @Override
                  protected OffsetDateTime realParse(final String input) {
                      return OffsetDateTime.parse(input);
                  }
              }

              @SuppressWarnings("unused")
              public static class ZonedDateTimeParam extends GuardrailAbstractParam<ZonedDateTime> {
                  public ZonedDateTimeParam(@Nullable final String input, final String parameterName) {
                      super(input, parameterName);
                  }

                  @Override
                  protected ZonedDateTime realParse(final String input) {
                      return ZonedDateTime.parse(input);
                  }
              }

              @SuppressWarnings("unused")
              public static class LocalDateTimeParam extends GuardrailAbstractParam<LocalDateTime> {
                  public LocalDateTimeParam(@Nullable final String input, final String parameterName) {
                      super(input, parameterName);
                  }

                  @Override
                  protected LocalDateTime realParse(final String input) {
                      return LocalDateTime.parse(input);
                  }
              }

              @SuppressWarnings("unused")
              public static class LocalDateParam extends GuardrailAbstractParam<LocalDate> {
                  public LocalDateParam(@Nullable final String input, final String parameterName) {
                      super(input, parameterName);
                  }

                  @Override
                  protected LocalDate realParse(final String input) {
                      return LocalDate.parse(input);
                  }
              }

              @SuppressWarnings("unused")
              public static class LocalTimeParam extends GuardrailAbstractParam<LocalTime> {
                  public LocalTimeParam(@Nullable final String input, final String parameterName) {
                      super(input, parameterName);
                  }

                  @Override
                  protected LocalTime realParse(final String input) {
                      return LocalTime.parse(input);
                  }
              }

              @SuppressWarnings("unused")
              public static class OffsetTimeParam extends GuardrailAbstractParam<OffsetTime> {
                  public OffsetTimeParam(@Nullable final String input, final String parameterName) {
                      super(input, parameterName);
                  }

                  @Override
                  protected OffsetTime realParse(final String input) {
                      return OffsetTime.parse(input);
                  }
              }

              @SuppressWarnings("unused")
              public static class DurationParam extends GuardrailAbstractParam<Duration> {
                  public DurationParam(@Nullable final String input, final String parameterName) {
                      super(input, parameterName);
                  }

                  @Override
                  protected Duration realParse(final String input) {
                      return Duration.parse(input);
                  }
              }

              @Deprecated
              public static class Binder extends AbstractBinder {
                  @Override
                  protected void configure() {}
              }

              @Deprecated
              public static class Bundle implements io.dropwizard.Bundle {
                  @Override
                  public void initialize(final Bootstrap<?> bootstrap) {}

                  @Override
                  public void run(final Environment environment) {}
              }
          }

          private GuardrailJerseySupport() {}
      }
    """
  )
}
