package support;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import io.dropwizard.jersey.validation.Validators;
import io.dropwizard.testing.junit.ResourceTestRule;
import io.dropwizard.vavr.jersey.*;
import io.dropwizard.vavr.validation.ValueValidatedValueUnwrapper;
import io.vavr.jackson.datatype.VavrModule;
import org.glassfish.jersey.test.grizzly.GrizzlyTestContainerFactory;

public class VavrHelpers {
    public static ResourceTestRule.Builder newResourceTestRuleBuilder() {
        return new ResourceTestRule.Builder()
            .setTestContainerFactory(new GrizzlyTestContainerFactory())
            .addProvider(EmptyValueExceptionMapper.class)
            .addProvider(LazyParamFeature.class)
            .addProvider(OptionParamFeature.class)
            .addProvider(CollectionParamFeature.class)
            .addProvider(EitherMessageBodyWriter.class)
            .addProvider(ValueMessageBodyWriter.class)
            .setValidator(Validators.newConfiguration().addValidatedValueHandler(new ValueValidatedValueUnwrapper()).buildValidatorFactory().getValidator())
            .setMapper(new ObjectMapper().registerModules(new VavrModule(), new JavaTimeModule()));
    }
}
