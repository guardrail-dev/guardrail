package support;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

public class PositiveLong {
    @JsonCreator
    public static PositiveLong parse(final String s) {
        final long value = Long.parseLong(s);
        if (value < 0) {
            throw new IllegalArgumentException(value + " is not positive");
        }
        return new PositiveLong(value);
    }

    private final long value;

    private PositiveLong(final long value) {
        this.value = value;
    }

    public long getValue() {
        return this.value;
    }

    @JsonValue
    public String toString() {
        return String.valueOf(getValue());
    }
}
