public class Shower {
    @SuppressWarnings("serial")
    public static class UnshowableInstanceException extends RuntimeException {
        public UnshowableInstanceException(final Object instance) {
            super("Instance of type " + instance.getClass().getName() + " is not showable");
        }
    }

    public static interface Showable<T> {
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

    @SuppressWarnings("unchecked")
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
