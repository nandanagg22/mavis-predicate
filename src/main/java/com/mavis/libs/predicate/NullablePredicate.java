package com.mavis.libs.predicate;

import java.util.function.Predicate;

/**
 * This Predicate Class extends java's {@link Predicate}
 * to override the the way nulls are handled. The classic
 * java {@link Predicate}  expects a primitive boolean to be returned by
 * test() method. {@link NullablePredicate} gives the implementor a
 * liberty to return nulls also by expecting the {@link Boolean}
 * object. if the returned value is null, it wont be considered in
 * evaluating the filter.
 * @param <T>
 */
@FunctionalInterface
public interface NullablePredicate<T> extends Predicate<T> {
    Boolean myTest(T t);

    @Override
    default boolean test(T t) {
        Boolean b = myTest(t);
        return b != null ? b : false;
    }

    @Override
    default Predicate<T> negate() {
        return t -> {
            Boolean b = myTest(t);
            return  b == null ? false : !b;
        };
    }
}

