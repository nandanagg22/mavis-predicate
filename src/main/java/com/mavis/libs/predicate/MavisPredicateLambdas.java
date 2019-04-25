package com.mavis.libs.predicate;

import org.apache.commons.beanutils.ConversionException;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * This class contains the equivalent Predicate Lambda expressions for all the
 * Operators listed in {@link Comparison}
 *
 * @param <T>
 * @author Nandan Aggarwal
 */
public class MavisPredicateLambdas<T> {
    private final Class<?> clazz;
    private static Logger LOGGER = LogManager.getLogger(MavisPredicateLambdas.class);
    /**
     * Returns A {@link Function} Based on the {@link Comparison} Operation passed.
     * This {@link Function} returns A {@link Predicate} Object When when applied.
     *
     * @param op
     * @return
     */
    public Function<ComparisonPredicate, Predicate<T>> get(Comparison op) {
        // load a operators map that maps operators to
        switch (op) {
            case EQUALS:
                return this::equality;
            case NOT_EQUALS:
                return this::equality;
            case GREATER_THAN:
                return this::greaterLessEquals;
            case LESS_THAN:
                return this::greaterLessEquals;
            case GREATER_THAN_EQUALS:
                return this::greaterLessEquals;
            case LESS_THAN_EQUALS:
                return this::greaterLessEquals;
            case LIKE:
                return this::like;
            case NOT_LIKE:
                return this::like;
            case IS_NULL:
                return this::isNull;
            case IS_NOT_NULL:
                return this::isNull;
            default:
                throw new UnparsableQueryException(op.toString() + " Not implemented");
        }
    }

    public MavisPredicateLambdas(Class<?> class1) {
        super();
        this.clazz = class1;
    }

    public Class<?> getClazz() {
        return clazz;
    }

    public NullablePredicate<T> equality(ComparisonPredicate predicate) {
        PredicateField predicateField = predicate.getField();
        String val = predicate.getValue();
        return s -> {
            try {

                Object obj = predicateField.getFieldConvertor().convert(s,predicateField.getName());
                if (obj != null) {
                    Class fieldType = obj.getClass();
                    predicateField.getConvertUtils().register(predicateField.getValueConvertor(), fieldType);
                    Object valObj = convertToFieldType(predicateField, val, fieldType);
                    if(valObj == null){
                        valObj = new ConvertUtils().convertUtilsBean().convert(val,fieldType);
                    }
                    if (fieldType.equals(String.class)) {
                        obj = ((String) obj).toUpperCase();
                        valObj = ((String) valObj).toUpperCase();
                    }
                    switch (predicate.getOperation()) {
                        case EQUALS:
                            return obj.equals(valObj);
                        case NOT_EQUALS:
                            return !obj.equals(valObj);
                        default:
                            return null;
                    }
                }
            } catch (ConversionException e) {
                LOGGER.log(Level.ERROR, e.getMessage());
                throw new UnparsableQueryException("Cannot convert " + val);
            } catch (Throwable e) {
                LOGGER.log(Level.ERROR, e.getMessage());
                throw new UnparsableQueryException(e.getMessage());
            }
            return null;
        };
    }

    private Object convertToFieldType(PredicateField predicateField, String val, Class fieldType) {
        Object obj =  predicateField.getConvertUtils().convertUtilsBean().convert(val,fieldType);
        //Use default convertor if  field convertor returns null
        if (obj == null){
            obj = new ConvertUtils().convertUtilsBean().convert(val,fieldType);
        }
        return obj;
    }

    public NullablePredicate<T> isNull(ComparisonPredicate predicate) {
        return s -> {
            try {
                PredicateField predicateField = predicate.getField();
                Object obj = predicateField.getFieldConvertor().convert(s,predicateField.getName());
                switch (predicate.getOperation()) {
                    case IS_NULL:
                        return obj == null ? true : false;
                    case IS_NOT_NULL:
                        return obj == null ? false : true;
                    default:
                        return null;
                }
            } catch (Throwable e) {
                LOGGER.log(Level.ERROR, e.getMessage());
            }
            return false;
        };
    }

    public NullablePredicate<T> greaterLessEquals(ComparisonPredicate predicate) {
        PredicateField predicateField = predicate.getField();
        String val = predicate.getValue();
        return s -> {
            try {
                @SuppressWarnings("rawtypes")
                Comparable valObj = null;
                Comparable obj = null;
                obj = (Comparable<?>) predicateField.getFieldConvertor().convert(s,predicateField.getName());
                if (obj != null) {
                    Class fieldType = obj.getClass();
                    predicateField.getConvertUtils().register(predicateField.getValueConvertor(), fieldType);
                    valObj = (Comparable<?>) convertToFieldType(predicateField, val, fieldType);
                    @SuppressWarnings("unchecked")
                    int compareTo = obj.compareTo(valObj);
                    switch (predicate.getOperation()) {
                        case GREATER_THAN:
                            return compareTo > 0;
                        case LESS_THAN:
                            return compareTo < 0;
                        case GREATER_THAN_EQUALS:
                            return compareTo >= 0;
                        case LESS_THAN_EQUALS:
                            return compareTo <= 0;
                        default:
                            return false;
                    }
                } else
                    return null;
            } catch (ConversionException e) {
                LOGGER.log(Level.ERROR, e.getMessage());
                throw new UnparsableQueryException("Cannot convert " + val);
            } catch (Throwable e) {
                LOGGER.log(Level.ERROR, e.getMessage());
                throw new UnparsableQueryException(e.getMessage());
            }

        };
    }

    private NullablePredicate<T> like(ComparisonPredicate predicate) {
        PredicateField predicateField = predicate.getField();
        String val = predicate.getValue();
        String valRegex = val.replaceAll("%", ".*").toUpperCase();
        return s -> {
            try {
                String obj = (String) predicateField.getFieldConvertor().convert(s,predicateField.getName());
                if (obj != null) {
                    obj = obj.toUpperCase();
                    Matcher m = Pattern.compile(valRegex).matcher(obj);
                    switch (predicate.getOperation()) {
                        case LIKE:
                            return m.find();
                        case NOT_LIKE:
                            return !m.find();
                        default:
                            return false;
                    }
                } else
                    return null;
            } catch (Throwable e) {
                LOGGER.log(Level.ERROR, e.getMessage());
                throw new UnparsableQueryException(e.getMessage());
            }
        };

    }

}
