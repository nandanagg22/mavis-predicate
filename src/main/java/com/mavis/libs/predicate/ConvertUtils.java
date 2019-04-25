package com.mavis.libs.predicate;


import org.apache.commons.beanutils.ConvertUtilsBean;
import org.apache.commons.beanutils.Converter;
import org.apache.commons.beanutils.converters.*;

public class ConvertUtils {
    private ConvertUtilsBean convertUtilsBeanutils = new ConvertUtilsBean();


    public ConvertUtils() {
        convertUtilsBeanutils.register(new IntegerConverter(), Integer.class);
        convertUtilsBeanutils.register(new IntegerConverter(), Integer.TYPE);
        convertUtilsBeanutils.register(new BooleanConverter(), Boolean.class);
        convertUtilsBeanutils.register(new BooleanConverter(), Boolean.TYPE);
        convertUtilsBeanutils.register(new ByteConverter(), Byte.class);
        convertUtilsBeanutils.register(new ByteConverter(), Byte.TYPE);
        convertUtilsBeanutils.register(new CharacterConverter(), Character.class);
        convertUtilsBeanutils.register(new CharacterConverter(), Character.TYPE);
        convertUtilsBeanutils.register(new DoubleConverter(), Double.class);
        convertUtilsBeanutils.register(new DoubleConverter(), Double.TYPE);
        convertUtilsBeanutils.register(new FloatConverter(), Float.class);
        convertUtilsBeanutils.register(new FloatConverter(), Float.TYPE);
        convertUtilsBeanutils.register(new LongConverter(), Long.class);
        convertUtilsBeanutils.register(new LongConverter(), Long.TYPE);
        convertUtilsBeanutils.register(new ShortConverter(), Short.class);
        convertUtilsBeanutils.register(new ShortConverter(), Short.TYPE);
        convertUtilsBeanutils.register(new StringConverter(), String.class);
    }

    public ConvertUtilsBean convertUtilsBean() {
        return convertUtilsBeanutils;
    }

    public void register(Converter converter, Class type) {
        if (converter !=  null && type != null) {
            this.convertUtilsBeanutils.register(converter, type);
        }
    }

    public Converter getDefaultConverter(Class clazz) {
        return this.convertUtilsBeanutils.lookup(clazz);
    }


}
