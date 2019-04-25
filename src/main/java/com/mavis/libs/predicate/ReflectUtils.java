package com.mavis.libs.predicate;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/**
 * 
 */
public class ReflectUtils {

    /**
     * Invokes the given method. Handles translation of exceptions thrown by the
     * method.
     * 
     * @param o
     * @param method
     * @param args
     * @return
     * @throws Throwable
     */
    public static Object invoke(Object o, Method method, Object... args) throws Throwable {
        try {
            return method.invoke(o, args);
        } catch (IllegalAccessException | IllegalArgumentException e) {
            throw new RuntimeException(e);
        } catch (InvocationTargetException e) {
            if (e.getCause() == null) {
                throw e;
            }
            throw resolveException(method, e.getCause());
        } catch (Exception e) {
            throw resolveException(method, e);
        }
    }

    private static Throwable resolveException(Method method, Throwable t) {
        if (RuntimeException.class.isInstance(t)) {
            return t;
        }
        for (Class<?> exceptionClass : method.getExceptionTypes()) {
            if (exceptionClass.isInstance(t)) {
                return t;
            }
        }
        return new UndeclaredThrowableException(t);
    }

    /**
     * Gets all classes inherited by and implemented by the given class.
     * 
     * @param clazz
     * @return
     */
    public static Field getFieldIncludingSuperClass(Class<?> clazz, String name) throws NoSuchFieldException{
        Field field = null;
        while (clazz != null && field == null) {
            try {
                field = clazz.getDeclaredField(name);
            } catch (NoSuchFieldException e) {
            	//DO Nothing Check for Super class
            }
            clazz = clazz.getSuperclass();
        }
        if(field==null)
        {
        	throw new NoSuchFieldException();
        }
        return field;
    }
    

}
