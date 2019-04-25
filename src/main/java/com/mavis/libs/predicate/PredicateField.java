package com.mavis.libs.predicate;

import org.apache.commons.beanutils.Converter;
import org.apache.commons.beanutils.PropertyUtils;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;

/**
 * Predicate Field contains Mapping and conversion Information to map and convert a field specified
 * in query filter to the POJO member variable name.
 */
public class PredicateField {

    private String name;
    private String mappedName;
    private boolean searchableFlag = true;
    private List<? extends PredicateField> linkedFields;
    private Converter valueConvertor;
    private PredicateFieldConverter fieldConvertor;
    private ConvertUtils convertUtils;

    /**
     * Creates a PredicateField Object based on the query filter filter field name
     * By Default,
     * 1. the field mappedName will be set to name
     * 2. Serchable flag will be  true
     * 3. fieldConvertor will call the getter Method og the POJO
     * @param name- Query field name
     */
    public PredicateField(String name) {
        this.name = name;
        this.mappedName = name;
        this.convertUtils = new ConvertUtils();
        //set the default field converter to get the value of teh field from field getter through reflection.
        this.setFieldConvertor(
                (o,fieldName) -> {
                    try {
                        if (fieldName.contains(".")) {
                            String childField = fieldName.substring(fieldName.indexOf('.') + 1);
                            Object parentObjectField = ReflectUtils.invoke(o, findGetter(o.getClass(), this.getMappedName()));
                            return PredicateFieldFactory.getField(childField, this.getLinkedFields()).getFieldConvertor().convert(parentObjectField,childField);
                        } else {
                            return ReflectUtils.invoke(o, findGetter(o.getClass(),
                                    this.getMappedName() != null ? this.getMappedName() : this.getName()));
                        }
                    } catch (Throwable throwable) {
                        throw new UnparsableQueryException("Invalid Field Name !!");
                    }
                }
        );
    }

    public Converter getValueConvertor() {
        return valueConvertor;
    }

    /**
     * Accepts a {@link Converter} Object.
     * If some special logic needs to be implemented for this field to
     * convert query string field to Object, then pass on the  {@link Converter} Object.
     * For eg. if query filter is "isActive EQUALS yes" , the 'yes' needs to be converted to
     * boolean 1 to compare with object . This conversion logic can be specified in the converter.
     * By Default, {@link org.apache.commons.beanutils.ConvertUtils} is used to convert string fields to
     * Objects
     * @param valueConvertor
     */
    public void setValueConvertor(Converter valueConvertor) {
        this.valueConvertor = valueConvertor;
    }

    public PredicateFieldConverter getFieldConvertor() {
        return this.fieldConvertor;
    }

    public void setFieldConvertor(PredicateFieldConverter fieldConvertor) {
        this.fieldConvertor = fieldConvertor;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getMappedName() {
        return mappedName;
    }

    public void setMappedName(String mappedName) {
        this.mappedName = mappedName;
    }

    public boolean isSearchableFlag() {
        return searchableFlag;
    }

    public void setSearchableFlag(boolean searchableFlag) {
        this.searchableFlag = searchableFlag;
    }

    public List<? extends PredicateField> getLinkedFields() {
        return linkedFields;
    }

    public void setLinkedFields(List<? extends PredicateField> linkedFields) {
        this.linkedFields = linkedFields;
    }


    private Method findGetter(Object o, String fieldName)
            throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        return PropertyUtils.getPropertyDescriptor(o,fieldName).getReadMethod();
    }

    public ConvertUtils getConvertUtils() {
        return convertUtils;
    }

}
