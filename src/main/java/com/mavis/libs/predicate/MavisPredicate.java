package com.mavis.libs.predicate;

import org.apache.commons.beanutils.Converter;

import java.util.List;

/**
 * MavisPredicate is a
 * @author Nandan Aggarwal
 *
 */
public interface MavisPredicate {
 
    Comparison getOperation();
    Class<?> getClazz();

    /**
     * returns all the {@link PredicateField} that the predicate contains.
     * @return
     */
    List<PredicateField> getFields();
    /**
     * returns all the {@link PredicateField} which have the mappedname passed that the predicate contains.
     * @return
     */
    List<PredicateField> lookupPredicateFields(String mappedFieldName);

    /**
     * Sets  Value converter for the fields  that has mapped name passed.
     * @param mappedFieldName
     * @param converter
     */
    void setValueConverter(String mappedFieldName, Converter converter);

    /**
     *  Sets  FieldConverter converter for the fields  that has mapped name passed.
     *
     * @param mappedFieldName
     * @param converter
     * @see PredicateFieldConverter
     */
    void setFieldConverter(String mappedFieldName,PredicateFieldConverter converter);
}
