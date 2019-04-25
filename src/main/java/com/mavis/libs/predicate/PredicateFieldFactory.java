package com.mavis.libs.predicate;

import java.util.List;

/**
 * Factory Class to create Predicate Fields.
 *
 * @author Nandan Aggarwal
 */


public class PredicateFieldFactory {
    /**
     * Factory method that returns a Predicate field from query field name.
     * If the List of {@link PredicateField} is Null, the default predicate fields will be returned
     * @see PredicateField
     */
    public static PredicateField getField(String field, List<? extends PredicateField> PredicateFields) {
        PredicateField predicateField = new PredicateField(field);
        String originalField = field;
        if (PredicateFields !=null && !PredicateFields.isEmpty()) {
            if (field.contains(".")) {

                field = field.substring(0, field.indexOf("."));
            }
            PredicateField temppredicateField = lookupPredicateField(field, PredicateFields);
            predicateField.setLinkedFields(temppredicateField.getLinkedFields());
            predicateField.setSearchableFlag(temppredicateField.isSearchableFlag());
            if(temppredicateField.getValueConvertor() != null){
                predicateField.setValueConvertor(temppredicateField.getValueConvertor());
            }
            if(temppredicateField.getFieldConvertor() != null){
                predicateField.setFieldConvertor(temppredicateField.getFieldConvertor());
            }
            predicateField.setName(originalField);
            predicateField.setMappedName(temppredicateField.getMappedName());
        }

        return predicateField;
    }

    public static PredicateField lookupPredicateField(String field, List<? extends PredicateField> PredicateFields) {
        return PredicateFields.stream()
                .filter(PredicateField -> PredicateField.getName().equalsIgnoreCase(field))
                .findFirst().get();
    }
}
