package com.mavis.libs.predicate;

import org.apache.commons.beanutils.Converter;
import java.util.List;
import java.util.stream.Collectors;

/**
 *
 * @author Nandan Aggarwal
 *
 */
public abstract class AbstractMavisPredicate implements MavisPredicate {

    protected final Comparison operation;
    protected final Class<?> clazz;

    public AbstractMavisPredicate(Comparison operation, Class<?> clazz) {
        super();
        this.operation = operation;
        this.clazz = clazz;
    }

    public Comparison getOperation() {
        return operation;
    }

    public Class<?> getClazz() {
        return clazz;
    }

    @Override
    public void setValueConverter(String mappedFieldName, Converter converter){
        this.getFields().stream().filter(field -> field.getMappedName().equalsIgnoreCase(mappedFieldName))
                .forEach(field -> field.setValueConvertor(converter));
    }

    @Override
    public void setFieldConverter(String mappedFieldName,PredicateFieldConverter converter){
        this.getFields().stream().filter(field -> field.getMappedName().equalsIgnoreCase(mappedFieldName))
                .forEach(field -> field.setFieldConvertor(converter));
    }

    @Override
    public List<PredicateField> lookupPredicateFields(String mappedFieldName){
       return this.getFields().stream().filter(field -> field.getMappedName().equalsIgnoreCase(mappedFieldName))
               .collect(Collectors.toList());
    }
}
