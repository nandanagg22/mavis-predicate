package com.mavis.libs.predicate;

import java.util.ArrayList;
import java.util.List;

/**
 * This object will contain A {@link PredicateField} and a value on which a {@link Comparison} operation
 * will be performed
 * 
 * @author Nandan Aggarwal
 *
 */
public class ComparisonPredicate extends AbstractMavisPredicate implements MavisPredicate {

	private PredicateField field;
	private String value;
/**
 * Instantiates a {@link ComparisonPredicate} object. Accepts the Object member variable name,
 * value to be compared and {@link Comparison} between the field and the value.
 * 
 * @param field
 * @param value
 * @param comparison 
 * @param clazz
 */
	public ComparisonPredicate(PredicateField field, String value, Comparison comparison, Class<?> clazz) {
		super(comparison, clazz);
		this.field = field;
		this.value = value;
	}

	public PredicateField getField() {
		return field;
	}

	public String getValue() {
		return value;
	}

	@Override
	public List<PredicateField> getFields() {
		List<PredicateField> predicateFields = new ArrayList<>();
		predicateFields.add(this.getField());
		return predicateFields;
	}
}
