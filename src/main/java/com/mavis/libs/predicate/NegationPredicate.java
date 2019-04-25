package com.mavis.libs.predicate;

import java.util.ArrayList;
import java.util.List;

public class NegationPredicate extends AbstractMavisPredicate implements MavisPredicate {
	private MavisPredicate predicate;

	public NegationPredicate(Class<?> clazz, MavisPredicate predicate) {
		super(Comparison.NOT, clazz);
		this.predicate = predicate;
	}

	public MavisPredicate getPredicate() {
		return predicate;
	}

	@Override
	public List<PredicateField> getFields() {
		List<PredicateField> predicateFields = new ArrayList<>();
		predicateFields.addAll(this.predicate.getFields());
		return predicateFields;
	}
}
