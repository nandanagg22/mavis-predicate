package com.mavis.libs.predicate;

import java.util.ArrayList;
import java.util.List;


/**
 * This object will contain  the two {@link MavisPredicate} objects on which a
 * logical operation (and/or) will be performed  
 *
 * @author Nandan Aggarwal
 *
 */
public class LogicalPredicate  extends AbstractMavisPredicate implements MavisPredicate {

    private MavisPredicate predicate1;
    private MavisPredicate predicate2;
/**
 * Instantiates A LogicalPredicate Object. Accepts Two MavisPredicate objects
 * And the Logical operation(and or Or) to be performed on the Predicates
 * 
 * 
 * @param predicate1
 * @param predicate2
 * @param logicalOperator-and/or
 * @param clazz
 */
    public LogicalPredicate(MavisPredicate predicate1, MavisPredicate predicate2, Comparison logicalOperator, Class<?> clazz) {
        super(logicalOperator,clazz);
        this.predicate1 = predicate1;
        this.predicate2 = predicate2;

    }

    public MavisPredicate getPredicate1() {
        return predicate1;
    }

    public MavisPredicate getPredicate2() {
        return predicate2;
    }

    @Override
    public List<PredicateField> getFields() {
        List<PredicateField> predicateFields = new ArrayList<>();
        predicateFields.addAll(this.predicate1.getFields());
        predicateFields.addAll(this.predicate2.getFields());
        return predicateFields;
    }
}
