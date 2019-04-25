package com.mavis.libs.predicate;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Collection;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * This Class Filters a list of objects.
 *
 * @param <T>
 * @author Nandan Aggarwal
 */
public class PredicateEngine<T> {
    private static Logger LOGGER = LogManager.getLogger(PredicateEngine.class);
    public PredicateEngine() {
    }

    /**
     * filters a List of OrEnabled Collection in memory. Accepts the collection
     * to be filtered along with the {@link MavisPredicate} Object that
     * contains the filtering Criteria and returns a filtered {@link List<T>}.
     *
     * @param collection
     * @param {@link
     *            MavisPredicate}
     * @return {@link List}
     */
    public List<T> filter(Collection<T> collection, MavisPredicate predicate) {
        long startTime = System.currentTimeMillis();
        try {
            List<T> filtereList = null;
            if (predicate == null) {
                filtereList = collection.stream().collect(Collectors.toList());
            }
            filtereList = collection.stream().filter(mapToLambda(predicate)).collect(Collectors.toList());
            long endTime = System.currentTimeMillis();
            LOGGER.info("Time taken for filtering a collection of size:" + collection.size() + " is :"
                    + (endTime - startTime) + "msec.");

            return filtereList;
        } catch (Exception e){
            throw  e;
        }

    }

    /**
     * Converts A {@link MavisPredicate} to a {@link Predicate} Object. The
     * {@link Predicate} object can be used to filter a stream by passing to the
     * streams.filter() method.
     *
     * @param {@link
     *            MavisPredicate}
     * @return {@link Predicate}
     */
    public Predicate<T> mapToLambda(MavisPredicate predicate) {
        String comparison = predicate.getOperation().toString();
        if (predicate instanceof LogicalPredicate) {
            MavisPredicate predicate1 = ((LogicalPredicate) predicate).getPredicate1();
            MavisPredicate predicate2 = ((LogicalPredicate) predicate).getPredicate2();
            if (comparison.equalsIgnoreCase(Comparison.AND.toString())) {
                return mapToLambda(predicate1).and(mapToLambda(predicate2));
            } else if (comparison.equalsIgnoreCase(Comparison.OR.toString())) {
                return mapToLambda(predicate1).or(mapToLambda(predicate2));
            } else
                return t -> false;
        } else if (predicate instanceof ComparisonPredicate) {
            MavisPredicateLambdas<T> fpl = new MavisPredicateLambdas<>(predicate.getClazz());
            ComparisonPredicate comparisonPredicate = (ComparisonPredicate) predicate;
            return fpl.get(comparisonPredicate.getOperation()).apply(comparisonPredicate);
        } else if (predicate instanceof NegationPredicate) {
            MavisPredicate neg = ((NegationPredicate) predicate).getPredicate();
            return neg instanceof LogicalPredicate ? mapToLambda(applyDistributivity(neg)) : mapToLambda(neg).negate();

        } else
            return t -> false;
    }

    private MavisPredicate applyDistributivity(MavisPredicate neg) {
        String comparison = neg.getOperation().toString();
        if (neg instanceof LogicalPredicate) {
            LogicalPredicate predicate = (LogicalPredicate) neg;
            MavisPredicate predicate1 = predicate.getPredicate1();
            MavisPredicate predicate2 = predicate.getPredicate2();
            NegationPredicate ngp1 = new NegationPredicate(predicate1.getClazz(), predicate1);
            NegationPredicate ngp2 = new NegationPredicate(predicate2.getClazz(), predicate2);
            return comparison.equalsIgnoreCase(Comparison.AND.toString())
                    ? new LogicalPredicate(ngp1, ngp2, Comparison.OR, ngp1.getClazz())
                    : new LogicalPredicate(ngp1, ngp2, Comparison.AND, ngp1.getClazz());
        }
        return null;
    }
}
