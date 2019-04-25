package com.mavis.libs.predicate;

/**
 * This Class shall contain the logic to
 * derive a Query field from the POJO Object if the query field
 * is not directly mapped to a pojo field. For eg, if Query field is
 * "isAvailable" but in the POJO thee is no such field  but it is
 * derived by some other fields in the object for eg. isAvailable =  obj.isActive && obj.Status != "closed"
 * then this logic needs to pe provided here.
 * @author Nandan Aggarwal
 *
 */
public interface PredicateFieldConverter {
    Object convert(Object obj,String fieldName);
}
