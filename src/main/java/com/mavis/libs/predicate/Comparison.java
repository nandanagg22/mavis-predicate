package com.mavis.libs.predicate;

public enum Comparison {

    EQUALS("=", "eq"), //
    NOT_EQUALS("<>", "neq"), //
    LESS_THAN("<", "lt"), //
    LESS_THAN_EQUALS("<=", "lteq"), //
    GREATER_THAN(">", "gt"), //
    GREATER_THAN_EQUALS(">=", "gteq"), //
    IS_NULL(" is null ",  "is-null"), //
    IS_NOT_NULL(" is not null ", "is-not-null"),//
    AND(" AND ","and"),//
    OR(" OR " , "or"),//
	NOT(" NOT ", "not"),//
	LIKE(" LIKE ","like"),//
	NOT_LIKE(" NOT LIKE ","not-like");
    private final String token;
    private final String apiOperator;
    Comparison(String token,String apiOperator) {
        this.token = token;
        this.apiOperator = apiOperator;
    }

    public String token() {
        return this.token;
    }

    public String getApiOperator(){
        return this.apiOperator;
    }
}