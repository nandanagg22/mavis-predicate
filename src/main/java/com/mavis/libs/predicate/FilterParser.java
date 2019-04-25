package com.mavis.libs.predicate;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * This Class builds A {@link MavisPredicate} out of a plain query.
 *
 * @author Nandan Aggarwal
 */
public class FilterParser {
    private Class<?> clazz;
    private List<PredicateField> predicateFields;
    private static final String TOKEN_TYPE_FIELD = "field";
    private static final String TOKEN_TYPE_OPERATOR = "operator";
    private static final String TOKEN_TYPE_LITERAL = "literal";
    private static final String TOKEN_TYPE_OPENING_BRACES = "(";
    private static final String TOKEN_TYPE_CLOSING_BRACES = ")";
    private static final String TOKEN_TYPE_AND_OPERATOR = "and";
    private static final String TOKEN_TYPE_OR_OPERATOR = "or";
    private static final String TOKEN_TYPE_NOT_OPERATOR = "not";

    /**
     * Use This constructor if there is no configuration required for filter
     * fields and Object Pojo Fields
     * @param claszz
     */
    public FilterParser(Class<?> claszz) {
        super();
        this.clazz = claszz;
    }

    private  class Token {
        String value;
        String type;

        public Token(String value, String  type) {
            this.value = value;
            this.type = type;
        }
    }

    /**
     * This Constructor accepts a List of the all the  {@link PredicateField}
     * That the Object being filtered contains. This List will be used By the
     * Parser to convert query filter to {@link MavisPredicate}.
     * @param claszz
     * @param predicateFields
     * @see {@link PredicateField}
     */
    public FilterParser(Class<?> claszz, List<PredicateField> predicateFields) {
        super();
        this.clazz = claszz;
        this.predicateFields = predicateFields;
    }

    public Class<?> getClazz() {
        return clazz;
    }

    private MavisPredicate parse(List<Token> expression) {
        Stack<Object> stack = new Stack<>();
        MavisPredicate retPredicate = null;
        for (Token s : expression) {
            if (s.type.equalsIgnoreCase(TOKEN_TYPE_FIELD)) {
                stack.push(s.value);
            } else if (s.type.equalsIgnoreCase(TOKEN_TYPE_AND_OPERATOR)) {
                pushLogicalPredicate(stack, Comparison.AND);
            } else if (s.type.equalsIgnoreCase(TOKEN_TYPE_OR_OPERATOR)) {
                pushLogicalPredicate(stack, Comparison.OR);
            } else if (s.type.equalsIgnoreCase(TOKEN_TYPE_NOT_OPERATOR)) {
                stack.push(new NegationPredicate(this.clazz, (MavisPredicate) stack.pop()));
            } else if (s.type.equalsIgnoreCase(TOKEN_TYPE_OPERATOR)) {
                String val;
                if (Comparison.IS_NULL.getApiOperator().equalsIgnoreCase(s.value) ||
                        Comparison.IS_NOT_NULL.getApiOperator().equalsIgnoreCase(s.value)) {
                    val = "";
                } else if (stack.peek() instanceof String == false) {
                    throw new UnparsableQueryException("expecting an operator !!");
                } else {
                    val = (String) stack.pop();
                }
                if(stack.peek() instanceof String == false){
                    throw new UnparsableQueryException();
                }
                String field = (String) stack.pop();
                if (!isField(field)) {
                    throw new UnparsableQueryException();
                }
                PredicateField predicateField = PredicateFieldFactory.getField(field, this.predicateFields);
                stack.push(new ComparisonPredicate(predicateField, val, getComparison(s.value), this.clazz));
            } else {
                // it's a literal Operand
                stack.push(s.value);
            }
        }
        if (stack.peek() instanceof MavisPredicate) {
            retPredicate = (MavisPredicate) stack.pop();
        } else {
            throw new UnparsableQueryException();
        }
        if (!stack.isEmpty()) {
            throw new UnparsableQueryException();
        }
        return retPredicate;
    }

    private void pushLogicalPredicate(Stack<Object> stack, Comparison logicalComparison) {
        MavisPredicate predicate1 = null;
        MavisPredicate predicate2 = null;
        if (stack.peek() instanceof MavisPredicate) {
            predicate1 = (MavisPredicate) stack.pop();
        } else {
            throw new UnparsableQueryException();
        }
        if (stack.peek() instanceof MavisPredicate) {
            predicate2 = (MavisPredicate) stack.pop();
        } else {
            throw new UnparsableQueryException();
        }
        MavisPredicate logicalPredicate = new LogicalPredicate(predicate1, predicate2, logicalComparison,
                this.clazz);
        stack.push(logicalPredicate);
    }

    /**
     * Returns a {@link MavisPredicate} Composed from the String Query Passed
     * Object
     *
     * @param expression
     * @return {@link MavisPredicate}
     * @throws UnparsableQueryException
     */
    public MavisPredicate parse(String expression) {
        List<Token> postFix = toPostFix(expression);
        if (postFix != null && !postFix.isEmpty()) {
            return parse(postFix);
        } else {
            throw new UnparsableQueryException();
        }
    }

    private void validateParathenesis(List<Token> expression) {
        Stack<String> stack = new Stack<>();
        for (Token brc : expression) {
            if (brc.type.equalsIgnoreCase(TOKEN_TYPE_OPENING_BRACES)) {
                stack.add(brc.value);
            } else if (brc.type.equalsIgnoreCase(TOKEN_TYPE_CLOSING_BRACES)) {
                if (stack.isEmpty()) {
                    throw new UnparsableQueryException("UnBalanced parentheses");
                }
                stack.pop();
            }
        }
        if (!stack.isEmpty()) {
            throw new UnparsableQueryException("Unbalanced Parentheses");
        }
    }

    private List<Token> toPostFix(String expression) {
        Stack<Token> operators = new Stack<>();
        List<Token> postfix = new LinkedList<>();
        List<Token> list = convertToList(expression);
        validateParathenesis(list);
        for (Token token : list) {
            addToPostfix(token, postfix, operators);
        }
        while (!operators.isEmpty()) {
            postfix.add(operators.pop());
        }
        return postfix;
    }

    private void addToPostfix(Token symbol, List<Token> postfix, Stack<Token> operators) {
        if (symbol.type.equalsIgnoreCase(TOKEN_TYPE_FIELD)) {
            postfix.add(symbol);
        } else if (symbol.type.equalsIgnoreCase(TOKEN_TYPE_OPENING_BRACES)) {
            operators.push(symbol);
        } else if (symbol.type.equalsIgnoreCase(TOKEN_TYPE_CLOSING_BRACES)) {
            while (TOKEN_TYPE_OPENING_BRACES.equals(operators.peek().type) == false) {
                postfix.add(operators.pop());
            }
            operators.pop();
        } else if (symbol.type.equalsIgnoreCase(TOKEN_TYPE_OPERATOR) || symbol.type.equalsIgnoreCase(TOKEN_TYPE_AND_OPERATOR)
                || symbol.type.equalsIgnoreCase(TOKEN_TYPE_OR_OPERATOR)) {
            while (!operators.isEmpty() && !(operators.peek().type.equals(TOKEN_TYPE_OPENING_BRACES)) && prec(symbol.value) > prec(operators.peek().value)) {
                postfix.add(operators.pop());
            }
            operators.push(symbol);
        } else {
                postfix.add(symbol);
        }
    }

    private List<Token> convertToList(String expression) {
       List<Token> tokens = new ArrayList<>();
        expression = expression.replaceAll("\\(", " \\( ").replaceAll("\\)", " \\) ");
        Matcher m = Pattern.compile("([^\"]\\S*|\".+?\")\\s*").matcher(expression);
        while (m.find()) {
            String match = m.group(1).trim();
            Matcher m2 = Pattern.compile("\".+?\"").matcher(match);
            Token token = null;
            if(m2.matches()){
                token = new Token(match.replaceAll("\"", ""),TOKEN_TYPE_LITERAL);
                tokens.add(token);
            }else if(isField(match)){
                token = new Token(match,TOKEN_TYPE_FIELD);
                tokens.add(token);
            }else  if(isOperator(match)){
                token = new Token(match,TOKEN_TYPE_OPERATOR);
                tokens.add(token);
            }else if (match.equalsIgnoreCase(TOKEN_TYPE_OPENING_BRACES)){
                token = new Token(match,TOKEN_TYPE_OPENING_BRACES);
                tokens.add(token);
            }else if(match.equalsIgnoreCase(TOKEN_TYPE_CLOSING_BRACES)){
                token = new Token(match,TOKEN_TYPE_CLOSING_BRACES);
                tokens.add(token);
            }else if(match.equalsIgnoreCase(TOKEN_TYPE_AND_OPERATOR)){
                token = new Token(match,TOKEN_TYPE_AND_OPERATOR);
                tokens.add(token);
            }else if(match.equalsIgnoreCase(TOKEN_TYPE_OR_OPERATOR)){
                token = new Token(match,TOKEN_TYPE_OR_OPERATOR);
                tokens.add(token);
            }else if(match.equalsIgnoreCase(TOKEN_TYPE_NOT_OPERATOR)){
                token = new Token(match,TOKEN_TYPE_NOT_OPERATOR);
                tokens.add(token);
            } else{
                token = new Token(match,TOKEN_TYPE_LITERAL);
                tokens.add(token);
            }
        }
        return tokens;
    }

    private int prec(String x) {
        if (isOperator(x)) {
            return 1;
        } else if (x.equalsIgnoreCase(Comparison.AND.getApiOperator())) {
            return 2;
        } else if (x.equalsIgnoreCase(Comparison.OR.getApiOperator())) {
            return 3;
        }
        return 0;
    }

    private boolean isOperator(String s) {
        boolean val = false;
        for (Comparison cmp : Comparison.values()) {
            if (cmp.getApiOperator().equalsIgnoreCase(s)
                    && (!Comparison.AND.getApiOperator().equalsIgnoreCase(s) && !Comparison.OR.getApiOperator().equalsIgnoreCase(s))) {
                val = true;
                break;
            }
        }
        return val;
    }

    private Comparison getComparison(String apiOperator) {
        for (Comparison cmp : Comparison.values()) {
            if (cmp.getApiOperator().equalsIgnoreCase(apiOperator)) {
                return cmp;
            }
        }
        return null;
    }

    private boolean isField(String s) {
        if (s.contains(".")) {
            s = s.substring(0, s.indexOf('.'));
        }
        if (this.predicateFields !=null && !this.predicateFields.isEmpty()) {
            return isFieldPresent(s);
        }
        try {
            ReflectUtils.getFieldIncludingSuperClass(this.getClazz(), s);
        } catch (NoSuchFieldException e) {
            return false;
        }
        return true;
    }

    private boolean isFieldPresent(String s) {
        return this.predicateFields.stream()
                .anyMatch(foundationPredicateField -> foundationPredicateField.getName().equalsIgnoreCase(s));
    }

}