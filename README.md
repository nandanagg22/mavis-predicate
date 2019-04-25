### ***Mavis predicate***
1. MavisPredicate is a Quering framework that can be used in a REST API search/filter queries for supporting dynamic querring on a List
2. The Framework converts a plain string query comming from the client into a MavisPredicate Object which is an 
Expression tree representaion of the filter. 
3. The MavisPredicate Object shall then be passed on to the PredicateEngine class which will apply the filter on the List<T>. 
4. The Predicate Engine Internally converts the ExpressionTree to a Chain of Java 8 Predicate Functions and 
pass them to the streams.filter() method. 

***Operations Supported***
1. eq
2. ne
3. lt
4. lte
5. gt
6. gte
7. and
8. or
9. not
10. like
11. not_like

***Example Usage***
api: ***/api/v1/testapp/employees?filter="salary gt 12000 or (salary lt 12000 and employeeType eq "TEMP")"***
EmployeeService  Code snippet
```java
Public List<Employee> loadAllEmployees(String filter) {
MavisPredicate predicate = new FilterParser(Employee.class)
                .parse(request.getParameter(filter));
        List<Employee> list = employeeDao.loadAllEmployees();
        List<Employee> filteredList = new PredicateEngine<Employee>().filter(list, predicate);
}
```
