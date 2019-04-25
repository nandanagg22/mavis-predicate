package com.mavis.libs.predicate;

public class PredicateBuilder {

	private MavisPredicate finalPredicate;
	private MavisPredicate currentPredicate;
	private Class<?> clazz;
	
	public PredicateBuilder(Class<?> clazz)
	{
		this.clazz=clazz;
	}
	
	/**
	 * Starts Building a {@link MavisPredicate}. To get a {@link MavisPredicate}
	 * object, call toPredicate() method of this object.  
	 * @param field  lhs
	 * @param comparison {@link Comparison}
	 * @param value  rhs
	 */
	public PredicateBuilder build(String field, Comparison comparison, String value) {
		ComparisonPredicate predicate = new ComparisonPredicate(new PredicateField(field), value, comparison,this.clazz);
		this.finalPredicate = predicate;
		this.currentPredicate=predicate;
		return this;	
	}
	
	/**
	 * Performs a logical AND chaining
	 * with the filter operation passed. 
	 * @param field  lhs
	 * @param comparison {@link Comparison}
	 * @param value  rhs
	 */
	public PredicateBuilder and(String field, Comparison comparison, String value) {
		MavisPredicate pred1=this.finalPredicate;
		this.currentPredicate=new ComparisonPredicate(new PredicateField(field), value,comparison,pred1.getClazz());
		this.finalPredicate=new LogicalPredicate(pred1,this.currentPredicate,Comparison.AND,pred1.getClazz());
		return this;
	}
	/**
	 * Performs a logical AND chaining 
	 * with the {@link MavisPredicate} passed.
	 * @param predicate
	 * @return
	 */
	public PredicateBuilder and(MavisPredicate predicate) {
		MavisPredicate pred1=this.finalPredicate;
		this.currentPredicate=predicate;
		this.finalPredicate=new LogicalPredicate(pred1, this.currentPredicate,Comparison.AND,pred1.getClazz());
		return this;
	}
	/**
	 * Performs a logical OR chaining 
	 * with the filter operation passed. 
	 * @param field  lhs
	 * @param comparison {@link Comparison}
	 * @param value  rhs
	 */
	public PredicateBuilder or(String field, Comparison comparison, String value) {
		MavisPredicate pred1=this.finalPredicate;
		this.currentPredicate=new ComparisonPredicate(new PredicateField(field), value,comparison,pred1.getClazz());
		this.finalPredicate=new LogicalPredicate(pred1, this.currentPredicate,Comparison.OR,pred1.getClazz());
		return this;
	}
	/**
	 * Performs a logical OR chaining 
	 * with the {@link MavisPredicate} passed.
	 * @param predicate
	 * @return
	 */
	public PredicateBuilder or(MavisPredicate predicate) {
		MavisPredicate pred1=this.finalPredicate;
		this.currentPredicate=predicate;
		this.finalPredicate=new LogicalPredicate(pred1, this.currentPredicate,Comparison.OR,pred1.getClazz());
		return this;
	}
	/**
	 * performs a logical negation of the operation.
	 * @return
	 */
	public PredicateBuilder negate() {
		MavisPredicate pred2=this.finalPredicate;
		MavisPredicate neg;
		if(pred2 instanceof ComparisonPredicate){
			this.finalPredicate=new NegationPredicate(pred2.getClazz(),pred2);
			this.currentPredicate=this.finalPredicate;			
		}
		else if(pred2 instanceof LogicalPredicate){	
			LogicalPredicate pred3=(LogicalPredicate)pred2;
			if(this.currentPredicate==(pred3.getPredicate1()))
			{	
				neg = new NegationPredicate(this.clazz,pred3.getPredicate1());
				this.finalPredicate=new LogicalPredicate(neg,pred3.getPredicate2(),pred3.getOperation(),pred3.getClazz());
				this.currentPredicate=neg;
			}
			else if(this.currentPredicate==((LogicalPredicate)pred2).getPredicate2())
			{
				neg = new NegationPredicate(this.clazz,pred3.getPredicate2());
				this.finalPredicate=new LogicalPredicate(pred3.getPredicate1(),neg,pred3.getOperation(),pred3.getClazz());
				this.currentPredicate=neg;
			}
		}
		else if(pred2 instanceof NegationPredicate)
		{
			this.finalPredicate=new NegationPredicate(pred2.getClazz(),((NegationPredicate) pred2).getPredicate());
			this.currentPredicate=this.finalPredicate;
		}
		return this;
	}
	
	/**
	 * Returns a {@link NegationPredicate} that represents the logical negation of the {@link MavisPredicate}
	 * passed as parameter.
	 * @param predicate
	 * @return
	 */
	public static NegationPredicate negate(MavisPredicate predicate) {
		return new NegationPredicate(predicate.getClazz(), predicate);
	}
	/**
	 * Returns A {@link MavisPredicate} Composed of the filter Operations
	 * Passed to this Object.
	 * @return
	 */
	public MavisPredicate toPredicate()
	{
		return this.finalPredicate;
	}

}
