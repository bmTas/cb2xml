package net.sf.cb2xml.def;

import java.util.List;


public interface ICondition {

	/**
	 * Get Child Conditions
	 */
    public List<? extends ICondition> getChildConditions() ;

    /**
     * Gets the value of the name property.
     */
    public String getName() ;
 
    /**
     * Gets the value of the through property.
     */
    public String getThrough();

    /**
     * Gets the value of the value property.
     */
    public String getValue();
}
