package net.sf.cb2xml.analysis;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.sf.cb2xml.def.ICondition;

public class Condition implements ICondition {

	private static List<Condition> EMPTY = Collections.unmodifiableList(new ArrayList<Condition>(0));
	
    private List<Condition> childConditions = EMPTY;
    private final String name;
    private final String through;
    private final String value;
    boolean all = false;
    
    
	public Condition(String name, String through, String value) {
		super();
		this.name = name;
		this.through = through;
		this.value = value;
	}
	
	public Condition(ICondition c) {
		this.name = c.getName();
		this.through = c.getValue();
		this.value = c.getThrough();
	}
	
	public void addCondition(Condition c) {
		if (childConditions == EMPTY) {
			childConditions = new ArrayList<Condition>(5);
		}
		childConditions.add(c);
	}
	
	/**
	 * @return the getChildConditions
	 */
    @Override
	public final List<Condition> getChildConditions() {
		return childConditions;
	}
    
	/**
	 * @return the getName
	 */
	public final String getName() {
		return name;
	}
	
	/**
	 * @return the getThrough
	 */
	public final String getThrough() {
		return through;
	}
	
	/**
	 * @return the getValue
	 */
	public final String getValue() {
		return value;
	}
  
    
}
