package net.sf.cb2xml.analysis;

import java.util.List;

import net.sf.cb2xml.def.ICondition;

public interface IHasConditions {

	void addCondition(Condition c);

	/**
	 * @return the getChildConditions
	 */
	List<? extends ICondition> getConditions();

}