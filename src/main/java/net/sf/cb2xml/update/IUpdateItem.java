package net.sf.cb2xml.update;

import net.sf.cb2xml.analysis.BaseItem;
import net.sf.cb2xml.analysis.Condition;
import net.sf.cb2xml.analysis.Item;
import net.sf.cb2xml.def.ICondition;
import net.sf.cb2xml.def.IItem;

/**
 * Defines a class that will update an Item
 * @author Bruce Martin
 *
 */
public interface IUpdateItem {

	/**
	 * Creates a new Item based on a supplied Item
	 * @param itemToBeUpdated item to be updated
	 * @return updated Item
	 */
	public Item updateItem(BaseItem parent, IItem itemToBeUpdated);
	
	/**
	 * Supply updated Condition
	 * @param conditionToBeUpdated  condition to be updated
	 * @return updated Condition
	 */
	public Condition updateCondition(ICondition conditionToBeUpdated);
}
