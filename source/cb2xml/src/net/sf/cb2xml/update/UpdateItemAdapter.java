package net.sf.cb2xml.update;

import net.sf.cb2xml.analysis.BaseItem;
import net.sf.cb2xml.analysis.Condition;
import net.sf.cb2xml.analysis.Item;
import net.sf.cb2xml.analysis.ItemBuilder;
import net.sf.cb2xml.def.ICondition;
import net.sf.cb2xml.def.IItem;

public class UpdateItemAdapter implements IUpdateItem {

	public static final IUpdateItem NO_UPDATE = new UpdateItemAdapter();


	@Override
	public Item updateItem(BaseItem parent, IItem itemToBeUpdated) {
		ItemBuilder itemBuilder = ItemBuilder.newBuilder();
		itemBuilder.setFrom(itemToBeUpdated);
		return itemBuilder.build(parent);
	}

	@Override
	public Condition updateCondition(ICondition conditionToBeUpdated) {
		return conditionToBeUpdated instanceof Condition 
				? (Condition) conditionToBeUpdated 
				: new Condition(conditionToBeUpdated);
	}

}
