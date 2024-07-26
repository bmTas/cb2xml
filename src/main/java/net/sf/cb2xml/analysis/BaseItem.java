package net.sf.cb2xml.analysis;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.sf.cb2xml.def.IItemBase;

/**
 * Provide common operations for the Item and Copybook classes.
 * 
 * @author Bruce Martin
 *
 */
public abstract class BaseItem implements IItemBase {

	private static List<Item> EMPTY_ITEM = Collections.unmodifiableList(new ArrayList<Item>(0));

	private List<Item> childItems = EMPTY_ITEM;
	
	private List<Object> allElements;

	protected BaseItem() {
	}

	protected BaseItem(int size) {
		allElements = new ArrayList<Object>(size + 50);
	}


	public List<? extends Item> getChildItems() {
		return childItems;
	}

	public void addItem(Item item) {
		if (childItems == EMPTY_ITEM) {
			childItems = new ArrayList<Item>(5);
		}
		childItems.add(item);
		add(item);
	}
	
	@Override
	public void addComment(String comment) {
		add(comment);
	}

	protected void add(Object o) {
		if (allElements == null) {
			allElements = new ArrayList<Object>();
		}
		allElements.add(o);
	}
	
	protected void addBefore(Object posObject, Object o) {
		if (allElements == null) { add(o); }
		
		int pos = allElements.indexOf(posObject);
		if (pos >= 0) {
			allElements.add(pos, o);
		} else {
			allElements.add(o);
		}
	}
	
	/**
	 * @return the allElements
	 */
	@Override
	public final List<Object> getAllElements() {
		return allElements;
	}
}
