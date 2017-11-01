package net.sf.cb2xml.def;

import java.util.List;

public interface IItemBase {


	public IItemBase getParent();

	/**
	 * Get Child Items
	 */
	public abstract List<? extends IItem> getChildItems();

}
