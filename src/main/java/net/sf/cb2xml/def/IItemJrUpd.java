package net.sf.cb2xml.def;

import java.util.List;

public interface IItemJrUpd extends IItemJr {
	
	public abstract void updatePosition(int adjustment);
	public abstract void setType(int typeId);

	
	@Override
	public abstract List<? extends IItemJrUpd> getChildItems();

}
