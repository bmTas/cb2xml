package net.sf.cb2xml.def;

import java.util.List;

public interface ICopybookJrUpd extends ICopybook {
	@Override
	public abstract List<? extends IItemJrUpd> getChildItems();

}
