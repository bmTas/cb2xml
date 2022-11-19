package net.sf.cb2xml.walker;

import net.sf.cb2xml.def.ICondition;
import net.sf.cb2xml.def.ICopybook;
import net.sf.cb2xml.def.IItem;

public interface ICopybookListner {

	public void processComment(String Comment);
	
	public void startCopybook(ICopybook copybook);
	
	public void endCopybook(ICopybook copybook);
	
	public void startCondition(ICondition condition);
	public void endCondition(ICondition condition);

	public void startItem(IItem item);
	public void endItem(IItem item);

}
