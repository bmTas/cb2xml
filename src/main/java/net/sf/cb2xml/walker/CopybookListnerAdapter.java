package net.sf.cb2xml.walker;

import net.sf.cb2xml.def.ICondition;
import net.sf.cb2xml.def.ICopybook;
import net.sf.cb2xml.def.IItem;

/**
 * A base {@link ICopybookListner} that the you can overide
 * methods as required
 * 
 * @author Bruce Martin
 *
 */
public class CopybookListnerAdapter implements ICopybookListner {

	@Override
	public void processComment(String Comment) {}

	@Override
	public void startCopybook(ICopybook copybook) {}

	@Override
	public void endCopybook(ICopybook copybook) {}

	@Override
	public void startCondition(ICondition condition) {}

	@Override
	public void endCondition(ICondition condition) {}

	@Override
	public void startItem(IItem item) {}

	@Override
	public void endItem(IItem item) {}

}
