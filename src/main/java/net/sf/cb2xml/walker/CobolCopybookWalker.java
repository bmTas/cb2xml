package net.sf.cb2xml.walker;

import java.util.List;

import net.sf.cb2xml.def.ICondition;
import net.sf.cb2xml.def.ICopybook;
import net.sf.cb2xml.def.IItem;

/**
 *  The **CobolCopybookWalker** will walk through a Cool copybook and call a supplied listner
 * with each item/condition/comment
 * 
 * @author Bruce Martin
 *
 */
public class CobolCopybookWalker {
	private final ICopybook copybook;

	/**
	 * The **CobolCopybookWalker** will walk through a Cool copybook and call a supplied listner
	 * with each item/condition/comment
	 * 
	 * @param copybook copybook walkthrough
	 */
	public CobolCopybookWalker(ICopybook copybook) {
		super();
		this.copybook = copybook;
	}
	
	/**
	 * Traverse the Cobol Copybook and call the copybookListner for each <ul>
	 *   <li>Cobol Item
	 *   <li>Cobol Condition
	 *   <li>Comment
	 * <ul>
	 * @param copybookListner Cobol Copybook listner. Normally you would override
	 * {@link CopybookListnerAdapter}.
	 * 
	 * @return This CobolCopybookWalker so you can string more Tree traversals
	 */
	public CobolCopybookWalker walk(ICopybookListner copybookListner) {
		
		copybookListner.startCopybook(copybook);
		
		processElements(copybookListner, copybook.getAllElements());
		
		copybookListner.endCopybook(copybook);
		
		return this;
	}
	
	private void processElements(ICopybookListner listner, List<Object> elements) {
		if (elements != null) {
			for (Object e : elements) {
				if (e instanceof IItem) {
					IItem item = (IItem) e;
					
					listner.startItem(item);
					processElements(listner, item.getAllElements());
					listner.endItem(item);
					
				} else if (e instanceof ICondition) {
					processCondition(listner, (ICondition) e);
				} else if (e != null) {
					listner.processComment(e.toString());
				}
			}
		}
	}


	/**
	 * @param listner
	 * @param condition
	 */
	private void processCondition(ICopybookListner listner, ICondition condition) {
		listner.startCondition(condition);
		for (ICondition c : condition.getChildConditions()) {
			processCondition(listner, c);
		}
		listner.endCondition(condition);
	}
}
