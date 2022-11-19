package net.sf.cb2xml.update;

import java.util.List;

import net.sf.cb2xml.analysis.BaseItem;
import net.sf.cb2xml.analysis.Condition;
import net.sf.cb2xml.analysis.Copybook;
import net.sf.cb2xml.analysis.IHasConditions;
import net.sf.cb2xml.analysis.Item;
import net.sf.cb2xml.analysis.PostProcess;
import net.sf.cb2xml.def.ICopybook;
import net.sf.cb2xml.def.NumericDefinition;

/**
 * Create a new Copybook with updates to the Item
 * @author Bruce Martin
 *
 */
public class UpdateCopybook {

	private final IUpdateItem updateItem;
	

	/**
	 * Create an updated Copybooks
	 * @param updateItem Item-Updater class to be used
	 */
	public UpdateCopybook(IUpdateItem itemUpdater) {
		super();
		this.updateItem = itemUpdater;
	}

	/**
	 * Create a new Copybook with updates to all Items in the supplied copybook Copybook
	 * @param copybook input copybook
	 * @return new Updated copybook
	 */
	public Copybook update(ICopybook copybook, NumericDefinition cobolDialectDefinition) {
		Copybook cpy = new Copybook(copybook.getFilename(), copybook.getDialect());
		
		updateItems(cpy, copybook.getAllElements());
		
		new PostProcess(cobolDialectDefinition, true).postProcessNode(cpy);
		
		return cpy;
	}
	
	private void updateItems(BaseItem parent, List<Object> childItems) {
		Item parentItem = parent instanceof Item ? (Item) parent : null;
		
		if (childItems != null && childItems.size() > 0) {
			for (Object o : childItems) {
				if (o instanceof Item) {
					Item item = (Item) o;
				
					Item updatedItem = updateItem.updateItem(parent, item);
					if (updatedItem != null) {
						updateItems(updatedItem, item.getAllElements());
					}
				} else if (parentItem != null && o instanceof Condition) {
					updateCondition(parentItem, (Condition) o);					
				} else {
					parent.addComment(o.toString());
				}
			}
		}
	}

	/**
	 * @param parentItem
	 * @param condition
	 */
	private void updateCondition(IHasConditions parent, Condition condition) {
		Condition updatedCondition = updateItem.updateCondition(condition);
		if (updatedCondition != null) {
			parent.addCondition(updatedCondition);
			if (condition != updatedCondition && updatedCondition.getChildConditions().size() == 0) {
				for (Condition c : condition.getChildConditions()) {
					updateCondition(updatedCondition, c);
				}
			}
		}
	}
}
