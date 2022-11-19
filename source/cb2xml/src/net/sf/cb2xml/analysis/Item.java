/**
 * 
 */
package net.sf.cb2xml.analysis;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.Cb2xmlConstants.Justified;
import net.sf.cb2xml.def.Cb2xmlConstants.NumericClass;
import net.sf.cb2xml.def.ICondition;
import net.sf.cb2xml.def.IItem;
import net.sf.cb2xml.def.IItemBase;
import net.sf.cb2xml.def.IItemJr;
import net.sf.cb2xml.def.IItemJrUpd;

/**
 * Representation of a field / group definition in
 * a Cobol Copybook
 * 
 * @author Bruce Martin
 *
 */
public class Item extends BaseItem implements IItemJrUpd {

//	private List<? extends ICondition> conditions;
//	private List<? extends IItem> childItems;
	
	/* internal use */
	private static List<Condition> EMPTY_CONDITION = Collections.unmodifiableList(new ArrayList<Condition>(0));

	final BaseItem parentItem;
	int assumedDigits=0, doubleByteChars = 0;
	private List<Condition> conditions = EMPTY_CONDITION;
	
	/* end internal use */


	final int levelNumber, relativeLevel;
	Cb2xmlConstants.Justified justified = Cb2xmlConstants.Justified.NOT_JUSTIFIED; 
	
	String dependingOn;
	int displayLength = NULL_INT_VALUE;
	private final String levelString;
	//final String name;
	/* Non JRecord */
	boolean inheritedUsage = false;
	boolean /*numeric=false, editedNumeric=false,*/ 
			insertDecimal = false, //TODO insertDecimal can probably be removed
			blankWhenZero=false;
//	boolean signSeparate;
	boolean signed;
	boolean fieldRedefined;
	boolean sync = false;
	int occurs=NULL_INT_VALUE;
	int occursMin=NULL_INT_VALUE;
	String picture;
	int position, displayPosition;
	String redefines;
	int scale;
	int storageLength;
//	Cb2xmlConstants.SignPosition signPosition;
	Cb2xmlConstants.NumericClass numericClass = Cb2xmlConstants.NumericClass.NON_NUMERIC;
	Cb2xmlConstants.SignClause signClause = Cb2xmlConstants.SignClause.NO_SIGN_CLAUSE;
	Cb2xmlConstants.Usage usage = Cb2xmlConstants.Usage.NONE;
	String value;
	private final String fieldName;
	
	
	int type;
	
	public Item(BaseItem parentItem, int levelNumber, String levelStr, String name) {
		this.parentItem = parentItem;
		this.levelNumber = levelNumber;
		this.levelString = levelStr;
		this.fieldName = name;
		
		int rLevel = 0;
		if (parentItem != null) {
			parentItem.addItem(this);
			if (parentItem instanceof Item) {
				rLevel = ((Item) parentItem).relativeLevel + 1;
			}
		}
		this.relativeLevel = rLevel;
	}
	
	public void set(IItem item) {
//		conditions         = item.getConditions();
//		childItems         = item.getChildItems();
		picture            = item.getPicture();
		numericClass       = item.getNumericClass();
		dependingOn        = item.getDependingOn();
		displayLength      = item.getDisplayLength();
		displayPosition    = item.getDisplayPosition();
		usage              = item.getUsage();
		occurs             = item.getOccurs();
		occursMin          = item.getOccursMin();
		justified          = item.getJustified();
		position           = item.getPosition();
		redefines          = item.getRedefinesFieldName();
		scale              = item.getScale();
		storageLength      = item.getStorageLength();
		sync               = item.isSync();
		value              = item.getValue();
		fieldRedefined     = item.isFieldRedefined();
		signClause         = item.getSignClause();
		inheritedUsage     = item.isInheritedUsage();
		
		if (item instanceof IItemJr) {
			type = ((IItemJr) item).getType();			
		}
	}
	
	

	/* (non-Javadoc)
	 * @see net.sf.cb2xml.def.IItem#getParent()
	 */
	@Override
	public IItemBase getParent() {
		return parentItem;
	}

	public List<? extends ICondition> getConditions() {
		return conditions;
	}

	public void addCondition(Condition condition) {
		if (conditions == EMPTY_CONDITION) {
			conditions = new ArrayList<Condition>(3);
		}
		conditions.add(condition);
		super.add(condition);
	}

	
//	
	
	/* (non-Javadoc)
	 * @see net.sf.cb2xml.def.IItem#getLevelNumber()
	 */
	@Override
	public int getLevelNumber() {
		return levelNumber;
	}
	
	/**
	 * @return the relativeLevel
	 */
	@Override
	public int getRelativeLevel() {
		return relativeLevel;
	}

	@Override
	public String getFieldName() {
		return fieldName;
	}
	
	
	@Override
	public String getPicture() {
		return picture;
	}

	
	@Override
	public Cb2xmlConstants.Usage getUsage() {
		return usage;
	}

	@Override
	public int getOccurs() {
		return occurs;
	}
	
	@Override
	public int getOccursMin() {
		return occursMin;
	}
	

	@Override
	public NumericClass getNumericClass() {
		return numericClass;
	}


	/* (non-Javadoc)
	 * @see net.sf.cb2xml.def.IItem#getJustified()
	 */
	@Override
	public Justified getJustified() {
		return justified;
	}

	@Override
	public String getDependingOn() {
		return dependingOn;
	}
	
	/**
	 * @return the displayPosition
	 */
	@Override
	public final int getDisplayPosition() {
		return displayPosition;
	}

	@Override
	public int getDisplayLength() {
		return displayLength;
	}
	
	@Override
	public String getLevelString() {
		return levelString;
	}
	
	
//	/**
//	 * @return the editedNumeric
//	 */
//	@Override
//	public final boolean isEditedNumeric() {
//		return editedNumeric;
//	}

//	/**
//	 * @return the insertDecimal
//	 */
//	@Override
//	public final boolean isInsertDecimal() {
//		return insertDecimal;
//	}
	
	@Override
	public int getPosition() {
		return position;
	}
	
	@Override
	public String getRedefinesFieldName() {
		return redefines;
	}
	
	@Override
	public int getScale() {
		return scale;
	}
		
	/**
	 * @return the signClause
	 */
	@Override
	public final Cb2xmlConstants.SignClause getSignClause() {
		return signClause;
	}
	
	@Override
	public int getStorageLength() {
		return storageLength;
	}
	
	@Override
	public boolean isSync() {
		return sync;
	}
	
	@Override
	public String getValue() {
		return value;
	}


	@Override
	public boolean isFieldRedefined() {
		return fieldRedefined;
	}
	
	@Override
	public boolean isFieldRedefines() {
		return redefines != null && redefines.length() > 0;
	}
		
	/**
	 * @return the inheritedUsage
	 */
	@Override
	public final boolean isInheritedUsage() {
		return inheritedUsage;
	}

	/**
	 * @return the blankWhenZero
	 */
	@Override
	public final boolean isBlankWhenZero() {
		return blankWhenZero;
	}


	/**
	 * @return the signed
	 */
	public boolean isSigned() {
		return signed;
	}

	/* (non-Javadoc)
	 * @see net.sf.cb2xml.def.IItemJr#updatePosition(int)
	 */
	@Override
	public void updatePosition(int adjustment) {
		position += adjustment;
		for (Item itm : getChildItems()) {
			itm.updatePosition(adjustment);
		}
	}

	/**
	 * Get the JRecord Type id. For use in JRecord !!!
	 */
	@Override
	public int getType() {
		return type;
	}



	/**
	 * Set the JRecord Type id. For use in JRecord !!!
	 */
	@Override
	public void setType(int type) {
		this.type = type;
	}
}
