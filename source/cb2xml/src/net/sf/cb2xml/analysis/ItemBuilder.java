package net.sf.cb2xml.analysis;

import net.sf.cb2xml.def.Cb2xmlConstants;

public class ItemBuilder {

	public static ItemBuilder newBuilder() {
		return new ItemBuilder();
	}
	
	private Item item = new Item(null, 1, "01", "");

	private String fieldName, levelString;


	/**
	 * @param justified the justified to set
	 */
	public void setJustified(Cb2xmlConstants.Justified justified) {
		this.item.justified = justified;
	}
	/**
	 * @param dependingOn the dependingOn to set
	 */
	public void setDependingOn(String dependingOn) {
		this.item.dependingOn = dependingOn;
	}
	/**
	 * @param displayLength the displayLength to set
	 */
	public void setDisplayLength(int displayLength) {
		this.item.displayLength = displayLength;
	}
	/**
	 * @param levelString the levelString to set
	 */
	public void setLevelString(String levelString) {
		this.levelString = levelString;
	}
	/**
	 * @param inheritedUsage the inheritedUsage to set
	 */
	public void setInheritedUsage(boolean inheritedUsage) {
		this.item.inheritedUsage = inheritedUsage;
	}
	/**
	 * @param insertDecimal the insertDecimal to set
	 */
	public void setInsertDecimal(boolean insertDecimal) {
		this.item.insertDecimal = insertDecimal;
	}
	/**
	 * @param blankWhenZero the blankWhenZero to set
	 */
	public void setBlankWhenZero(boolean blankWhenZero) {
		this.item.blankWhenZero = blankWhenZero;
	}
	/**
	 * @param signed the signed to set
	 */
	public void setSigned(boolean signed) {
		this.item.signed = signed;
	}
	/**
	 * @param fieldRedefined the fieldRedefined to set
	 */
	public void setFieldRedefined(boolean fieldRedefined) {
		this.item.fieldRedefined = fieldRedefined;
	}
	/**
	 * @param sync the sync to set
	 */
	public void setSync(boolean sync) {
		this.item.sync = sync;
	}
	/**
	 * @param occurs the occurs to set
	 */
	public void setOccurs(int occurs) {
		this.item.occurs = occurs;
	}
	/**
	 * @param occursMin the occursMin to set
	 */
	public void setOccursMin(int occursMin) {
		this.item.occursMin = occursMin;
	}
	/**
	 * @param picture the picture to set
	 */
	public void setPicture(String picture) {
		this.item.picture = picture;
	}
	/**
	 * @param position the position to set
	 */
	public void setPosition(int position) {
		this.item.position = position;
	}
	/**
	 * @param redefines the redefines to set
	 */
	public void setRedefines(String redefines) {
		this.item.redefines = redefines;
	}
	/**
	 * @param scale the scale to set
	 */
	public void setScale(int scale) {
		this.item.scale = scale;
	}
	/**
	 * @param storageLength the storageLength to set
	 */
	public void setStorageLength(int storageLength) {
		this.item.storageLength = storageLength;
	}
	/**
	 * @param numericClass the numericClass to set
	 */
	public void setNumericClass(Cb2xmlConstants.NumericClass numericClass) {
		this.item.numericClass = numericClass;
	}
	/**
	 * @param signClause the signClause to set
	 */
	public void setSignClause(Cb2xmlConstants.SignClause signClause) {
		this.item.signClause = signClause;
	}
	/**
	 * @param usage the usage to set
	 */
	public void setUsage(Cb2xmlConstants.Usage usage) {
		this.item.usage = usage;
	}
	/**
	 * @param value the value to set
	 */
	public void setValue(String value) {
		this.item.value = value;
	}
	/**
	 * @param fieldName the fieldName to set
	 */
	public void setFieldName(String fieldName) {
		this.fieldName = fieldName;
	}
	
	protected Item getItem() {
		return item;
	}

	/**
	 * Create the new Item.
	 * @return
	 */
	public Item build(BaseItem parent) {
		Item ret = new Item(parent, Integer.parseInt(levelString), levelString, fieldName);
		
		ret.set(item);
		
		return ret;
	}
}
