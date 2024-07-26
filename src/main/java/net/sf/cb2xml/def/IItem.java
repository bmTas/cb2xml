/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */

package net.sf.cb2xml.def;

import java.util.List;

import net.sf.cb2xml.analysis.IHasConditions;
import net.sf.cb2xml.def.Cb2xmlConstants.SignClause;


/**
 * This class represent a Cobol Item in a Cobol copybook
 * i.e. each of the following lines is a IItem.
 * 
 *     03  Field-1           pic s9(4).
 *     03  Field-2           pic x(30).
 *     03  table-1 occurs 8
 * 
 * @author Bruce Martin
 * 
 *
 */
public interface IItem extends IItemBase, IHasConditions {

	public static final int NULL_INT_VALUE = Integer.MIN_VALUE;


	/**
	 * Get Child Items
	 */
	public abstract List<? extends ICondition> getConditions();
	
	/**
	 * Gets the value of the level property.
	 *      
	 */
	public abstract String getLevelString();
	public abstract int getLevelNumber();
	
	/**
	 * @return the fieldName
	 */
	public abstract String getFieldName();

	
	/**
	 * Gets the value of the picture property.
	 */
	public abstract String getPicture();

//	/**
//	 * Gets the value of the numeric property.
//	 */
//	public abstract boolean isNumeric();

	/**
	 * Get the numeric class of Cobol Item.
	 * There are 3 classes<ul>
	 * <li><b>NON_NUMERIC</b> - Non numeric field
	 * <li><b>NUMERIC_EDITED</b> - Cobol Numeric-Edited field
	 * (e.g. -,--9.99) these fields are not strictly numeric in Cobol
	 * but are often used to send numeric values to non-numeric systems.
	 * <li><b>NUMERIC_IN_COBOL</b> - Tru Cobol numeric field.
	 * </ul>
	 * @return numeric class
	 */
	public abstract Cb2xmlConstants.NumericClass getNumericClass();

	/**
	 * Gets the value of the dependingOn property.
	 *   
	 */
	public abstract String getDependingOn();

	/**
	 * Gets the value of the displayLength property.
	 * 
	 */
	public abstract int getDisplayLength();

	public abstract int getDisplayPosition();

//	public boolean isEditedNumeric();

//	/**
//	 * Wether a physical decimal point appears in the field
//	 * (i.e. a physical '.' is used instead of the assumed decimal
//	 *  ~~ -,--9.99 instead of s9(4)V99)
//	 *  
//	 * @return
//	 */
//	public boolean isInsertDecimal();
	
	/**
	 * Gets the value of the usage property. 
	 */
	public abstract Cb2xmlConstants.Usage getUsage();

	/**
	 * Gets the value of the occurs property.
	 * 
	 */
	public abstract int getOccurs();

	/**
	 * Gets the value of the occursMin property.
	 * 
	 * @return
	 *     possible object is
	 *     {@link Integer }
	 *     
	 */
	public abstract int getOccursMin();

	/**
	 * Justified clause (returns null if no justified clause)
	 */
	public Cb2xmlConstants.Justified getJustified();
	
	/**
	 * Gets the value of the position property.
	 * 
	 */
	public abstract int getPosition();

	/**
	 * Gets the name of the item being redefined.
	 */
	public abstract String getRedefinesFieldName();

	/**
	 * Gets the scale of the number (< 0 for non-numeric). 
	 */
	public abstract int getScale();

//	/**
//	 * Does the the item have a sign ???
//	 */
//	public abstract boolean isSigned();

	/**
	 * Get details of Cobol Sign clause
	 */
	public abstract SignClause getSignClause();

	/**
	 * Gets the number of bytes the field occupies
	 */
	public abstract int getStorageLength();

	/**
	 * Is there a Sync clause ??.
	 */
	public abstract boolean isSync();

	/**
	 * Gets the value of the value property.
	 */
	public abstract String getValue();


	/**
	 * is the field redefined ???
	 */
	public abstract boolean isFieldRedefined();

	/**
	 * does the fields redefine another field
	 */
	public abstract boolean isFieldRedefines();

	/**
	 * is the usage inherited from an upper level
	 */
	public abstract boolean isInheritedUsage();

	/**
	 * is the usage inherited from an upper level
	 */
	public boolean isBlankWhenZero();

	public int getRelativeLevel();

//	public int getDoubleByteChars();
}