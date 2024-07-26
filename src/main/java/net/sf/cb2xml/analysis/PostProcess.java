package net.sf.cb2xml.analysis;

import java.util.List;

import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.NumericDefinition;
import net.sf.cb2xml.def.Cb2xmlConstants.Usage;

/**
 * This class will calculate Storage/Display Lengths/Positions for either a Cobol Item 
 * or a Cobol Copybook
 * @author Bruce Martin
 *
 */
public class PostProcess {

	private final NumericDefinition dialectDetails;
	private final boolean recalculateLength;

	/**
	 * This class will calculate Storage/Display Lengths/Positions for either a Cobol Item 
	 * or a Cobol Copybook
	 * @param cobolDialectDefinition Cobol Dialect definition
	 * @param recalculateLength whether to recalculate lengths or use those already calculated
	 */
	public PostProcess(NumericDefinition cobolDialectDefinition, boolean recalculateLength) {
		super();
		this.dialectDetails = cobolDialectDefinition;
		this.recalculateLength = recalculateLength;
	}

	/**
	 * Postprocess (recalulate positions etc) a Copybook
	 * @param copybook Copybook to be updated
	 */
	public void postProcessNode(Copybook copybook) {
		Positions lastPos = new Positions();
		for (Item itm : copybook.getChildItems()) {
			if (itm.levelNumber == 1) {
				postProcessNode(itm, new Positions());
			} else {
				lastPos = postProcessNode(itm, lastPos);
			}
		}
	}
	
	/**
	 * This is for DOM post-processing of the XML before saving to resolve the field lengths
	 * of each node and also calculate the start position of the data field in the
	 * raw copybook buffer (mainframe equivalent)
	 * recursive traversal.  note that REDEFINES declarations are taken care of
	 * as well as the OCCURS declarations
	 */
	
	public Positions postProcessNode(Item element, Positions position) {
	    int storageLength = 0;
		int displayLength = 0;
		int assumedDigits = 0;
		int doubleByteChars = 0;
		Positions oldEnd = position.clonePos();

		String usage = getUsage(element);
		
		if (element.redefines != null && element.redefines.length() > 0) {
			String redefinedName = element.redefines;
			Item redefinedElement = null;
			List<? extends Item> childItems = element.parentItem.getChildItems();
			
			for (Item item : childItems) {
				if (redefinedName.equalsIgnoreCase(item.getFieldName())) {
					redefinedElement = item;
					break;
				}
				if (item == element) {
					break;
				}
			}

			if (redefinedElement == null) {
				System.out.println("\n>> " + element.getFieldName() 
				+ ": Could not find the redefined field  >> "+ redefinedName);
			} else if (redefinedElement.position >= 0) {
				position.set(redefinedElement.position, redefinedElement.displayPosition);
				redefinedElement.fieldRedefined = true; 
//			} else if (redefinedElement != null && redefinedElement.levelNumber == 1) {
//				position.set(0,  0);
//
//				redefinedElement.fieldRedefined = true; 
			} else {
				System.out.println("\n>> " + element.getFieldName() 
					+ ": error determining record position of the field being redefined  >> "+ redefinedName);
			}
		}

		Positions newPos = position.clonePos();
		
		boolean lengthsCalculated = ! recalculateLength;
		List<? extends Item> childItems = element.getChildItems();
		if (childItems.size() == 0 && recalculateLength 
		&& element.getPicture() != null && element.getPicture().length() > 0) {
			element.displayLength = 0;
			calculateLengths(element, element.getPicture());
			lengthsCalculated = true;
		}
		if (element.displayLength > 0 && lengthsCalculated) {
			displayLength = element.displayLength;
			assumedDigits = Math.max(0, element.assumedDigits);
			doubleByteChars = Math.max(0, element.doubleByteChars);
			
			storageLength = setLength(element, ! element.signed, 
					displayLength, assumedDigits, doubleByteChars);
			displayLength = element.displayLength;
			
			for (Item itm : childItems) {
				checkNumericStatus(itm);
			}
		} else {
			if (childItems.size() == 0) {
				storageLength = setLength(element, ! element.signed, 
						Math.max(displayLength,  element.displayLength), assumedDigits, doubleByteChars);
				displayLength = element.displayLength;
			} else {
				for (Item itm : childItems) {
					newPos = postProcessNode(itm, newPos);
					displayLength = Math.max(displayLength, newPos.display - position.display);
					storageLength  = Math.max(storageLength, newPos.storage - position.storage);
				}
				element.displayLength = displayLength;
				element.storageLength = storageLength;
			}
		}
//		actualLength = setLength(element, ! element.signed, 
//				displayLength, assumedDigits, doubleByteChars);
//		displayLength = element.displayLength;

		int syncOn = 1;
		int startPos = position.storage;
		int remainder;
		if (element.sync) {
			syncOn = dialectDetails.getSyncAt(usage, storageLength);

			remainder = (startPos - 1) % syncOn;
			if (remainder > 0) {
				startPos = startPos - remainder + syncOn;
				position.storage = startPos;
			}
		}
		element.position = startPos;
		element.displayPosition = position.display;

		if (element.occurs >= 0) {
		    storageLength *= element.occurs;
		    displayLength *= element.occurs;
		}
		position.storage += storageLength;
		position.display += displayLength;

		position.max(oldEnd);
		return position;
	}

	void calculateLengths(Item curItem, String pictureString) {
		boolean positive = true;
		curItem.picture =  pictureString;
		if (pictureString.length() > 0 && pictureString.charAt(0) == 'S' || pictureString.charAt(0) == 's') {
			curItem.signed = true;
			pictureString = pictureString.substring(1);
			positive = false;
		}
		int displayLength = 0, assumedDigitsBeforeDecimal=0, assumedDigitsAfterDecimal=0, 
				doubleByteChars = 0, lastSizeAdj = 1, currSizeAdj;
			/* change "length" to "display-length" - bm  ??*/
		if (curItem.displayLength >= 0) {
			displayLength = curItem.displayLength;
		}
//		if (curItem.element.hasAttribute(Cb2xmlConstants.STORAGE_LENGTH)) {
//			storageLength = Integer.parseInt(curItem.element.getAttribute(Cb2xmlConstants.STORAGE_LENGTH));
//		}
		int decimalPos = -1;
		int decimalCount = 0;
		boolean isNumeric = false, isEditNumeric=false;
		boolean isFirstCurrencySymbol = true;
		String ucCharacterString = pictureString.toUpperCase();
		for (int i = 0; i < pictureString.length(); i++) {
			char c = ucCharacterString.charAt(i);
			currSizeAdj = 0;
			switch (c) {
			case 'G':
			case 'N':
				doubleByteChars += 1;
				currSizeAdj = 1;
			case 'A':
			case 'B':
			case 'E':
				isEditNumeric = true;
				displayLength++;
				break;
			//==========================================
			case '.':
				isEditNumeric = true;
				displayLength++;
			case 'V':
				isNumeric = true;
				decimalPos = displayLength;
				break;
			//==========================================
			case 'P':
//				if (characterString.charAt(0) == 'P') {
//					decimalPos = 0;
//				}
				isNumeric = true;
				displayLength++;
				if (decimalPos <0) {
					assumedDigitsBeforeDecimal++;
				} else {
					assumedDigitsAfterDecimal++;
					decimalCount += 1;
				}
				break;
			//==========================================
			case '$':
				isEditNumeric = true;
				if (isFirstCurrencySymbol) {
					isFirstCurrencySymbol = false;
					isNumeric = true;
				} else {
					displayLength++;
				}
				break;
			//==========================================
			case 'C': // CR
			case 'D': // DR
				i++;  // skip R
			case 'Z':
			case '+':
			case '-':
			case '*':
				isEditNumeric = true;
				isNumeric = true;
				displayLength++;
				break;
			case '0':
				isEditNumeric = true;
			case '9':
				isNumeric = true;
				if (decimalPos >=0) {
					decimalCount += 1;
				}
			case '/':
			case ',':
			case 'X':
				displayLength++;
				break;
			case '(':
				int endParenPos = pictureString.indexOf(')', i + 1);
				int count = Integer.parseInt(pictureString.substring(i + 1,
						endParenPos));
				i = endParenPos;
				doubleByteChars += (count - 1) * lastSizeAdj;
				displayLength += count - 1;
				if (isNumeric && decimalPos >=0) {
					decimalCount += count - 1;
				}
			}
			lastSizeAdj = currSizeAdj;
		}

		if (isNumeric && curItem.getChildItems().size() == 0) {
			if (isEditNumeric) {
				curItem.numericClass = Cb2xmlConstants.NumericClass.NUMERIC_EDITED;
			} else {
				curItem.numericClass = Cb2xmlConstants.NumericClass.NUMERIC_IN_COBOL;
			}
		} else {
			curItem.numericClass = Cb2xmlConstants.NumericClass.NON_NUMERIC;
		}

		setLength(curItem, positive, displayLength, assumedDigitsBeforeDecimal + assumedDigitsAfterDecimal, doubleByteChars);
		//curItem.element.setAttribute(Attributes.DISPLAY_LENGTH, displayLength + "");
		//curItem.element.setAttribute("bytes", bytes + "");
		if (decimalPos != -1 && (displayLength - decimalPos != assumedDigitsAfterDecimal)) {
			curItem.scale = decimalCount; //displayLength - decimalPos;
			if (pictureString.indexOf('.') != -1) {
				curItem.insertDecimal = true;
			}
		} else if (assumedDigitsBeforeDecimal > 0) {
			curItem.scale = -assumedDigitsBeforeDecimal;
		}

	}
	
	private void checkNumericStatus(Item element) {
		if (element.getChildItems().size() > 0) {
			element.numericClass = Cb2xmlConstants.NumericClass.NON_NUMERIC;
			for (Item itm : element.getChildItems()) {
				checkNumericStatus(itm);
			}
		}
	}

	/**
	 * Assigning display and actual length to current element
	 *
	 */
	private int setLength(Item element, boolean positive, int displayLength, int assumedDigits, int doubleByteChars) {
	    int storageLength = displayLength - assumedDigits + doubleByteChars;

	    if (element.getChildItems().size() > 0) {

	    } else {
		    Usage usage = element.usage;
	    	if (usage != null && usage != Cb2xmlConstants.Usage.NONE) {
		    	if (dialectDetails != null) {
			    	displayLength = dialectDetails.chkStorageLength(displayLength, usage.getName());
			        storageLength = dialectDetails.getBinarySize(usage.getName(), storageLength, positive, element.isSync());
		    	}
		    } else if (element.signClause.signSeparate ) {
		    	storageLength += 1;
		    	displayLength += 1;
		    }
	    }

	    element.displayLength = displayLength;
	    element.storageLength = storageLength;
	    if (doubleByteChars > 0) {
	    	element.doubleByteChars = doubleByteChars;
	    }
	    if (assumedDigits != 0) { 
	    	element.assumedDigits = assumedDigits;
	    }

	    return storageLength;
	}

	private String getUsage(Item element) {
		String usage = "";
		if (element.getChildItems().size() > 0) {
			element.numericClass = Cb2xmlConstants.NumericClass.NON_NUMERIC;
		} 
		if (element.usage != Cb2xmlConstants.Usage.NONE) {
	    	usage = element.usage.getName();
		} else if (element.numericClass == Cb2xmlConstants.NumericClass.NUMERIC_IN_COBOL) {
			Item node =  element;
			while (node.parentItem != null && node.parentItem instanceof Item
			   && (node = (Item) node.parentItem).usage == Cb2xmlConstants.Usage.NONE) {}
			
			element.inheritedUsage = false;
			if (node != null && node.usage != null && node.usage != Cb2xmlConstants.Usage.NONE
			&& (element.usage != node.usage)) {
				element.inheritedUsage = true;
				element.usage = node.usage;
			}
		}
		return usage;
	}
}
