/*************************************************************
 * This file is part of CB2XML.
 * See the file "LICENSE" for copyright information and the
 * terms and conditions for copying, distribution and
 * modification of CB2XML.
 *************************************************************
 */

package net.sf.cb2xml;

import java.text.DecimalFormat;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.DialectManager;
import net.sf.cb2xml.def.NumericDefinition;
import net.sf.cb2xml.sablecc.analysis.DepthFirstAdapter;
import net.sf.cb2xml.sablecc.node.ABinaryUsagePhrase;
import net.sf.cb2xml.sablecc.node.ABlankWhenZeroClause;
import net.sf.cb2xml.sablecc.node.ABlankWhenZeroClauseClause;
import net.sf.cb2xml.sablecc.node.AComp1UsagePhrase;
import net.sf.cb2xml.sablecc.node.AComp2UsagePhrase;
import net.sf.cb2xml.sablecc.node.AComp3UsagePhrase;
import net.sf.cb2xml.sablecc.node.AComp4UsagePhrase;
import net.sf.cb2xml.sablecc.node.AComp5UsagePhrase;
import net.sf.cb2xml.sablecc.node.AComp6UsagePhrase;
import net.sf.cb2xml.sablecc.node.ACompUsagePhrase;
import net.sf.cb2xml.sablecc.node.ADisplay1UsagePhrase;
import net.sf.cb2xml.sablecc.node.ADisplayUsagePhrase;
import net.sf.cb2xml.sablecc.node.AFixedOccursFixedOrVariable;
import net.sf.cb2xml.sablecc.node.AFunctionPointerUsagePhrase;
import net.sf.cb2xml.sablecc.node.AIndexUsagePhrase;
import net.sf.cb2xml.sablecc.node.AItem;
import net.sf.cb2xml.sablecc.node.AJustifiedClause;
import net.sf.cb2xml.sablecc.node.ALeadingLeadingOrTrailing;
import net.sf.cb2xml.sablecc.node.ANationalUsagePhrase;
import net.sf.cb2xml.sablecc.node.AObjectReferencePhrase;
import net.sf.cb2xml.sablecc.node.AOccursTo;
import net.sf.cb2xml.sablecc.node.APackedDecimalUsagePhrase;
import net.sf.cb2xml.sablecc.node.APictureClause;
import net.sf.cb2xml.sablecc.node.APointerUsagePhrase;
import net.sf.cb2xml.sablecc.node.AProcedurePointerUsagePhrase;
import net.sf.cb2xml.sablecc.node.ARecordDescription;
import net.sf.cb2xml.sablecc.node.ARedefinesClause;
import net.sf.cb2xml.sablecc.node.ASequenceLiteralSequence;
import net.sf.cb2xml.sablecc.node.ASignClause;
import net.sf.cb2xml.sablecc.node.ASingleLiteralSequence;
import net.sf.cb2xml.sablecc.node.ASynchronizedClauseClause;
import net.sf.cb2xml.sablecc.node.AThroughSequenceLiteralSequence;
import net.sf.cb2xml.sablecc.node.AThroughSingleLiteralSequence;
import net.sf.cb2xml.sablecc.node.ATrailingLeadingOrTrailing;
import net.sf.cb2xml.sablecc.node.AValueClause;
import net.sf.cb2xml.sablecc.node.AValueItem;
import net.sf.cb2xml.sablecc.node.AVariableOccursFixedOrVariable;
import net.sf.cb2xml.sablecc.node.PDataNameOrFiller;
import net.sf.cb2xml.sablecc.node.TAlphanumericLiteral;
import net.sf.cb2xml.sablecc.node.THighValues;
import net.sf.cb2xml.sablecc.node.TLowValues;
import net.sf.cb2xml.sablecc.node.TNulls;
import net.sf.cb2xml.sablecc.node.TNumber88;
import net.sf.cb2xml.sablecc.node.TNumberNot88;
import net.sf.cb2xml.sablecc.node.TQuotes;
import net.sf.cb2xml.sablecc.node.TSpaces;
import net.sf.cb2xml.sablecc.node.TZeros;
import net.sf.cb2xml.sablecc.node.Token;
import net.sf.cb2xml.sablecc.parser.Parser;
import net.sf.cb2xml.util.XmlUtils;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Main logic for translating the parse tree of SableCC into XML.
 * Currently the XML element and attribute names are hardcoded.
 *
 * All the inA* methods are fired when the corresponding node is "visited".
 * Each node name corresponds to each "production" etc. within the grammar file.
 *
 * The "tree walking" approach generates the XML DOM in a very inituitive manner.
 *
 * @author Peter Thomas
 *
 * @version .91a Bruce Martin
 * <p>
 * <ol compact>
 * <li>Change ---... to ===... in comments
 * <li>Increment Position when initial level>01
 * <li>Convert Picture to uppercase (unlike java, Cobol is not case sensitive)
 * <li>Added storage-length (actual field length in bytes) based on Mainframe
 *     sizes
 * </ul>
 * @version .92a Jean-Francois Gagnon
 * <p>
 * <ol compact>
 * <li>Implemented Bruce Martin's changes in version 0.92
 * <li>Added a sign-position=("trailing"|"leading") attribute
 * <li>Fixed the Sign Separate size computation (changes the storage length only)
 * <li>Made the CopyBookAnalyzer class constructor public
 * </ul>
 * @version .95 Bruce Martin<ol>
 * <li>Adding new interface NumericDefinition to encapsulate binary size / position calculation
 * <li>Adding support for PC Cobol's,
 * <li>Adding size calculation for comp-1, comp-2.
 * <li>Adding support for comp-6
 * <li>Ensure comp-1 / comp-2 fields are tagged as numeric. Comp-1 / 2 fields are floating point
 * numbers and do not have a picture clause i.e.
 *      03 float              comp-1.
 *      03 double             comp-2.
 * <li>Changing position calculation for sync verb.
 * <li>Add blank-when-zero attribute to xml
 * <li>allow numeric-definition to be passed in as a parameter
 * </ol>
 */

public class CopyBookAnalyzer extends DepthFirstAdapter {

	private static NumericDefinition defaultNumDef = DialectManager.MAINFRAME_NUMERIC_DEFINITION;

	

	public CopyBookAnalyzer(String copyBookName, Parser parser) {
		this(copyBookName, parser, defaultNumDef);
	}

	
	public CopyBookAnalyzer(String copyBookName, Parser parser, NumericDefinition numericDefinition) {
		this.copyBookName = copyBookName;
		this.parser = parser; 
		this.numDef = numericDefinition; 
	}


	private final NumericDefinition numDef;
	private final Parser parser;
	private String copyBookName;
	private Document document;
	private Item prevItem, curItem;

	// our internal representation of a copybook "item" node
	class Item {
		int level;
		Element element;
		// constructor
		Item(int level, String name) {
			this.level = level;
			element = document.createElement(Cb2xmlConstants.ITEM);
			element.setAttribute(Cb2xmlConstants.LEVEL, new DecimalFormat("00").format(level));
			element.setAttribute(Cb2xmlConstants.NAME, name);
		}
		// default constructor
		Item() {
		}
	}

	// getter for XML document
	public Document getDocument() {
		return document;
	}

	// enter copybook, set up XML DOM and root element
	public void inARecordDescription(ARecordDescription node) {
	    document = XmlUtils.getNewXmlDocument();
	    Element root = document.createElement("copybook");
	    root.setAttribute(Cb2xmlConstants.FILENAME, copyBookName);
	    document.appendChild(root);
	}

	// exit root element, save XML as file
	public void outARecordDescription(ARecordDescription node) {
		Element el;
		int lastPos = 1;
		NodeList nodeList = document.getDocumentElement().getChildNodes();
		for (int i = 0; i < nodeList.getLength(); i++) {
			org.w3c.dom.Node testNode = nodeList.item(i);
			if (testNode.getNodeType() == org.w3c.dom.Node.ELEMENT_NODE) {
			    el = (Element) nodeList.item(i);

			    if ("01".equals(el.getAttribute(Cb2xmlConstants.LEVEL))) {
			        postProcessNode(el, 1);
			    } else {
			        lastPos = postProcessNode(el, lastPos);
			    }
			}
		}
	}

	// check for comments before these Tokens and add to XML
	public void caseTNumberNot88(TNumberNot88 node) {
		checkForComments(node);
	}

	public void caseTNumber88(TNumber88 node) {
		checkForComments(node);
	}

	public void checkForComments(Token node) {
		List list = (List) parser.ignoredTokens.getIn(node);
		if (list != null) {
			Iterator i = list.iterator();
			while (i.hasNext()) {
				String s = i.next().toString().trim();
				if (s.length() > 0) {
                    curItem.element.getParentNode().insertBefore(
                            document.createComment(correctForMinus(s)),
                            curItem.element);
				}
			}
		}
	}


    /**
     * Replace '-' chars with '=' to avoid invalid XML comments
     *
     * @param s input string Comment
     * @return corrected comment
     */
    private String correctForMinus(String s) {
        int start = s.indexOf("--");
        if (start >= 0){
            int i=start;
            StringBuffer buf = new StringBuffer(s);
            boolean wasMinus = false;

            while (i < s.length()-1) {
            	if (buf.charAt(i) == '-' && (wasMinus || buf.charAt(i + 1) == '-')) {
            		buf.setCharAt(i, '=');
            		wasMinus = true;
            	} else {
            		wasMinus = false;
            	}
                i += 1;
            }
            i = s.length()-1;
            if (buf.charAt(i) == '-' && wasMinus) {
            	buf.setCharAt(i, '=');
            }
            s = buf.toString();
        }

        return s;
    }


    // main elementary item

	// enter item, set up Item object
	public void inAItem(AItem node) {
		int level = Integer.parseInt(node.getNumberNot88().toString().trim());
		PDataNameOrFiller dataName = node.getDataNameOrFiller();
		String name = "";
		if (dataName != null) {
			name = dataName.toString().trim();
		}
		curItem = new Item(level, name);
		if (level <= 77) {
			if (prevItem == null) {
				document.getDocumentElement().appendChild(curItem.element);
			} else if (curItem.level > prevItem.level) {
				prevItem.element.appendChild(curItem.element);
			} else if (curItem.level == prevItem.level) {
				prevItem.element.getParentNode().appendChild(curItem.element);
			} else if (curItem.level < prevItem.level) {
				Element tempElement = prevItem.element;
				while (true) {
					tempElement = (Element) tempElement.getParentNode();
					String tempLevel = tempElement.getAttribute(Cb2xmlConstants.LEVEL);
					if ("".equals(tempLevel)) {
						// we reached the root / document element!
						// start of a separate record structure, append to root as top level
						tempElement.appendChild(curItem.element);
						break;
					}
					int tempLevelNumber = Integer.parseInt(tempLevel);
					if (tempLevelNumber == curItem.level){ // sibling
						tempElement.getParentNode().appendChild(curItem.element);
						break;
					} else if (tempLevelNumber < curItem.level) {
						tempElement.appendChild(curItem.element);
						break;
					}
				}
			}
			prevItem = curItem;
		}
	}

	public void inARedefinesClause(ARedefinesClause node) {
		String dataName = node.getDataName().getText();
		//curItem.element.setAttribute(Attributes.ATTR_REDEFINES, getJavaName(dataName));
		curItem.element.setAttribute(Cb2xmlConstants.REDEFINES, dataName);
	}

	public void inAFixedOccursFixedOrVariable(AFixedOccursFixedOrVariable node) {
		curItem.element.setAttribute(Cb2xmlConstants.OCCURS, node.getNumber().toString().trim());
	}

	public void inAVariableOccursFixedOrVariable(
			AVariableOccursFixedOrVariable node) {
		curItem.element.setAttribute(Cb2xmlConstants.OCCURS, node.getNumber().toString().trim());
		curItem.element.setAttribute(Cb2xmlConstants.DEPENDING_ON, node.getDataName().getText());
	}

	public void inAOccursTo(AOccursTo node) {
		curItem.element.setAttribute(Cb2xmlConstants.OCCURS_MIN, node.getNumber().toString()
				.trim());
	}

    public void inASynchronizedClauseClause(ASynchronizedClauseClause node)
    {
     	curItem.element.setAttribute(Cb2xmlConstants.SYNC, Cb2xmlConstants.TRUE);
    }

	//============================= PICTURE CLAUSE ===================================

	public void inAPictureClause(APictureClause node) {

		boolean positive = true;
		String characterString = removeChars(node.getCharacterString().toString()," ");
		curItem.element.setAttribute(Cb2xmlConstants.PICTURE, characterString);
		if (characterString.charAt(0) == 'S' || characterString.charAt(0) == 's') {
			curItem.element.setAttribute(Cb2xmlConstants.SIGNED, Cb2xmlConstants.TRUE);
			characterString = characterString.substring(1);
			positive = false;
		}
		int displayLength = 0, assumedDigitsBeforeDecimal=0, assumedDigitsAfterDecimal=0, 
				doubleByteChars = 0, lastSizeAdj = 1, currSizeAdj;
			/* change "length" to "display-length" - bm  ??*/
		if (curItem.element.hasAttribute(Cb2xmlConstants.DISPLAY_LENGTH)) {
			displayLength = Integer.parseInt(curItem.element.getAttribute(Cb2xmlConstants.DISPLAY_LENGTH));
		}
//		if (curItem.element.hasAttribute(Cb2xmlConstants.STORAGE_LENGTH)) {
//			storageLength = Integer.parseInt(curItem.element.getAttribute(Cb2xmlConstants.STORAGE_LENGTH));
//		}
		int decimalPos = -1;
		boolean isNumeric = false, isEditNumeric=false;
		boolean isFirstCurrencySymbol = true;
		String ucCharacterString = characterString.toUpperCase();
		for (int i = 0; i < characterString.length(); i++) {
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
			case '0':
			case '+':
			case '-':
			case '*':
				isEditNumeric = true;
			case '9':
				isNumeric = true;
			case '/':
			case ',':
			case 'X':
				displayLength++;
				break;
			case '(':
				int endParenPos = characterString.indexOf(')', i + 1);
				int count = Integer.parseInt(characterString.substring(i + 1,
						endParenPos));
				i = endParenPos;
				doubleByteChars += (count - 1) * lastSizeAdj;
				displayLength += count - 1;
			}
			lastSizeAdj = currSizeAdj;
		}

		if (isNumeric) {
			curItem.element.setAttribute(Cb2xmlConstants.NUMERIC, Cb2xmlConstants.TRUE);
			if (isEditNumeric) {
				curItem.element.setAttribute(Cb2xmlConstants.EDITTED_NUMERIC, Cb2xmlConstants.TRUE);
			}
		}
        setLength(curItem.element, positive, displayLength, assumedDigitsBeforeDecimal + assumedDigitsAfterDecimal, doubleByteChars);
		//curItem.element.setAttribute(Attributes.DISPLAY_LENGTH, displayLength + "");
		//curItem.element.setAttribute("bytes", bytes + "");
		if (decimalPos != -1 && (displayLength - decimalPos != assumedDigitsAfterDecimal)) {
			curItem.element.setAttribute(Cb2xmlConstants.SCALE, displayLength - decimalPos + "");
			if (characterString.indexOf('.') != -1) {
				curItem.element.setAttribute(Cb2xmlConstants.INSERT_DECIMAL_POINT, Cb2xmlConstants.TRUE);
			}
		} else if (assumedDigitsBeforeDecimal > 0) {
			curItem.element.setAttribute(Cb2xmlConstants.SCALE, "-" + assumedDigitsBeforeDecimal);
		}
	}




	public void inASignClause(ASignClause node) {
		if (node.getSeparateCharacter() != null) {
			curItem.element.setAttribute(Cb2xmlConstants.SIGN_SEPARATE, Cb2xmlConstants.TRUE);
			// No need to change the display length for the sign clause
			// As for the storage length, it is only computed in one place. JFG
			//int length = 1, bytes = 1;
			//if (curItem.element.hasAttribute(Attributes.DISPLAY_LENGTH)) {
			//	length = Integer.parseInt(curItem.element.getAttribute(Attributes.DISPLAY_LENGTH))
			//			+ length;
			//}
			//curItem.element.setAttribute(Attributes.DISPLAY_LENGTH, length + "");
			//if (curItem.element.hasAttribute("bytes")) {
			//	bytes = Integer.parseInt(curItem.element.getAttribute("bytes"))
			//			+ bytes;
			//}
			// curItem.element.setAttribute("bytes", bytes + "");
		}
	}

	// Added the processing to capture the sign position JFG
    public void inALeadingLeadingOrTrailing(ALeadingLeadingOrTrailing node)
    {
        curItem.element.setAttribute(Cb2xmlConstants.SIGN_POSITION, Cb2xmlConstants.LEADING);
    }

    public void inATrailingLeadingOrTrailing(ATrailingLeadingOrTrailing node)
    {
        curItem.element.setAttribute(Cb2xmlConstants.SIGN_POSITION, Cb2xmlConstants.TRAILING);
    }

	/**
	 * @see net.sf.cb2xml.sablecc.analysis.DepthFirstAdapter#inABlankWhenZeroClause(net.sf.cb2xml.sablecc.node.ABlankWhenZeroClause)
	 */
	public void inABlankWhenZeroClause(ABlankWhenZeroClause node) {
		curItem.element.setAttribute(Cb2xmlConstants.BLANK_WHEN_ZERO, Cb2xmlConstants.TRUE);
		super.inABlankWhenZeroClause(node);
	}

	/**
	 * @see net.sf.cb2xml.sablecc.analysis.DepthFirstAdapter#inABlankWhenZeroClauseClause(net.sf.cb2xml.sablecc.node.ABlankWhenZeroClauseClause)
	 */
	public void inABlankWhenZeroClauseClause(ABlankWhenZeroClauseClause node) {
		curItem.element.setAttribute(Cb2xmlConstants.BLANK_WHEN_ZERO, Cb2xmlConstants.TRUE);
		super.inABlankWhenZeroClauseClause(node);
	}


	//======================= USAGE CLAUSE ==========================


	public void inABinaryUsagePhrase(ABinaryUsagePhrase node) {
		curItem.element.setAttribute(Cb2xmlConstants.USAGE, Cb2xmlConstants.BINARY);
		curItem.element.setAttribute(Cb2xmlConstants.NUMERIC, Cb2xmlConstants.TRUE);
	}

	public void inACompUsagePhrase(ACompUsagePhrase node) {
		curItem.element.setAttribute(Cb2xmlConstants.USAGE, Cb2xmlConstants.COMP);
		curItem.element.setAttribute(Cb2xmlConstants.NUMERIC, Cb2xmlConstants.TRUE);
	}

	public void inAComp1UsagePhrase(AComp1UsagePhrase node) {
		curItem.element.setAttribute(Cb2xmlConstants.USAGE, Cb2xmlConstants.COMP_1);
		curItem.element.setAttribute(Cb2xmlConstants.NUMERIC, Cb2xmlConstants.TRUE);
	}

	public void inAComp2UsagePhrase(AComp2UsagePhrase node) {
		curItem.element.setAttribute(Cb2xmlConstants.USAGE, Cb2xmlConstants.COMP_2);
		curItem.element.setAttribute(Cb2xmlConstants.NUMERIC, Cb2xmlConstants.TRUE);
	}

	public void inAComp3UsagePhrase(AComp3UsagePhrase node) {
		curItem.element.setAttribute(Cb2xmlConstants.USAGE, Cb2xmlConstants.COMP_3);
		curItem.element.setAttribute(Cb2xmlConstants.NUMERIC, Cb2xmlConstants.TRUE);
	}

	public void inAComp4UsagePhrase(AComp4UsagePhrase node) {
		curItem.element.setAttribute(Cb2xmlConstants.USAGE, Cb2xmlConstants.COMP_4);
		curItem.element.setAttribute(Cb2xmlConstants.NUMERIC, Cb2xmlConstants.TRUE);
	}


	public void inAComp5UsagePhrase(AComp5UsagePhrase node) {
		curItem.element.setAttribute(Cb2xmlConstants.USAGE, Cb2xmlConstants.COMP_5);
		curItem.element.setAttribute(Cb2xmlConstants.NUMERIC, Cb2xmlConstants.TRUE);
	}


	public void inAComp6UsagePhrase(AComp6UsagePhrase node) {
		curItem.element.setAttribute(Cb2xmlConstants.USAGE, Cb2xmlConstants.COMP_6);
		curItem.element.setAttribute(Cb2xmlConstants.NUMERIC, Cb2xmlConstants.TRUE);
	}

	public void inADisplayUsagePhrase(ADisplayUsagePhrase node) {
		curItem.element.setAttribute(Cb2xmlConstants.USAGE, Cb2xmlConstants.DISPLAY);
	}

	public void inADisplay1UsagePhrase(ADisplay1UsagePhrase node) {
		curItem.element.setAttribute(Cb2xmlConstants.USAGE, Cb2xmlConstants.DISPLAY_1);
	}

	public void inAIndexUsagePhrase(AIndexUsagePhrase node) {
		curItem.element.setAttribute(Cb2xmlConstants.USAGE, "index");
	}

	public void inANationalUsagePhrase(ANationalUsagePhrase node) {
		curItem.element.setAttribute(Cb2xmlConstants.USAGE, "national");
	}

	public void inAObjectReferencePhrase(AObjectReferencePhrase node) {
		curItem.element.setAttribute("object-reference", node.getDataName().getText());
	}

	public void inAPackedDecimalUsagePhrase(APackedDecimalUsagePhrase node) {
		curItem.element.setAttribute(Cb2xmlConstants.USAGE, Cb2xmlConstants.PACKED_DECIMAL);
	}

	public void inAPointerUsagePhrase(APointerUsagePhrase node) {
		curItem.element.setAttribute(Cb2xmlConstants.USAGE, "pointer");
	}

	public void inAProcedurePointerUsagePhrase(AProcedurePointerUsagePhrase node) {
		curItem.element.setAttribute(Cb2xmlConstants.USAGE, "procedure-pointer");
	}

	public void inAFunctionPointerUsagePhrase(AFunctionPointerUsagePhrase node) {
		curItem.element.setAttribute(Cb2xmlConstants.USAGE, "function-pointer");
	}
	
	
	/* (non-Javadoc)
	 * @see net.sf.cb2xml.sablecc.analysis.DepthFirstAdapter#inAJustifiedClause(net.sf.cb2xml.sablecc.node.AJustifiedClause)
	 */
	public void inAJustifiedClause(AJustifiedClause node) {
		String s = Cb2xmlConstants.RIGHT;
		if (node.getRight() == null) {
			s = Cb2xmlConstants.TRUE;
		}
		curItem.element.setAttribute(Cb2xmlConstants.JUSTIFIED, s);
		
//		super.inAJustifiedClause(node);
	}



	//	======================= 88 / VALUE CLAUSE ==========================

	public void caseTZeros(TZeros node) {
		node.setText("zeros");
	}

	public void caseTSpaces(TSpaces node) {
		node.setText("spaces");
	}

	public void caseTHighValues(THighValues node) {
		node.setText("high-values");
	}

	public void caseTLowValues(TLowValues node) {
		node.setText("low-values");
	}

	public void caseTQuotes(TQuotes node) {
		node.setText("quotes");
	}

	public void caseTNulls(TNulls node) {
		node.setText("nulls");
	}


	public void caseTAlphanumericLiteral(TAlphanumericLiteral node) {
		String nodeText = node.getText();
		if (nodeText.startsWith("X")) {
			node.setText(nodeText.replace('\"', '\''));
		} else {
			int st = 0;
			int en = nodeText.length();
			StringBuilder b = new StringBuilder(en);
			char ch;
			char lastChar = (char) -1;
			boolean skip = false;
			boolean addQuote = false;
			
			if (nodeText.startsWith("'") || nodeText.startsWith("\"")) {
				st = 1;
				if (nodeText.endsWith(nodeText.substring(0, 1))) {
					en -= 1;
				}
				addQuote = true;
			}
			for (int i = st; i < en; i++) {
				ch = nodeText.charAt(i);
				if (skip) {
					switch (ch) {
					case ' ':
					case '\t':
					case '\r':
					case '-':
						break;
					default:
						lastChar = (char) -1;
						skip = false;
					}
				} else {
					switch (ch) {
					case '\n':
						skip = true;
						lastChar = ch;
						break;
					case '\'':
					case '\"':
						if (ch == lastChar) {
							lastChar = (char) -1 ;
							break;
						}
					default:
						b.append(ch);
						lastChar = ch;
					}
				}
			}
			
		
			if (addQuote) {
				char q = '"';
				if (b.indexOf("\"") > 0) {
					q = '\'';
					adj(b);
				}
				b.insert(0, q);
				b.append(q);
			} 
			node.setText(b.toString());
		}
	}

	public static void adj(StringBuilder b) {
		int len = b.length() - 2;
		if (b.charAt(b.length() - 1) == '\'') {
			b.append('\'');
		}
		for (int i = len; i >= 0; i--) {
			if (b.charAt(i) == '\'') {
				b.insert(i+1, '\'');
			}
		}
	}


	public void outAValueClause(AValueClause node) {
		curItem.element.setAttribute(Cb2xmlConstants.VALUE, node.getLiteral().toString().trim());
	}

	// 88 LEVEL CONDITION NODE
	public void inAValueItem(AValueItem node) {
		String name = node.getDataName().getText();
		curItem = new Item();
		curItem.element = document.createElement(Cb2xmlConstants.CONDITION);
		// curItem.element.setAttribute(Attributes.ATTR_LEVEL, "88");
		curItem.element.setAttribute(Cb2xmlConstants.NAME, name);
		prevItem.element.appendChild(curItem.element);
	}

	public void outASingleLiteralSequence(ASingleLiteralSequence node) {
		if (node.getAll() != null) {
			curItem.element.setAttribute(Cb2xmlConstants.ALL, Cb2xmlConstants.TRUE);
		}
		Element element = document.createElement(Cb2xmlConstants.CONDITION);
		element.setAttribute(Cb2xmlConstants.VALUE, node.getLiteral().toString().trim());
		curItem.element.appendChild(element);
	}

	public void outASequenceLiteralSequence(ASequenceLiteralSequence node) {
		Element element = document.createElement(Cb2xmlConstants.CONDITION);
		element.setAttribute(Cb2xmlConstants.VALUE, node.getLiteral().toString().trim());
		curItem.element.appendChild(element);
	}

	public void outAThroughSingleLiteralSequence(AThroughSingleLiteralSequence node) {
		Element element = document.createElement(Cb2xmlConstants.CONDITION);
		element.setAttribute(Cb2xmlConstants.VALUE, node.getFrom().toString().trim());
		element.setAttribute(Cb2xmlConstants.THROUGH, node.getTo().toString().trim());
		curItem.element.appendChild(element);
	}

	public void outAThroughSequenceLiteralSequence(AThroughSequenceLiteralSequence node) {
		Element element = document.createElement(Cb2xmlConstants.CONDITION);
		element.setAttribute(Cb2xmlConstants.VALUE, node.getFrom().toString().trim());
		element.setAttribute(Cb2xmlConstants.THROUGH, node.getTo().toString().trim());
		curItem.element.appendChild(element);
	}



	//===============================================================================


	

	private String removeChars(String s, String charToRemove) {
		StringTokenizer st = new StringTokenizer(s, charToRemove, false);
		StringBuffer b = new StringBuffer();
		while (st.hasMoreElements()) {
			b.append(st.nextElement());
		}
		return b.toString();
	}

	/**
	 * This is for DOM post-processing of the XML before saving to resolve the field lengths
	 * of each node and also calculate the start position of the data field in the
	 * raw copybook buffer (mainframe equivalent)
	 * recursive traversal.  note that REDEFINES declarations are taken care of
	 * as well as the OCCURS declarations
	 */

	private int postProcessNode(Element element, int startPos) {
	    int actualLength = 0;
		int displayLength = 0;
		int assumedDigits = 0;
		int doubleByteChars = 0;
		int newPos;
		int oldEnd = startPos;

		String usage = getUsage(element);
		
		if (element.hasAttribute(Cb2xmlConstants.REDEFINES)) {
			String redefinedName = element.getAttribute(Cb2xmlConstants.REDEFINES);
			Element redefinedElement = null;
			// NodeList nodeList = ((Element) element.getParentNode()).getElementsByTagName(Attributes.ITEM);
			NodeList nodeList = document.getDocumentElement().getElementsByTagName(Cb2xmlConstants.ITEM);
			for (int i = 0; i < nodeList.getLength(); i++) {
				Element testElement = (Element) nodeList.item(i);
				if (testElement.getAttribute(Cb2xmlConstants.NAME).equalsIgnoreCase(redefinedName)) {
					redefinedElement = testElement;
					break;
				}
			}
			if (redefinedElement != null && redefinedElement.hasAttribute(Cb2xmlConstants.POSITION)) {
				startPos = Integer.parseInt(redefinedElement.getAttribute(Cb2xmlConstants.POSITION));
				redefinedElement.setAttribute(Cb2xmlConstants.REDEFINED, Cb2xmlConstants.TRUE); 
			} else {
				System.out.println(">> position error " + element.getAttribute(Cb2xmlConstants.NAME) + " %% "+ redefinedName);
			}
		}

		newPos = startPos;
		if (element.hasAttribute(Cb2xmlConstants.DISPLAY_LENGTH)) {
			displayLength = Integer.parseInt(element.getAttribute(Cb2xmlConstants.DISPLAY_LENGTH));
			if (element.hasAttribute(Cb2xmlConstants.ASSUMED_DIGITS)) {
				assumedDigits = Integer.parseInt(element.getAttribute(Cb2xmlConstants.ASSUMED_DIGITS));
			}
			if (element.hasAttribute(Cb2xmlConstants.DOUBLE_BYTE_CHARS)) {
				doubleByteChars = Integer.parseInt(element.getAttribute(Cb2xmlConstants.DOUBLE_BYTE_CHARS));
			}
		} else {
			NodeList nodeList = element.getChildNodes();
			for (int i = 0; i < nodeList.getLength(); i++) {
				org.w3c.dom.Node testNode = nodeList.item(i);
				if (testNode.getNodeType() == org.w3c.dom.Node.ELEMENT_NODE) {
					Element childElement = (Element) testNode;
					if (!childElement.getTagName().equals(Cb2xmlConstants.CONDITION)) {
						newPos = postProcessNode(childElement, newPos);
					}
				}
				displayLength = newPos - startPos;
				//System.out.println("Testing Group: " + startPos + " + " + displayLength + " -> " + newPos);
			}
			//System.out.println(" --> " + startPos + " " + displayLength);
			//element.setAttribute(Attributes.DISPLAY_LENGTH, thisElementLength + "");
			//setLength(element, thisElementLength);
		}
		actualLength = setLength(element, ! Cb2xmlConstants.TRUE.equals(element.getAttribute(Cb2xmlConstants.SIGNED)), 
				displayLength, assumedDigits, doubleByteChars);

		int syncOn = 1;
		int remainder;
		if (element.hasAttribute(Cb2xmlConstants.SYNC)) {
			syncOn = numDef.getSyncAt(usage, actualLength);
//		}
//		if (syncOn > 1) {
			remainder = (startPos - 1) % syncOn;
			if (remainder >0) {
				startPos = startPos - remainder + syncOn;
			}
		}
		element.setAttribute(Cb2xmlConstants.POSITION, Integer.toString(startPos));

		if (element.hasAttribute(Cb2xmlConstants.OCCURS)) {
		    actualLength *= Integer.parseInt(element.getAttribute(Cb2xmlConstants.OCCURS));
		}

		return Math.max(oldEnd, startPos + actualLength);
	}


	/**
	 * Assigning display and actual length to current element
	 *
	 */
	private int setLength(Element element, boolean positive, int displayLength, int assumedDigits, int doubleByteChars) {
	    int storageLength = displayLength - assumedDigits + doubleByteChars;

	    if (hasChildItems(element)) {

	    } else {
		    String usage = getUsage(element);
	    	if (usage != null && usage.length() > 0) {
		    	if (numDef != null) {
			    	displayLength = numDef.chkStorageLength(displayLength, usage);
			        storageLength = numDef.getBinarySize(usage, storageLength, positive, element.hasAttribute(Cb2xmlConstants.SYNC));
		    	}
		    } else if (element.hasAttribute(Cb2xmlConstants.SIGN_SEPARATE)
		    		&& Cb2xmlConstants.TRUE.equalsIgnoreCase(element.getAttribute(Cb2xmlConstants.SIGN_SEPARATE))) {
		    	storageLength += 1;
		    	displayLength += 1;
		    }
	    }

	    element.setAttribute(Cb2xmlConstants.DISPLAY_LENGTH, Integer.toString(displayLength));
	    element.setAttribute(Cb2xmlConstants.STORAGE_LENGTH, Integer.toString(storageLength));
	    if (doubleByteChars > 0) {
	    	element.setAttribute(Cb2xmlConstants.DOUBLE_BYTE_CHARS, Integer.toString(doubleByteChars));
	    }
	    if (assumedDigits != 0) { 
	    	element.setAttribute(Cb2xmlConstants.ASSUMED_DIGITS, Integer.toString(assumedDigits));
	    }

	    return storageLength;
	}
	
	private boolean hasChildItems(Element element) {
		if (element.hasChildNodes()) {
			NodeList childNodes = element.getChildNodes();
			for (int i = childNodes.getLength() - 1; i >= 0; i--) {
				if (childNodes.item(i).getNodeType() == org.w3c.dom.Node.ELEMENT_NODE) {
					Element childElement = (Element) childNodes.item(i);
					if (!childElement.getTagName().equals(Cb2xmlConstants.CONDITION)) {
						return true;
					}
				}
			}
		}
			return false;
	}

	private String getUsage(Element element) {
		String usage = "";
		if (element.hasAttribute(Cb2xmlConstants.USAGE)) {
	    	usage = element.getAttribute(Cb2xmlConstants.USAGE);
		} else if (Cb2xmlConstants.TRUE.equalsIgnoreCase(element.getAttribute(Cb2xmlConstants.NUMERIC))
			   && (! Cb2xmlConstants.TRUE.equalsIgnoreCase(element.getAttribute(Cb2xmlConstants.EDITTED_NUMERIC)))) {
			Node node =  element;
			do {
				node =  node.getParentNode();
			} while (node != null && (node instanceof Element) && (! ((Element) node).hasAttribute(Cb2xmlConstants.USAGE)));
			
			if (node != null && (node instanceof Element)) {
				usage = ((Element) node).getAttribute(Cb2xmlConstants.USAGE);
				if (usage != null && usage.length() > 0) {
					element.setAttribute(Cb2xmlConstants.INHERITED_USAGE, Cb2xmlConstants.TRUE);
					element.setAttribute(Cb2xmlConstants.USAGE, usage);
				}
			}
		}
		return usage;
	}
	
	/**
	 * Set the possible Sizes for Comp fields
	 * @param numericDef numeric definition class
	 */
	public static void setNumericDetails(NumericDefinition numericDef) {
		defaultNumDef = numericDef;
	}

}