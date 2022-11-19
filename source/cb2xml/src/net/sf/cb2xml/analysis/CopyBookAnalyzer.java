/*************************************************************
 * This file is part of CB2XML.
 * See the file "LICENSE" for copyright information and the
 * terms and conditions for copying, distribution and
 * modification of CB2XML.
 *************************************************************
 */

package net.sf.cb2xml.analysis;

import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import net.sf.cb2xml.def.Cb2xmlConstants;
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

/**
 * Main logic for translating the parse tree of SableCC into XML.
 * Currently the XML element and attribute names are hardcoded.
 *
 * All the inA* methods are fired when the corresponding node is "visited".
 * Each node name corresponds to each "production" etc. within the grammar file.
 *
 * The "tree walking" approach generates the XML DOM in a very inituitive manner.
 *
 * @author Peter Thomas / Bruce Martin
 *
 * @version 0.96 Bruce Martin (01.Sep.2017)
 * Complete rewrite to use Copybook and Item classes instead of 
 * Xml-DOM classes
 */

public class CopyBookAnalyzer extends DepthFirstAdapter {

	
	public CopyBookAnalyzer(String copyBookName, Parser parser, NumericDefinition numericDefinition, boolean loadComments) {
		this.copyBookName = copyBookName;
		this.parser = parser; 
		this.numDef = numericDefinition;
		this.loadComments = loadComments;
		this.postProcess = new PostProcess(numericDefinition, false);
	}

	private final NumericDefinition numDef;
	private final PostProcess postProcess;
	private final Parser parser;
	private final boolean loadComments;
	private String copyBookName;
	//private Document document;
	private Item prevItem, curItem;
	private Condition condition;
	
	private Copybook copybook;

	private BaseItem parent;
	private int lowestLevel = 1000;

	
	/**
	 * @return the copybook
	 */
	public final Copybook getCopybook() {
		return copybook;
	}

	// enter copybook, set up XML DOM and root element
	public void inARecordDescription(ARecordDescription node) {
	    copybook = new Copybook(copyBookName, numDef.getName());
	    parent = copybook;
	}

	// exit root element, save XML as file
	public void outARecordDescription(ARecordDescription node) {
		Positions lastPos = new Positions();
		for (Item itm : copybook.getChildItems()) {
			if (itm.levelNumber == 1) {
				postProcess.postProcessNode(itm, new Positions());
			} else {
				lastPos = postProcess.postProcessNode(itm, lastPos);
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
		if (loadComments) {
			List list = (List) parser.ignoredTokens.getIn(node);
			if (list != null) {
				Iterator i = list.iterator();
				while (i.hasNext()) {
					String s = i.next().toString().trim();
					if (s.length() > 0) {
	                    parent.addBefore(
	                    		curItem,
	                            correctForMinus(s));
					}
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
        if (start >= 0 || s.endsWith("-")){
            int i=start >= 0 ? start : s.length() - 1;
            StringBuilder buf = new StringBuilder(s);
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
            if (buf.charAt(i) == '-' ) {
            	buf.setCharAt(i, '=');
            }
            s = buf.toString();
        }

        return s;
    }


    // main elementary item

	// enter item, set up Item object
	public void inAItem(AItem node) {
		String levelStr = node.getNumberNot88().toString().trim();
		int level = Integer.parseInt(levelStr);
		PDataNameOrFiller dataName = node.getDataNameOrFiller();
		String name = "";
		if (dataName != null) {
			name = dataName.toString().trim();
		}
		if (level <= 77) {
			if (level <= lowestLevel || prevItem == null) {
				parent = copybook;
				lowestLevel = level;
			} else if (level > prevItem.levelNumber) {
				parent = prevItem;
				//prevItem.addItem((curItem = new Item(prevItem, level, name);
			} else if (level <= prevItem.levelNumber) {
				Item parentItem;
				parent = prevItem.parentItem;
				while (parent instanceof Item && (parentItem = ((Item) parent)).levelNumber >= level) {
					parent = parentItem.parentItem;
				}
			}
			curItem = new Item(parent, level, levelStr, name);
			prevItem = curItem;
		}
	}

	public void inARedefinesClause(ARedefinesClause node) {
		String dataName = node.getDataName().getText();
		//curItem.element.setAttribute(Attributes.ATTR_REDEFINES, getJavaName(dataName));
		curItem.redefines = dataName;
	}

	public void inAFixedOccursFixedOrVariable(AFixedOccursFixedOrVariable node) {
		curItem.occurs = Integer.parseInt(node.getNumber().toString().trim());
	}

	public void inAVariableOccursFixedOrVariable(
			AVariableOccursFixedOrVariable node) {
		curItem.occurs = Integer.parseInt(node.getNumber().toString().trim());
		curItem.dependingOn = node.getDataName().getText();
	}

	public void inAOccursTo(AOccursTo node) {
		curItem.occursMin = Integer.parseInt(node.getNumber().toString().trim());
	}

    public void inASynchronizedClauseClause(ASynchronizedClauseClause node)
    {
     	curItem.sync = true;
    }

	//============================= PICTURE CLAUSE ===================================

	public void inAPictureClause(APictureClause node) {
		postProcess.calculateLengths(curItem, removeChars(node.getCharacterString().toString()," "));
	}




	public void inASignClause(ASignClause node) {
		if (node.getSeparateCharacter() != null) {
			curItem.signClause = Cb2xmlConstants.SignClause.SIGN_SEPARATE;
		}
	}

	// Added the processing to capture the sign position JFG
    public void inALeadingLeadingOrTrailing(ALeadingLeadingOrTrailing node) {
    	if (curItem.signClause != null && curItem.signClause.signSeparate) {
    		curItem.signClause = Cb2xmlConstants.SignClause.SIGN_LEADING_SEPARATE;
    	} else {
    		curItem.signClause = Cb2xmlConstants.SignClause.SIGN_LEADING;
    	}
    }

    public void inATrailingLeadingOrTrailing(ATrailingLeadingOrTrailing node) {
    	if (curItem.signClause != null && curItem.signClause.signSeparate) {
    		curItem.signClause = Cb2xmlConstants.SignClause.SIGN_TRAILING_SEPARATE;
    	} else {
    		curItem.signClause = Cb2xmlConstants.SignClause.SIGN_TRAILING;
    	}
    }

	/**
	 * @see net.sf.cb2xml.sablecc.analysis.DepthFirstAdapter#inABlankWhenZeroClause(net.sf.cb2xml.sablecc.node.ABlankWhenZeroClause)
	 */
	public void inABlankWhenZeroClause(ABlankWhenZeroClause node) {
		curItem.blankWhenZero = true; 
		super.inABlankWhenZeroClause(node);
	}

	/**
	 * @see net.sf.cb2xml.sablecc.analysis.DepthFirstAdapter#inABlankWhenZeroClauseClause(net.sf.cb2xml.sablecc.node.ABlankWhenZeroClauseClause)
	 */
	public void inABlankWhenZeroClauseClause(ABlankWhenZeroClauseClause node) {
		curItem.blankWhenZero = true; 
		super.inABlankWhenZeroClauseClause(node);
	}


	//======================= USAGE CLAUSE ==========================


	public void inABinaryUsagePhrase(ABinaryUsagePhrase node) {
		curItem.usage = Cb2xmlConstants.Usage.BINARY;
		curItem.numericClass = Cb2xmlConstants.NumericClass.NUMERIC_IN_COBOL;
	}

	public void inACompUsagePhrase(ACompUsagePhrase node) {
		curItem.usage = Cb2xmlConstants.Usage.COMP;
		curItem.numericClass = Cb2xmlConstants.NumericClass.NUMERIC_IN_COBOL;
	}

	public void inAComp1UsagePhrase(AComp1UsagePhrase node) {
		curItem.usage = Cb2xmlConstants.Usage.COMP_1;
		curItem.numericClass = Cb2xmlConstants.NumericClass.NUMERIC_IN_COBOL;
	}

	public void inAComp2UsagePhrase(AComp2UsagePhrase node) {
		curItem.usage = Cb2xmlConstants.Usage.COMP_2;
		curItem.numericClass = Cb2xmlConstants.NumericClass.NUMERIC_IN_COBOL;
	}

	public void inAComp3UsagePhrase(AComp3UsagePhrase node) {
		curItem.usage = Cb2xmlConstants.Usage.COMP_3;
		curItem.numericClass = Cb2xmlConstants.NumericClass.NUMERIC_IN_COBOL;
	}

	public void inAComp4UsagePhrase(AComp4UsagePhrase node) {
		curItem.usage = Cb2xmlConstants.Usage.COMP_4;
		curItem.numericClass = Cb2xmlConstants.NumericClass.NUMERIC_IN_COBOL;
	}


	public void inAComp5UsagePhrase(AComp5UsagePhrase node) {
		curItem.usage = Cb2xmlConstants.Usage.COMP_5;
		curItem.numericClass = Cb2xmlConstants.NumericClass.NUMERIC_IN_COBOL;
	}


	public void inAComp6UsagePhrase(AComp6UsagePhrase node) {
		curItem.usage = Cb2xmlConstants.Usage.COMP_6;
		curItem.numericClass = Cb2xmlConstants.NumericClass.NUMERIC_IN_COBOL;
	}

	public void inADisplayUsagePhrase(ADisplayUsagePhrase node) {
		curItem.usage = Cb2xmlConstants.Usage.DISPLAY;
	}

	public void inADisplay1UsagePhrase(ADisplay1UsagePhrase node) {
		curItem.usage = Cb2xmlConstants.Usage.DISPLAY_1;
	}

	public void inAIndexUsagePhrase(AIndexUsagePhrase node) {
		curItem.usage = Cb2xmlConstants.Usage.INDEX;
	}

	public void inANationalUsagePhrase(ANationalUsagePhrase node) {
		curItem.usage = Cb2xmlConstants.Usage.NATIONAL;
	}

	public void inAObjectReferencePhrase(AObjectReferencePhrase node) {
		curItem.usage = Cb2xmlConstants.Usage.OBJECT_REFERENCE;
	}

	public void inAPackedDecimalUsagePhrase(APackedDecimalUsagePhrase node) {
		curItem.usage = Cb2xmlConstants.Usage.PACKED_DECIMAL;
	}

	public void inAPointerUsagePhrase(APointerUsagePhrase node) {
		curItem.usage = Cb2xmlConstants.Usage.POINTER;
	}

	public void inAProcedurePointerUsagePhrase(AProcedurePointerUsagePhrase node) {
		curItem.usage = Cb2xmlConstants.Usage.PROCEDURAL_POINTER;
	}

	public void inAFunctionPointerUsagePhrase(AFunctionPointerUsagePhrase node) {
		curItem.usage = Cb2xmlConstants.Usage.FUNCTION_POINTER;
	}
	
	
	/* (non-Javadoc)
	 * @see net.sf.cb2xml.sablecc.analysis.DepthFirstAdapter#inAJustifiedClause(net.sf.cb2xml.sablecc.node.AJustifiedClause)
	 */
	public void inAJustifiedClause(AJustifiedClause node) {
		curItem.justified = Cb2xmlConstants.Justified.RIGHT;
		if (node.getRight() == null) {
			curItem.justified = Cb2xmlConstants.Justified.JUSTIFIED;
		}
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
		curItem.value = node.getLiteral().toString().trim();
	}

	// 88 LEVEL CONDITION NODE
	public void inAValueItem(AValueItem node) {
		String name = node.getDataName().getText();
		condition = new Condition(name, "", "");
		prevItem.addCondition(condition);
		parent = prevItem;
	}

	public void outASingleLiteralSequence(ASingleLiteralSequence node) {
		if (node.getAll() != null) {
			condition.all = true;
		}
		condition.addCondition(new Condition("", "", node.getLiteral().toString().trim()));
	}

	public void outASequenceLiteralSequence(ASequenceLiteralSequence node) {
		condition.addCondition(new Condition("", "", node.getLiteral().toString().trim()));
	}

	public void outAThroughSingleLiteralSequence(AThroughSingleLiteralSequence node) {
		condition.addCondition(new Condition("", node.getTo().toString().trim(), node.getFrom().toString().trim()));
	}

	public void outAThroughSequenceLiteralSequence(AThroughSequenceLiteralSequence node) {
		condition.addCondition(new Condition("", node.getTo().toString().trim(), node.getFrom().toString().trim()));
	}



	//===============================================================================


	

	private String removeChars(String s, String charToRemove) {
		StringTokenizer st = new StringTokenizer(s, charToRemove, false);
		StringBuilder b = new StringBuilder();
		while (st.hasMoreElements()) {
			b.append(st.nextElement());
		}
		return b.toString();
	}
}