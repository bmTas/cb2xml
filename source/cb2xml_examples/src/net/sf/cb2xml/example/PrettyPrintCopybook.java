package net.sf.cb2xml.example;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBException;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import org.w3c.dom.Document;

import net.sf.cb2xml.Cb2Xml2;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;
import net.sf.cb2xml.util.XmlUtils;


/**
 * This program will "Pretty Print" a Cobol copybook;
 * 
 * @author Bruce Martin
 *
 */
public class PrettyPrintCopybook {

	public static final int PIC_POS = 46;

	private int commentColumn = 6; 

	private final static String SPACES = "                                                                                                            "
			      + "                                                                                                            ";

	HashMap<String, String> usageMap = new HashMap<String, String>(150);
	
	public PrettyPrintCopybook(String input) 
			throws IOException, XMLStreamException, ParserException, LexerException {
		this(new File(input), new OutputStreamWriter(System.out));
	}

	public PrettyPrintCopybook(String input, String ouput) 
			throws IOException, XMLStreamException, ParserException, LexerException {
		this(new File(input), new FileWriter(ouput)); 
	}
	
	public PrettyPrintCopybook(File in , Writer w) 
			throws IOException, XMLStreamException, ParserException, LexerException {
		this(new FileReader(in), in.getName(), w);
	}
	
	/**
	 * "Obfuscate" a Cobol copybook; 
	 * @param copybookReader Cobopl Copybook
	 * @param copybookname name of the copybook
	 * @param copybookWriter output for "Obfuscated" Copybook
	 * 
	 * @throws IOException
	 * @throws JAXBException
	 * @throws ParserException
	 * @throws LexerException
	 * @throws XMLStreamException 
	 */
	public PrettyPrintCopybook(Reader copybookReader, String copybookname, Writer copybookWriter) 
			throws IOException, XMLStreamException, ParserException, LexerException {

		
		usageMap.put("computational", "comp");
		usageMap.put("computational-1", "comp-1");
		usageMap.put("computational-2", "comp-2");
		usageMap.put("computational-3", "comp-3");
		usageMap.put("computational-4", "comp-4");
		usageMap.put("computational-5", "comp-5");
		usageMap.put("computational-6", "comp-6");
		

		BufferedWriter writer = null;
		try {
			writer = new BufferedWriter(copybookWriter);
			printCopybook(copybookReader, copybookname, new BufferedWriter(copybookWriter));
		} finally {
			if (writer != null) {
				writer.close();
			}
		}
		
		copybookWriter.close();
	}
	
	public void printCopybook(Reader xmlReader, String copybookName, BufferedWriter cobolWriter) 
	throws IOException,  XMLStreamException, ParserException, LexerException{

		String name;
		
		int type = -1;
		LevelDetails lvlDetails = new LevelDetails();
		Document cblDoc = Cb2Xml2.convert(xmlReader, copybookName, false, Cb2xmlConstants.USE_LONG_LINE);
	
		String xmlStr = XmlUtils.domToString(cblDoc).toString();
		
		XMLStreamReader parser = XMLInputFactory.newInstance()
								  				.createXMLStreamReader(
								  						new StringReader(xmlStr));
		int conditionLvl = 0;
		ConditionDetails condition = null;
		

		
		while (parser.hasNext()) {
			type = parser.next();
			switch (type) {
            case XMLStreamConstants.START_ELEMENT:
            	HashMap<String, String> attrMap = new HashMap<String, String>(40);
            	name = parser.getName().toString();

            	for (int i = 0; i < parser.getAttributeCount(); i++) {
            		attrMap.put(parser.getAttributeName(i).toString().toLowerCase(), parser.getAttributeValue(i));
            	}
            	
            	if ("item".equalsIgnoreCase(name)) {
            		String levelNo = attrMap.get(Cb2xmlConstants.LEVEL);
            		if ("01".equals(levelNo) || "1".equals(levelNo)) {
            			lvlDetails.topLevel01 = true;
            		}
             		printItem(cobolWriter, attrMap, lvlDetails);
              		lvlDetails.level +=1;
            	} else if ("condition".equalsIgnoreCase(name)) {
            		if (conditionLvl == 0) {
            			condition = new ConditionDetails(attrMap.get(Cb2xmlConstants.NAME));
                		lvlDetails.level +=1;
            		} else {
            			condition.conditions.add(
            					new Condition(
            							attrMap.get(Cb2xmlConstants.VALUE),
            							attrMap.get(Cb2xmlConstants.THROUGH)));
            		}
        			conditionLvl += 1;
            	}
            	break;
            case XMLStreamConstants.END_ELEMENT:
            	String name2 = parser.getName().toString();
            	if ("item".equalsIgnoreCase(name2)) {
            		lvlDetails.level -=1;
            	} else if ("condition".equalsIgnoreCase(name2)) {
            		conditionLvl -= 1;
            		if (conditionLvl == 0) {
            			print88s(cobolWriter, condition, lvlDetails);
            			condition = null;
                		lvlDetails.level -=1;
            		}
            	}
             	break;
            case XMLStreamConstants.CHARACTERS:
            	//String text = parser.getText();
            	break;
            case XMLStreamConstants.COMMENT:
            	if (condition != null) {
           			print88s(cobolWriter, condition, lvlDetails);
            	}
            	cobolWriter.write(SPACES.substring(0, commentColumn) + parser.getText());
            	cobolWriter.newLine();
                break;
        
//            case (XMLStreamConstants.START_DOCUMENT) :
//            break;
//            case (XMLStreamConstants.COMMENT) :
//            break;
//            case (XMLStreamConstants.DTD) :
//            	break;
//            case (XMLStreamConstants.ENTITY_REFERENCE) :
//            	break;
//            case (XMLStreamConstants.CDATA) :
//              break;
//            case (XMLStreamConstants.END_DOCUMENT): 
//            	break;
          	default:
			}
		}

		parser.close();
		xmlReader.close();
		cobolWriter.close();
	}

	
	/**
	 * Print a list of items (or Cobol Groups / fields)
	 * 
	 * @param w writer to write the output to
	 * @param items List of Cobol Items (Group / Field definiotions) 
	 * @param indent the amount to indent the fields
	 * 
	 * @throws IOException
	 */
	private void printItem(BufferedWriter w, Map<String, String> attr, LevelDetails lvlDtls) throws IOException {

		String name;
		String usage="";
		String sign="", sync="";
		StringBuilder occurs = new StringBuilder();
		String redefines = "";
		String dependingOnStr = "";
		
		String ussageAttr = attr.get(Cb2xmlConstants.USAGE);
		if (ussageAttr != null && (! isTrue(attr.get(Cb2xmlConstants.INHERITED_USAGE)))) {
			usage = ussageAttr;
			
			if (usageMap.containsKey(usage)) {
				usage = usageMap.get(usage);
			}
		}
		if (isTrue(attr.get(Cb2xmlConstants.SYNC))) {
			sync = "Sync";
		}
		
		String signPos = attr.get(Cb2xmlConstants.SIGN_POSITION);
		
		boolean isSignSeparate = isTrue(attr.get(Cb2xmlConstants.SIGN_SEPARATE));
		if (isPresent(signPos) ) {
			sign = "Sign " + signPos;
			if (isSignSeparate) {
				sign = sign + " Separate";
			}
		} else if (isSignSeparate) {
			sign = "Separate";
		}
		
		if (isTrue(attr.get(Cb2xmlConstants.TRAILING))) {
			if (sign.length() == 0) {
				sign = "Trailing";
			} else {
				sign = sign + " Trailing";
			}
		} else if (isTrue(attr.get(Cb2xmlConstants.LEADING))) {
			if (sign.length() == 0) {
				sign = "Leading";
			} else {
				sign = sign + " Leading";
			}
		}
		
		String signPosition = attr.get(Cb2xmlConstants.SIGN_POSITION);
		if (isPresent(signPosition)) {
			signPos = signPosition;
		}
		
		String redef = attr.get(Cb2xmlConstants.REDEFINES);
		if (isPresent(redef)) {
			redefines = "Redefines " + redef;
		}
		
		String occursAttr = attr.get(Cb2xmlConstants.OCCURS);
		if (occursAttr != null) {
			occurs.append(" Occurs ");
			String minOccursAttr = attr.get(Cb2xmlConstants.OCCURS_MIN);
			if (minOccursAttr != null) {
				occurs.append(minOccursAttr).append(" to "); 
			}
			
			occurs.append(occursAttr).append(" times");
			
			String dependingOn = attr.get(Cb2xmlConstants.DEPENDING_ON);
			if (dependingOn != null && dependingOn.length() > 0) {
				dependingOnStr = "Depending on " + dependingOn;
			}
		}

		name = attr.get(Cb2xmlConstants.NAME);
		
		String pic = attr.get(Cb2xmlConstants.PICTURE);
		String levelNo = attr.get(Cb2xmlConstants.LEVEL);
		String value = attr.get(Cb2xmlConstants.VALUE);
		if (isPresent(pic)) {
			String str = levelNo + " " + name;
			printLine(w, lvlDtls.indent(), value, new String[] {pic, sign, usage, sync}, str, redefines, occurs.toString(), dependingOnStr);
		} else {
			printLine(w, lvlDtls.indent(), value, null, levelNo + " " + name, redefines, usage, sync, occurs.toString());
		}
	}
	
	private boolean isTrue(String s) {
		return "true".equalsIgnoreCase(s);
	}

	private boolean isPresent(String s) {
		return s != null && s.length() > 0;
	}
	/**
	 * Print the 88's for a field
	 * 
	 * @param w output writer
	 * @param item item (field) that is to have its 88's printed
	 * @param indent the amount to indent the program
	 * 
	 * @throws IOException
	 */
	private void print88s(BufferedWriter w, ConditionDetails cond, LevelDetails lvlDtls) throws IOException {
		ArrayList<String> items = new ArrayList<String>();
		
		if (cond.toPrintHeader) {
			items.add("88");
			items.add(cond.name);
			items.add("Value");
		}
		if (cond.conditions.size() > 0 || cond.toPrintHeader) {
			for (Condition xx : cond.conditions) {
				if (xx == null) {
							
				} else 	if (isPresent(xx.thru)) {
					items.add(xx.value + " Thru " + xx.thru);	
				} else {
					items.add(xx.value);	
				} 
			}
			printLine(w, lvlDtls.indent(-1), null, null, items.toArray(new String[items.size()]));	
			cond.toPrintHeader = false;
		} 
	}
		
	/**
	 * Print one Cobol Copybook field
	 * 
	 * @param w writer
	 * @param indent ampount to indent the code
	 * @param pics picture details
	 * @param fieldDetails Field details
	 * @throws IOException
	 */
	private void printLine(BufferedWriter w, String indent, String value, String[] pics,  String...fieldDetails ) throws IOException {
		StringBuilder b  = new StringBuilder(indent);
		
		for (int i = 0; i < fieldDetails.length; i++) {
		    String s = fieldDetails[i];
			if (isPresent(s)) {
				if (b.length() + s.length() > 70/*
				|| ("value".equalsIgnoreCase(s)  && b.length() + len(fieldDetails, i) > 70 )*/) {
					printCurrentLine(w, indent.length(), b);
					b.append("   ");
				} 
				if (! s.startsWith(" ")) {
					b.append(' ');
				}
				b.append(s);
			}
		}
		
		if (b.length() > PIC_POS - 1 && pics != null) {
			printCurrentLine(w, 0, b);
		}
		
		if (pics != null && pics.length > 0) {
			b.append(SPACES.substring(0, PIC_POS - b.length()));
			if (pics[0] != null) {
				b.append("Pic");
			}
			for (String s : pics) {
				if (s != null && s.length() > 0) {
					if (b.length() + s.length() > 70) {
						printCurrentLine(w, 0, b);
						
						b.append(SPACES.substring(1, Math.min(PIC_POS + 4, Math.max(20, 70 - s.length()))));
					}
					b.append(' ').append(s);
				}
			}
		}
		
		int i = b.length() - 1;
		while (i >= 20 && b.charAt(i) == ' ') {
			b.setLength(i);
			i -= 1;
		}
		
		if (value != null) {
			int valueLength = "value".length() + value.length();
			if (b.charAt(b.length()-1) == ' ') {
				b.setLength(b.length() - 1);
			}
			if (b.length() + valueLength < 68) {
				b.append(" value ");
			} else if (indent.length() + valueLength < 66) {
				printCurrentLine(w, Math.min(22, indent.length()), b);
				b.append("    value");
			} else if (b.length() + "value".length() < 70) {
				b.append(" value");
				printCurrentLine(w, Math.min(22, indent.length()), b);
			}
			appendValue(b, value);
		}

		b.append('.');
		w.write(b.toString());
		w.newLine();
	}
	
	private void appendValue(StringBuilder b, String value) {
		b.append(' ').append(value);
		
	}
	


	/**
	 * @param w
	 * @param indent
	 * @param b
	 * @throws IOException
	 */
	private void printCurrentLine(BufferedWriter w, int indentAmount,
			StringBuilder b) throws IOException {
		w.write(b.toString());
		w.newLine();
		b.setLength(indentAmount);
		for (int i = 0; i < indentAmount; i++) {
			b.setCharAt(i, ' ');
		}
	}
	
	private class LevelDetails {
		int level = 0;
		boolean topLevel01=false;
		
		String indent() {
			return indent(0);
		}
		String indent(int adj) {
			return SPACES.substring(0, indentCount(adj));
		}

		int indentCount(int adj) {
			int indentBy;
			int start = commentColumn + 5;
			int slowLevel = 5;
			if (topLevel01) {
				start = commentColumn + 3;
				slowLevel = 6;
			}
			int lvl = level + adj;
			if (lvl < slowLevel) {
				indentBy = start + lvl * 3;
			} else {
				indentBy = start + slowLevel * 3 + lvl;
			}
			return indentBy;
		}
	}
	
	private static class ConditionDetails {
		final String name;
		final ArrayList<Condition> conditions = new ArrayList<Condition>();
		boolean toPrintHeader = true;
		

		protected ConditionDetails(String name) {
			super();
			this.name = name;
		}
	}
	private static class Condition {
		final String value, thru;

		protected Condition(String value, String thru) {
			super();
			this.value = value;
			this.thru = thru;
		}
	}
	public static void main(String[] args) throws IOException, XMLStreamException, ParserException, LexerException  {
		if (args == null || args.length == 0) {
			//System.err.println("You must supply atleast the Cobol copybook name");
			new PrettyPrintCopybook(Code.getFullName("BitOfEverything.cbl").getFile());
		} else if (args.length == 1) {
			new PrettyPrintCopybook(args[0]);
		} else {
			new PrettyPrintCopybook(args[0], args[1]);
		}

	}

}
