package net.sf.cb2xml.example;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.HashMap;
import java.util.List;

import javax.xml.bind.JAXBException;

import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.jaxb.Condition;
import net.sf.cb2xml.jaxb.Copybook;
import net.sf.cb2xml.jaxb.Item;
import net.sf.cb2xml.parse.CobolParser;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;


/**
 * This program will "Obfuscate" a Cobol copybook; this is useful if you 
 * do not want to reveal any details about the company you work for.
 * 
 * @author Bruce Martin
 *
 */
public class ObscureCopybook {

	public static final int PIC_POS = 46;
	
	int count = 10001;
	String spaces = "                                                                                                            "
			      + "                                                                                                            ";

	HashMap<String, String> fieldMap = new HashMap<String, String>(150);
	HashMap<String, String> usageMap = new HashMap<String, String>(150);
	
	public ObscureCopybook(String input) throws IOException, JAXBException, ParserException, LexerException {
		this(new File(input), new OutputStreamWriter(System.out));
	}

	public ObscureCopybook(String input, String ouput) throws IOException, JAXBException, ParserException, LexerException {
		this(new File(input), new FileWriter(ouput)); 
	}
	
	public ObscureCopybook(File in , Writer w) throws IOException, JAXBException, ParserException, LexerException {
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
	 */
	public ObscureCopybook(Reader copybookReader, String copybookname, Writer copybookWriter) throws IOException, JAXBException, ParserException, LexerException {
		Copybook copybook = CobolParser.newParser() 
								.parseCobol(copybookReader, copybookname, Cb2xmlConstants.USE_LONG_LINE);
		List<Item> items = copybook.getItem();

		copybookReader.close();
		
		usageMap.put("computational", "comp");
		usageMap.put("computational-1", "comp-1");
		usageMap.put("computational-2", "comp-2");
		usageMap.put("computational-3", "comp-3");
		usageMap.put("computational-4", "comp-4");
		usageMap.put("computational-5", "comp-5");
		usageMap.put("computational-6", "comp-6");
		
		if (items == null || items.size() == 0) { 
			System.err.println("Copybook is empty");
		} else {
			BufferedWriter writer = null;
			try {
				writer = new BufferedWriter(copybookWriter);
				String indent = spaces.substring(0, 11);
				String level = items.get(0).getLevel();
				
				if ("01".equals(level) || "1".equals(level)) {
					indent = spaces.substring(0, 8);
				}
				
				printItem(writer, items, indent);
			} finally {
				if (writer != null) {
					writer.close();
				}
			}
		}
		
		copybookWriter.close();
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
	private void printItem(BufferedWriter w, List<Item> items, String indent) throws IOException {
		if (items != null && items.size() > 0) {
			for (Item item : items) {
				String name,  str;
				String usage="";
				String sign="", sync="";
				String ext = Integer.toString(count++).substring(1, 5);
				StringBuilder occurs = new StringBuilder();
				String redefines = "";
				String dependingOnStr = "";
				
				if (item.getUsage() != null && item.isInheritedUsage() != Boolean.TRUE) {
					usage = item.getUsage();
					
					if (usageMap.containsKey(usage)) {
						usage = usageMap.get(usage);
					}
				}
				if (Boolean.TRUE.equals(item.isSync())) {
					sync = "Sync";
				}
				if (isPresent(item.getSignPosition()) ) {
					sign = "Sign " + item.getSignPosition();
					if (Boolean.TRUE.equals(item.isSignSeparate())) {
						sign = sign + " Separate";
					}
				} else if (Boolean.TRUE.equals(item.isSignSeparate())) {
					sign = "Separate";
				}
				if (isPresent(item.getRedefines())) {
					redefines = "Redefines " + item.getRedefines();
				}
				if (item.getOccurs() != null) {
					occurs.append(" Occurs ");
					if (item.getOccursMin() != null) {
						occurs.append(item.getOccursMin()).append(" to "); 
					}
					
					occurs.append(item.getOccurs()).append(" times");
					
					String dependingOn = item.getDependingOn();
					if (dependingOn != null && item.getDependingOn().length() > 0) {
						String key = dependingOn.toLowerCase();
						if (fieldMap.containsKey(key)) {
							dependingOn = fieldMap.get(key);
						}
						
						dependingOnStr = "Depending on " + dependingOn;
					}
				} 
				if (item.getItem().size() == 0) {
					name = "Field-" + ext;
					str = item.getLevel() + " " + name;
					printLine(w, indent, item.getValue(), new String[] {item.getPicture(), sign, usage, sync}, str, redefines, occurs.toString(), dependingOnStr);
					print88s(w, item, indent);
				} else {
					name = "Group-" + ext;
					printLine(w, indent, item.getValue(), null, item.getLevel() + " " + name, redefines, usage, sync, occurs.toString());
					print88s(w, item, indent);
					
					printItem(w, item.getItem(), indent + "   ");
				}
				
				if (item.getName() != null && item.getName().length() > 0 
				&& ! "filler".equalsIgnoreCase(item.getName())) {
					fieldMap.put(item.getName().toLowerCase(), name);
				}
			}
		}
		
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
	private void print88s(BufferedWriter w, Item item, String indent) throws IOException {
		List<Condition> conditions = item.getCondition();
		
		if (conditions != null && conditions.size() > 0) {
			for (Condition c : conditions) {
				List<Condition> condition = c.getCondition();
				if (condition != null && condition.size() > 0) {
					String[] l = new String[condition.size() + 3];
					int i = 3;
					l[0] = "    88";
					l[1] = c.getName();
					l[2] = "Value";
					for (Condition xx : condition) {
						if (xx == null) {
							l[i++] = null;
						} else 	if (xx.getThrough() == null) {			
							l[i++] = fix(item.isNumeric(), xx.getValue());
						} else {
							l[i++] = fix(item.isNumeric(), xx.getValue()) + " Thru " + fix(item.isNumeric(), xx.getThrough());
						}
					}
					printLine(w, indent, null, null, l);					
				} else if (c.getThrough() != null && c.getThrough().length() > 0) {
					printLine(w, indent, null, null, "   88 " + fix(item.isNumeric(), c.getName()), 
							"Value", c.getValue(), "Thru", fix(item.isNumeric(), c.getThrough()));
				} else {
					printLine(w, indent, null, null, "   88 " + c.getName(), "Value", fix(item.isNumeric(), c.getValue()));
				}
			}
		}
	}
	
	/**
	 * Add Quotes to a text value (if necessay)
	 * 
	 * @param isNumeric Wether it is a numeric field
	 * @param s value to be updated
	 * 
	 * @return
	 */
	private String fix(Boolean isNumeric, String s) {
//		if (s != null && isNumeric != Boolean.TRUE) {
//			s = "'" + s.replace("'", "''") + "'";
//		}
		return s;
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
			b.append(spaces.substring(0, PIC_POS - b.length()));
			if (pics[0] != null) {
				b.append("Pic");
			}
			for (String s : pics) {
				if (s != null && s.length() > 0) {
					if (b.length() + s.length() > 70) {
						printCurrentLine(w, 0, b);
						
						b.append(spaces.substring(1, Math.min(PIC_POS + 4, Math.max(20, 70 - s.length()))));
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
	
	public static void main(String[] args) throws IOException, JAXBException, ParserException, LexerException {
		if (args == null || args.length == 0) {
			//System.err.println("You must supply atleast the Cobol copybook name");
			new ObscureCopybook(Code.getFullName("BitOfEverything.cbl").getFile());
		} else if (args.length == 1) {
			new ObscureCopybook(args[0]);
		} else {
			new ObscureCopybook(args[0], args[1]);
		}

	}

}
