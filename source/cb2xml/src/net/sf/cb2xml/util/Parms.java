package net.sf.cb2xml.util;

import java.io.PrintStream;

public class Parms {

	public static final String FONT_PRM = "-font";
	public static final String COBOL_PRM = "-cobol";
	public static final String XML_PRM = "-xml";
	public static final String DEBUG_PRM = "-debug";
	public static final String INDENT_XML_PRM = "-indentxml";
	
	private static final int FONT_IDX = 0;
	private static final int COBOL_IDX = 1;
	private static final int XML_IDX = 2;
	private static final int DEBUG_IDX = 3;
	private static final int INDENT_IDX = 4;

	
	private static final String[] ARG_NAMES = {
			FONT_PRM, COBOL_PRM, XML_PRM, DEBUG_PRM, INDENT_XML_PRM,
	};
	
	public final String font, cobol, xml;
	public final boolean debug, indentXml;
	public boolean ok = true;
	
	
	public Parms(String[] args) {
		String[] v = {null, null, null, null, null, null};
		PrintStream p = System.err;
		
		if (args == null || args.length == 0 || args[0] == null) {
			
		} else if (args[0].toLowerCase().startsWith("-h")) {
			p = System.out;
		} else if (args[0].startsWith("-")) {
			v  = getAttrs(args);
		} else {
			v[COBOL_IDX] = args[0];
			if (args.length > 1) {
				v[DEBUG_IDX] = "t";
			}
		}
		 
		font = v[FONT_IDX];
		cobol = v[COBOL_IDX];
		xml = v[XML_IDX];
		debug = v[DEBUG_IDX] != null && (! v[DEBUG_IDX].toLowerCase().startsWith("f"));
		indentXml = v[INDENT_IDX] != null;
		ok = ok && cobol != null;
		
		if (ok) {
			if (xml == null && font != null) {
				System.err.println();
				System.err.println("  --------------- Warning  ---------  Warning -------------");
				System.err.println();
				System.err.println(" You have specified a -font parameter but no -Xml parameter");
				System.err.println(" This combination could result in corrupt Xml !!!");
				System.err.println();
				System.err.println("  ---------------------------------------------------------");
				System.err.println();
			}
		} else {
			printMsg(p);
		}  
	}
	
	private String[] getAttrs(String[] args) {
		String[] v = {null, null, null, null, null, null};
		String lcArg;
		String sep = "";
		StringBuilder b = new StringBuilder();
		int idx = ARG_NAMES.length;
		
		for (int i = 0; i < args.length; i++) {
			lcArg = args[i].toLowerCase();
			if (lcArg.startsWith("-")) {
				v[idx] = b.toString();
				b.setLength(0);
				idx = ARG_NAMES.length;
				for (int j = 0; j < ARG_NAMES.length; j++) {
					if (ARG_NAMES[j].equals(lcArg)) {
						idx = j;
						break;
					}
				}
				
				if (idx == ARG_NAMES.length) {
					System.out.println("Invalid Argument: " + args[i]);
					ok = false;
				}
				sep = "";
			} else {
				b.append(sep).append(args[i]);
				sep = " ";
			}
		}
		v[idx] = b.toString();
		
		
		return v;
	}
	
	private static void printMsg(PrintStream out) {
		out.println("Usage:\n\n  Either:"
				+ "\n\tcb2xml <copybookFileName> [debug]"
				+ "\n  or:"
				+ "\n\tcb2xml <attributes>\n "
				+ "\n  Where <attributes> are:\n"
				+ "\n\t-cobol - Cobol Copybook"
				+ "\n\t-xml - Output xml file"
				+ "\n\t-font - Font of Characterset of the copybook"
				+ "\n\t-debug - true:  run in debug mode"
				+ "\n\t-indentXml - indent (or format) the generated Xml "
		);
	}
}
