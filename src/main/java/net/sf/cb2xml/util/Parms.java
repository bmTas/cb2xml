package net.sf.cb2xml.util;

import java.io.PrintStream;

import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.DialectManager;
import net.sf.cb2xml.def.IBasicDialect;

public class Parms {

	public static final String FONT_PRM = "-font";
	public static final String COBOL_PRM = "-cobol";
	public static final String XML_PRM = "-xml";
	public static final String DEBUG_PRM = "-debug";
	public static final String INDENT_XML_PRM = "-indentxml";
	public static final String XML_FORMAT_PRM = "-xmlformat";
	public static final String STACK_SIZE_PRM = "-stacksize";
	public static final String DIALECT_PRM = "-dialect";
	
	private static final int FONT_IDX = 0;
	private static final int COBOL_IDX = 1;
	private static final int XML_IDX = 2;
	private static final int DEBUG_IDX = 3;
	private static final int INDENT_IDX = 4;
	private static final int XML_FORMAT_IDX = 5;
	private static final int STACK_SIZE_IDX = 6;
	private static final int DIALECT_IDX = 7;
	
	private static final String[] ARG_NAMES = {
			FONT_PRM, COBOL_PRM, XML_PRM, DEBUG_PRM, INDENT_XML_PRM, XML_FORMAT_PRM, STACK_SIZE_PRM, DIALECT_PRM,
	};
	private static final IBasicDialect[] ALL_DIALECTS = {
			DialectManager.MAINFRAME_COBOL, DialectManager.MAINFRAME_COBOL_64_BIT, 
			DialectManager.GNU_COBOL, DialectManager.FUJITSU_COBOL
	};
	public final String font, cobol, xml;
	public final boolean debug, indentXml;
	public final Cb2xmlConstants.Cb2xmlXmlFormat xmlFormat;
	public final long stackSize;
	public boolean ok = true;
	public final IBasicDialect dialect;
	
	
	public Parms(String[] args) {
		String[] v = new String[ARG_NAMES.length];
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
			v[XML_FORMAT_IDX] = "Classic";
		}
		 
		font = v[FONT_IDX];
		cobol = v[COBOL_IDX];
		xml = v[XML_IDX];
		debug = v[DEBUG_IDX] != null && (! v[DEBUG_IDX].toLowerCase().startsWith("f"));
		indentXml = v[INDENT_IDX] != null;
		xmlFormat = v[XML_FORMAT_IDX] == null        || "".equals(v[XML_FORMAT_IDX])
				 || "2017".equals(v[XML_FORMAT_IDX]) || "new".equalsIgnoreCase(v[XML_FORMAT_IDX])
						? Cb2xmlConstants.Cb2xmlXmlFormat.FORMAT_2017
						: Cb2xmlConstants.Cb2xmlXmlFormat.CLASSIC;
		
		int ss = -1;
		if (v[STACK_SIZE_IDX] == null) {
			
		} else if ("normal".equalsIgnoreCase(v[STACK_SIZE_IDX])) {
			ss = 0;
		} else {
			try {
				ss = Integer.parseInt(v[STACK_SIZE_IDX]) * 1024 * 1024;
			} catch (Exception e) {
				ok = false;
				System.err.println();
				System.err.println("Invalid Stacksize: " + v[STACK_SIZE_IDX] + " it should be normal or the number of megabytes");
				System.err.println();
			}
		}
		stackSize = ss;
		
		IBasicDialect d = DialectManager.MAINFRAME_COBOL;
		String dialectStr = v[DIALECT_IDX];
		boolean looking = true;
		for (IBasicDialect dialect : ALL_DIALECTS) {
			if (dialect.getNumericDefinition().getName().equalsIgnoreCase(dialectStr)) {
				d = dialect;
				looking = false;
			}
		}
		if (looking) {
			if (dialectStr == null || dialectStr.length() == 0 || dialectStr.startsWith("m") || dialectStr.startsWith("M")) {
				
			} else if (dialectStr.toLowerCase().startsWith("gnu_")) {
				d = DialectManager.GNU_COBOL;
			} else if (dialectStr.toLowerCase().startsWith("f")) {
				d = DialectManager.FUJITSU_COBOL;
			} else {
				ok = false;
				System.err.println();
				System.err.println("Invalid Cobol Dialect: " + dialectStr + "it should be Mainframe, Gnu_Cobol or Fujitsu");
				System.err.println();
			}
		}
		dialect = d;
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
		String[] v = new String[ARG_NAMES.length + 1];
		String lcArg;
		String sep = "";
		StringBuilder b = new StringBuilder();
		int idx = ARG_NAMES.length;
		
		
		for (int i = 0; i < args.length; i++) {
			lcArg = args[i] == null ? "" : args[i].toLowerCase();
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
	
	private static void printMsg(PrintStream output) {
		output.println();
		output.print(
				  "Usage:\n\n  Either:"
				+ "\n\tcb2xml <copybookFileName> [debug]"
				+ "\n  or:"
				+ "\n\tcb2xml <attributes>\n "
				+ "\n  Where <attributes> are:\n"
				+ "\n\t-cobol - Cobol Copybook"
				+ "\n\t-xml   - Output xml file"
				+ "\n\t-font  - Font (encoding) of the copybook"
				+ "\n\t-debug - true:  run in debug mode"
				+ "\n\t-indentXml - indent (or format) the generated Xml "
				+ "\n\t-XmlFormat - for the new Xml format: old or 2017 (for the new 2017 format) default"
				+ "\n\t-StackSize - Stack Size (in megabytes (or normal to use the default) "
				+ "\n\t-Dialect   - Cobol Dialect: "
		);
		for (IBasicDialect dialect : ALL_DIALECTS) {
			output.print( dialect.getNumericDefinition().getName() + "\t");
		}
		output.println();
		output.println();
	}
}
