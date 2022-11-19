package tests;

import static org.junit.Assert.*;

import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.DialectManager;
import net.sf.cb2xml.def.IBasicDialect;
import net.sf.cb2xml.util.Parms;

import org.junit.Test;

/**
 * Test the Parm class
 * 
 * @author Bruce Martin
 *
 */
public class TstParms {

	private static final String MAINFRAME_64_BIT = "Mainframe_64_Bit";
	private static final String MAINFRAME = "mainframe";
	private static final String GNU_COBOL = "gnu_cobol";
	private static final String FUJITSU = "fujitsu";
	
	private static final String[] ARG_NAMES = {
		Parms.FONT_PRM, Parms.COBOL_PRM, Parms.XML_PRM, Parms.DEBUG_PRM, Parms.INDENT_XML_PRM,
	};

	private static final String[] ARG_NAMES_1 = {
		Parms.FONT_PRM, Parms.COBOL_PRM, Parms.XML_PRM, Parms.DEBUG_PRM, Parms.INDENT_XML_PRM,
		Parms.STACK_SIZE_PRM, Parms.XML_FORMAT_PRM, Parms.DIALECT_PRM,
	};

	@Test
	public void test1() {
		tst1(true, new String[] {"a","b","c","true","xx"});
		tst1(true, new String[] {"a","b","c","false", null});

		tst1(true, new String[] {null,"b","c","true","xx"});
		tst1(false,new String[] {"a", null,"c","true","xx"});
		tst1(true, new String[] {"a","b", null ,"true","xx"});
		tst1(true, new String[] {"a","b","c", null,"xx"});
		tst1(true, new String[] {"a","b","c","true", null});
	}
	
	@Test
	public void test2() {
		tst2(true, new String[] {"a","b","c","true","xx", null, "2017", MAINFRAME});
		tst2(true, new String[] {"a","b","c","true","xx", "5", "Classic", GNU_COBOL});
		tst2(true, new String[] {"a","b","c","true","xx", "5", null, GNU_COBOL});
		tst2(true, new String[] {"a","b","c","true","xx", "5", "new", FUJITSU});
		tst2(true, new String[] {"a","b","c","true","xx", "5", "new", MAINFRAME});
		tst2(true, new String[] {"a","b","c","true","xx", "5", "new", MAINFRAME_64_BIT});
		tst2(true, new String[] {"a","b","c","true","xx", "5", "2017", null});
		tst2(false, new String[] {"a","b","c","true","xx", "xx", "2017", null});
		tst2(false, new String[] {"a","b","c","true","xx", "5", "2017", "xzx"});
		
	}


	private void tst1(boolean ok, String[] strings  ) {
		Parms p = loadParms(ARG_NAMES, strings);
		
		assertEquals(strings[0], p.font);
		assertEquals(strings[1], p.cobol);
		assertEquals(strings[2], p.xml);
		assertEquals("true".equals(strings[3]) , p.debug);
		assertEquals(strings[4] != null, p.indentXml);
		assertEquals(ok, p.ok);
	}


	private void tst2(boolean ok, String[] strings  ) {
		Parms p = loadParms(ARG_NAMES_1, strings);
		
		assertEquals(strings[0], p.font);
		assertEquals(strings[1], p.cobol);
		assertEquals(strings[2], p.xml);
		assertEquals("true".equals(strings[3]) , p.debug);
		assertEquals(strings[4] != null, p.indentXml);
		if (strings[5] == null) {
			assertEquals("-1", Long.toString(p.stackSize));
		} else {
			try {
				assertEquals(Long.parseLong(strings[5]) * 1024 * 1024, p.stackSize);
			} catch (NumberFormatException e) {
			}
		}
		
		Cb2xmlConstants.Cb2xmlXmlFormat eXmlFormat = Cb2xmlConstants.Cb2xmlXmlFormat.CLASSIC;
		if (strings[6] == null || "2017".equals(strings[6]) || "new".equalsIgnoreCase(strings[6])) {
			eXmlFormat = Cb2xmlConstants.Cb2xmlXmlFormat.FORMAT_2017;
		}
		assertEquals(eXmlFormat, p.xmlFormat);
		IBasicDialect dialect = DialectManager.MAINFRAME_COBOL;
		
		if (strings[7] == null || strings[7].length() == 0 || MAINFRAME.equals(strings[7])) {
		} else if (MAINFRAME_64_BIT.equals(strings[7])) {
			dialect = DialectManager.MAINFRAME_COBOL_64_BIT;			
		} else if (FUJITSU.equals(strings[7])) {
			dialect = DialectManager.FUJITSU_COBOL;
		} else if (GNU_COBOL.equals(strings[7])) {
			dialect = DialectManager.GNU_COBOL;
		} 
		assertEquals(dialect, p.dialect);
		
		assertEquals(ok, p.ok);
	}


	private Parms loadParms(String[] ARG_NAMES, String[] strings) {
		int c = 0;
		for (String s : strings) {
			if (s != null) {
				c += 2;
			}
		}
		String[] args = new  String[c];
//		if (strings[ARG_NAMES.length - 1] != null) {
//			args = new  String[c - 1];
//			args[c-2] = ARG_NAMES[ARG_NAMES.length - 1];
//		}
		int j = 0;
		for (int i = 0; i < ARG_NAMES.length; i++) {
			if (strings[i] != null) {
				args[j++] = ARG_NAMES[i];
				args[j++] = strings[i];
			}
		}
		Parms p = new Parms(args);
		return p;
	}
}
