package tests;

import static org.junit.Assert.*;
import net.sf.cb2xml.util.Parms;

import org.junit.Test;

/**
 * Test the Parm class
 * 
 * @author Bruce Martin
 *
 */
public class TstParms {

	private static final String[] ARG_NAMES = {
		Parms.FONT_PRM, Parms.COBOL_PRM, Parms.XML_PRM, Parms.DEBUG_PRM, Parms.INDENT_XML_PRM,
	};
	
	@Test
	public void test1() {
		tst(true, new String[] {"a","b","c","true","xx"});
		tst(true, new String[] {"a","b","c","false", null});

		tst(true, new String[] {null,"b","c","true","xx"});
		tst(false,new String[] {"a", null,"c","true","xx"});
		tst(true, new String[] {"a","b", null ,"true","xx"});
		tst(true, new String[] {"a","b","c", null,"xx"});
		tst(true, new String[] {"a","b","c","true", null});

	}

	private void tst(boolean ok, String[] strings  ) {
		int c = 0;
		for (String s : strings) {
			if (s != null) {
				c += 2;
			}
		}
		String[] args = new  String[c];
		if (strings[4] != null) {
			args = new  String[c - 1];
			args[c-2] = ARG_NAMES[ARG_NAMES.length - 1];
		}
		int j = 0;
		for (int i = 0; i < ARG_NAMES.length - 1; i++) {
			if (strings[i] != null) {
				args[j++] = ARG_NAMES[i];
				args[j++] = strings[i];
			}
		}
		Parms p = new Parms(args);
		
		assertEquals(strings[0], p.font);
		assertEquals(strings[1], p.cobol);
		assertEquals(strings[2], p.xml);
		assertEquals("true".equals(strings[3]) , p.debug);
		assertEquals(strings[4] != null, p.indentXml);
		assertEquals(ok, p.ok);
	}
}
