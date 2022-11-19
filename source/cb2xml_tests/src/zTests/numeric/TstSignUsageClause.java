package zTests.numeric;

import static org.junit.Assert.*;

import java.io.IOException;
import java.io.StringReader;
import java.util.List;

import javax.xml.stream.XMLStreamException;

import org.junit.Test;

import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.ICb2XmlBuilder;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.IItem;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;


/**
 * Test Sign and usage clauses + basic numeric tests
 * 
 * @author Bruce Martin
 *
 */
public class TstSignUsageClause {

	
	static String expectedJustifiedClasicXml 
			= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<copybook filename=\"Record\" dialect=\"Mainframe\">\n"
			+ "    <item level=\"01\" name=\"Record\" position=\"1\" storage-length=\"30\" display-length=\"30\">\n"
			+ "        <item level=\"03\" name=\"pic-x\" picture=\"x(4)\" position=\"1\" storage-length=\"4\" display-length=\"4\"/>\n"
			+ "        <item level=\"03\" name=\"unsigned-1\" picture=\"9(4)\" position=\"5\" storage-length=\"4\" display-length=\"4\" numeric=\"true\"/>\n"
			+ "        <item level=\"03\" name=\"signed-1\" picture=\"s9(4)\" position=\"9\" storage-length=\"4\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
			+ "        <item level=\"03\" name=\"signed-leading\" picture=\"s9(4)\" position=\"13\" storage-length=\"4\" display-length=\"4\" numeric=\"true\" sign-position=\"leading\" signed=\"true\"/>\n"
			+ "        <item level=\"03\" name=\"signed-trailing\" picture=\"s9(4)\" position=\"17\" storage-length=\"4\" display-length=\"4\" numeric=\"true\" sign-position=\"trailing\" signed=\"true\"/>\n"
			+ "        <item level=\"03\" name=\"signed-sep-leading\" picture=\"s9(4)\" position=\"21\" storage-length=\"5\" display-length=\"5\" numeric=\"true\" sign-position=\"leading\" sign-separate=\"true\" signed=\"true\"/>\n"
			+ "        <item level=\"03\" name=\"signed-sep-trailing\" picture=\"s9(4)\" position=\"26\" storage-length=\"5\" display-length=\"5\" numeric=\"true\" sign-position=\"trailing\" sign-separate=\"true\" signed=\"true\"/>\n"
			+ "    </item>\n"
			+ "</copybook>";
	static String expectedSignClasicXml 	
			= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
					+ "<copybook filename=\"Record\" dialect=\"Mainframe\">\n"
					+ "    <item level=\"01\" name=\"Record\" position=\"1\" storage-length=\"58\" display-length=\"104\">\n"
					+ "        <item level=\"05\" name=\"grp-1\" position=\"1\" storage-length=\"16\" display-length=\"32\">\n"
					+ "            <item level=\"10\" name=\"pic-9\" picture=\"s9(4)\" position=\"1\" storage-length=\"4\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
					+ "            <item level=\"10\" name=\"pic-9e\" usage=\"computational-1\" position=\"5\" storage-length=\"4\" display-length=\"10\" numeric=\"true\"/>\n"
					+ "            <item level=\"10\" name=\"pic-9f\" usage=\"computational-2\" position=\"9\" storage-length=\"8\" display-length=\"18\" numeric=\"true\"/>\n"
					+ "        </item>\n"
					+ "        <item level=\"05\" name=\"grp-2\" position=\"17\" storage-length=\"14\" display-length=\"24\">\n"
					+ "            <item level=\"10\" name=\"pic-9a\" picture=\"s9(4)\" usage=\"computational\" position=\"17\" storage-length=\"2\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
					+ "            <item level=\"10\" name=\"pic-9b\" picture=\"s9(4)\" usage=\"computational-3\" position=\"19\" storage-length=\"3\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
					+ "            <item level=\"10\" name=\"pic-9c\" picture=\"s9(4)\" usage=\"computational-4\" position=\"22\" storage-length=\"2\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
					+ "            <item level=\"10\" name=\"pic-9d\" picture=\"s9(4)\" usage=\"computational-5\" position=\"24\" storage-length=\"2\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
					+ "            <item level=\"10\" name=\"pic-9e\" picture=\"s9(4)\" usage=\"computational-6\" position=\"26\" storage-length=\"3\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
					+ "            <item level=\"10\" name=\"pic-9f\" picture=\"s9(4)\" usage=\"binary\" position=\"29\" storage-length=\"2\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
					+ "        </item>\n"
					+ "        <item level=\"05\" name=\"grp-3\" position=\"31\" storage-length=\"14\" display-length=\"24\">\n"
					+ "            <item level=\"10\" name=\"pic-9a\" picture=\"s9(4)\" usage=\"computational\" position=\"31\" storage-length=\"2\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
					+ "            <item level=\"10\" name=\"pic-9b\" picture=\"s9(4)\" usage=\"computational-3\" position=\"33\" storage-length=\"3\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
					+ "            <item level=\"10\" name=\"pic-9c\" picture=\"s9(4)\" usage=\"computational-4\" position=\"36\" storage-length=\"2\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
					+ "            <item level=\"10\" name=\"pic-9d\" picture=\"s9(4)\" usage=\"computational-5\" position=\"38\" storage-length=\"2\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
					+ "            <item level=\"10\" name=\"pic-9e\" picture=\"s9(4)\" usage=\"computational-6\" position=\"40\" storage-length=\"3\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
					+ "            <item level=\"10\" name=\"pic-9f\" picture=\"s9(4)\" usage=\"binary\" position=\"43\" storage-length=\"2\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
					+ "        </item>\n"
					+ "        <item level=\"05\" name=\"grp-4\" position=\"45\" storage-length=\"14\" display-length=\"24\">\n"
					+ "            <item level=\"10\" name=\"pic-9a\" picture=\"s9(4)\" usage=\"computational\" position=\"45\" storage-length=\"2\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
					+ "            <item level=\"10\" name=\"pic-9b\" picture=\"s9(4)\" usage=\"computational-3\" position=\"47\" storage-length=\"3\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
					+ "            <item level=\"10\" name=\"pic-9c\" picture=\"s9(4)\" usage=\"computational-4\" position=\"50\" storage-length=\"2\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
					+ "            <item level=\"10\" name=\"pic-9d\" picture=\"s9(4)\" usage=\"computational-5\" position=\"52\" storage-length=\"2\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
					+ "            <item level=\"10\" name=\"pic-9e\" picture=\"s9(4)\" usage=\"computational-6\" position=\"54\" storage-length=\"3\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
					+ "            <item level=\"10\" name=\"pic-9f\" picture=\"s9(4)\" usage=\"binary\" position=\"57\" storage-length=\"2\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
					+ "        </item>\n"
					+ "    </item>\n"
					+ "</copybook>";
	
	String cblSignClause
		= "        01  Record.\n"
		+ "            03 pic-x                  pic x(4).\n"
		+ "            03 unsigned-1             pic 9(4).\n"
		+ "            03 signed-1               pic s9(4).\n"
		+ "            03 signed-leading         pic s9(4) sign leading.\n"
		+ "            03 signed-trailing        pic s9(4) sign trailing.\n"
		+ "            03 signed-sep-leading     pic s9(4) sign leading separate.\n"
		+ "            03 signed-sep-trailing    pic s9(4) sign trailing separate.\n";
	

	String cblComp
		= "        01  Record.\n"
		+ "            05 grp-1.\n"
		+ "               10 pic-9                   pic s9(4).\n"
		+ "               10 pic-9e    comp-1.\n"
		+ "               10 pic-9f    comp-2.\n"
		+ "            05 grp-2.\n"
		+ "               10 pic-9a                  pic s9(4) comp.\n"
		+ "               10 pic-9b                  pic s9(4) comp-3.\n"
		+ "               10 pic-9c                  pic s9(4) comp-4.\n"
		+ "               10 pic-9d                  pic s9(4) comp-5.\n"
		+ "               10 pic-9e                  pic s9(4) comp-6.\n"
		+ "               10 pic-9f                  pic s9(4) binary.\n"
		+ "            05 grp-3.\n"
		+ "               10 pic-9a                  pic s9(4) usage comp.\n"
		+ "               10 pic-9b                  pic s9(4) usage comp-3.\n"
		+ "               10 pic-9c                  pic s9(4) usage comp-4.\n"
		+ "               10 pic-9d                  pic s9(4) usage comp-5.\n"
		+ "               10 pic-9e                  pic s9(4) usage comp-6.\n"
		+ "               10 pic-9f                  pic s9(4) usage binary.\n"
		+ "            05 grp-4.\n"
		+ "               10 pic-9a                  pic s9(4) computational.\n"
		+ "               10 pic-9b                  pic s9(4) computational-3.\n"
		+ "               10 pic-9c                  pic s9(4) computational-4.\n"
		+ "               10 pic-9d                  pic s9(4) computational-5.\n"
		+ "               10 pic-9e                  pic s9(4) computational-6.\n"
		+ "               10 pic-9f                  pic s9(4) binary.\n"
		;
	
	
	

	@Test
	public void testSignValues() {
		List<? extends IItem> itemTree
				= Cb2Xml3.newBuilder(new StringReader(cblSignClause), "Record")
						.asCobolItemTree().getChildItems();
		
		assertEquals(1, itemTree.size());
		
		List<? extends IItem> items = itemTree.get(0).getChildItems();
		assertEquals(7, items.size());
		
		for (int i = 0; i < items.size(); i++) {
			assertEquals((i > 0), items.get(i).getNumericClass().numeric);
			assertEquals(false, items.get(i).getNumericClass().editNumeric);
			assertEquals(0, items.get(i).getScale());
			assertEquals("Field: " + i, (i < 5) ? 4 : 5, items.get(i).getStorageLength());
			assertEquals("Field: " + i, (i < 5) ? 4 : 5, items.get(i).getDisplayLength());
		}
		
		assertEquals(Cb2xmlConstants.SignClause.NO_SIGN_CLAUSE, items.get(0).getSignClause());
		assertEquals(Cb2xmlConstants.SignClause.NO_SIGN_CLAUSE, items.get(1).getSignClause());
		assertEquals(Cb2xmlConstants.SignClause.NO_SIGN_CLAUSE, items.get(2).getSignClause());
		assertEquals(Cb2xmlConstants.SignClause.SIGN_LEADING,   items.get(3).getSignClause());
		assertEquals(Cb2xmlConstants.SignClause.SIGN_TRAILING,  items.get(4).getSignClause());
		assertEquals(Cb2xmlConstants.SignClause.SIGN_LEADING_SEPARATE,  items.get(5).getSignClause());
		assertEquals(Cb2xmlConstants.SignClause.SIGN_TRAILING_SEPARATE, items.get(6).getSignClause());
		
		assertEquals(Cb2xmlConstants.NumericClass.NON_NUMERIC, items.get(0).getNumericClass());
		assertEquals(Cb2xmlConstants.NumericClass.NUMERIC_IN_COBOL, items.get(1).getNumericClass());
		assertEquals(Cb2xmlConstants.NumericClass.NUMERIC_IN_COBOL, items.get(2).getNumericClass());
		assertEquals(Cb2xmlConstants.NumericClass.NUMERIC_IN_COBOL,   items.get(3).getNumericClass());
		assertEquals(Cb2xmlConstants.NumericClass.NUMERIC_IN_COBOL,  items.get(4).getNumericClass());
		assertEquals(Cb2xmlConstants.NumericClass.NUMERIC_IN_COBOL,  items.get(5).getNumericClass());
		assertEquals(Cb2xmlConstants.NumericClass.NUMERIC_IN_COBOL, items.get(6).getNumericClass());
	}
	
	@Test
	public void testUsageValues() {
		List<? extends IItem> itemTree 
				= Cb2Xml3.newBuilder(new StringReader(cblComp), "Record")
							.asCobolItemTree().getChildItems();
		
		assertEquals(1, itemTree.size());
		
		List<? extends IItem> items = itemTree.get(0).getChildItems();
		assertEquals(4, items.size());
		
		
		
		List<? extends IItem> grp1 = items.get(0).getChildItems();
		assertEquals(Cb2xmlConstants.Usage.NONE, grp1.get(0).getUsage());
		assertEquals(Cb2xmlConstants.Usage.COMP_1, grp1.get(1).getUsage());
		assertEquals(Cb2xmlConstants.Usage.COMP_2, grp1.get(2).getUsage());

		tstUsageGrp(items.get(1).getChildItems());
		tstUsageGrp(items.get(2).getChildItems());
		tstUsageGrp(items.get(3).getChildItems());
	}
	
	private void tstUsageGrp(List<? extends IItem> grp) {
		assertEquals(Cb2xmlConstants.Usage.COMP, grp.get(0).getUsage());
		assertEquals(Cb2xmlConstants.Usage.COMP_3, grp.get(1).getUsage());
		assertEquals(Cb2xmlConstants.Usage.COMP_4, grp.get(2).getUsage());
		assertEquals(Cb2xmlConstants.Usage.COMP_5, grp.get(3).getUsage());
		assertEquals(Cb2xmlConstants.Usage.COMP_6, grp.get(4).getUsage());
		assertEquals(Cb2xmlConstants.Usage.BINARY, grp.get(5).getUsage());
	}
	
	@Test
	public void testSignClasicXml() throws XMLStreamException, LexerException, IOException, ParserException {
		ICb2XmlBuilder builder = Cb2Xml3.newBuilder(new StringReader(cblSignClause), "Record")
										.setXmlFormat(Cb2xmlConstants.Cb2xmlXmlFormat.CLASSIC)
										.setIndent(true);
		assertEquals(expectedJustifiedClasicXml, builder.asXmlString());
	}

	@Test
	public void testUsageClasicXml() throws XMLStreamException, LexerException, IOException, ParserException {
		ICb2XmlBuilder builder = Cb2Xml3.newBuilder(new StringReader(cblComp), "Record")
										.setXmlFormat(Cb2xmlConstants.Cb2xmlXmlFormat.CLASSIC)
										.setIndent(true);
	
		assertEquals(expectedSignClasicXml, builder.asXmlString());
	}

	private static String EN_SIGN_TRAILING_1
		= "          05 SOME-VALUE-1   PIC 9(14)V9-.\n"
		+ "          05 SOME-VALUE-3   PIC 9(14)V999+.\n"
		+ "          05 SOME-VALUE-1a  PIC 9(14).9-.\n"
		+ "          05 SOME-VALUE-3a  PIC 9(14).999+.\n";
//		+ "          05 SOME-VALUE-1b  PIC 9(14).9DB.\n"
//		+ "          05 SOME-VALUE-3b  PIC 9(14).999DB.\n";
	
	private static String signTrailingXml
			="<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<copybook filename=\"Record\" dialect=\"Mainframe\" cb2xml-format=\"2017\">\n"
			+ "    <item level=\"05\" name=\"SOME-VALUE-1\" picture=\"9(14)V9-\" position=\"1\" storage-length=\"16\" display-length=\"16\" scale=\"1\" numeric=\"Numeric_Edited\" display-position=\"1\"/>\n"
			+ "    <item level=\"05\" name=\"SOME-VALUE-3\" picture=\"9(14)V999+\" position=\"17\" storage-length=\"18\" display-length=\"18\" scale=\"3\" numeric=\"Numeric_Edited\" display-position=\"17\"/>\n"
			+ "    <item level=\"05\" name=\"SOME-VALUE-1a\" picture=\"9(14).9-\" position=\"35\" storage-length=\"17\" display-length=\"17\" scale=\"1\" numeric=\"Numeric_Edited\" display-position=\"35\"/>\n"
			+ "    <item level=\"05\" name=\"SOME-VALUE-3a\" picture=\"9(14).999+\" position=\"52\" storage-length=\"19\" display-length=\"19\" scale=\"3\" numeric=\"Numeric_Edited\" display-position=\"52\"/>\n"
			+ "</copybook>";
	
	@Test
	public void testSignTrailingScale() throws XMLStreamException, LexerException, IOException, ParserException {
		ICb2XmlBuilder iob = Cb2Xml3.newBuilder(new StringReader(EN_SIGN_TRAILING_1), "Record")
									.setIndent(true);
		List<? extends IItem> itemTree 	= iob .asCobolItemTree().getChildItems();
		assertEquals(4, itemTree.size());

		cmpItem(itemTree.get(0), "1", 1, 16);
		cmpItem(itemTree.get(1), "3", 3, 18);
		cmpItem(itemTree.get(2), "1a", 1, 17);
		cmpItem(itemTree.get(3), "3a", 3, 19);
		
//		cmpItem(itemTree.get(4), "1b", 1, 18);
//		cmpItem(itemTree.get(5), "3b", 3, 20);
		

//		System.out.println(iob.asXmlString());
		
		assertEquals(iob.asXmlString(), signTrailingXml);
	}

	/**
	 * @param item
	 */
	private void cmpItem(IItem item, String ext,  int scale, int length) {

		assertEquals("SOME-VALUE-" + ext, item.getFieldName());
		assertEquals(scale, item.getScale());
		assertEquals(length, item.getDisplayLength());
		assertEquals(length, item.getStorageLength());
	}


}
