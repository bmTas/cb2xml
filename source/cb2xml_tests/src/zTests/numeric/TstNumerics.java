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

public class TstNumerics {

	
	String cblNumericClause
		= "        01  Record.\n"
		+ "            03 pic-x                  pic x(4).\n"
		+ "            03 edit-numeric-1         pic zzz9.\n"
		+ "            03 edit-numeric-2         pic -,--9.\n"
		+ "            03 num-1                  pic s9(4).\n"
		;

	String numericXmlStr
		= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
		+ "<copybook filename=\"Record\" dialect=\"Mainframe\">\n"
		+ "    <item level=\"01\" name=\"Record\" position=\"1\" storage-length=\"17\" display-length=\"17\">\n"
		+ "        <item level=\"03\" name=\"pic-x\" picture=\"x(4)\" position=\"1\" storage-length=\"4\" display-length=\"4\"/>\n"
		+ "        <item level=\"03\" name=\"edit-numeric-1\" picture=\"zzz9\" position=\"5\" storage-length=\"4\" display-length=\"4\" numeric=\"true\" editted-numeric=\"true\"/>\n"
		+ "        <item level=\"03\" name=\"edit-numeric-2\" picture=\"-,--9\" position=\"9\" storage-length=\"5\" display-length=\"5\" numeric=\"true\" editted-numeric=\"true\"/>\n"
		+ "        <item level=\"03\" name=\"num-1\" picture=\"s9(4)\" position=\"14\" storage-length=\"4\" display-length=\"4\" numeric=\"true\" signed=\"true\"/>\n"
		+ "    </item>\n"
		+ "</copybook>";
	
	@Test
	public void testNumericClause() throws XMLStreamException, LexerException, IOException, ParserException {
		List<? extends IItem> itemTree
					= Cb2Xml3.newBuilder(new StringReader(cblNumericClause), "Record")
							.asCobolItemTree().getChildItems();

		assertEquals(1, itemTree.size());

		List<? extends IItem> items = itemTree.get(0).getChildItems();
		
		assertEquals(Cb2xmlConstants.NumericClass.NON_NUMERIC, items.get(0).getNumericClass());
		assertEquals(Cb2xmlConstants.NumericClass.NUMERIC_EDITED, items.get(1).getNumericClass());
		assertEquals(Cb2xmlConstants.NumericClass.NUMERIC_EDITED, items.get(2).getNumericClass());
		assertEquals(Cb2xmlConstants.NumericClass.NUMERIC_IN_COBOL,   items.get(3).getNumericClass());
		
		ICb2XmlBuilder builder = Cb2Xml3.newBuilder(new StringReader(cblNumericClause), "Record")
				.setXmlFormat(Cb2xmlConstants.Cb2xmlXmlFormat.CLASSIC)
				.setIndent(true);
		//System.out.println(builder.asXmlString());
		assertEquals(numericXmlStr, builder.asXmlString());
	}

	
	String cblDecimal
				= "          03 Num-1          pic s9.\n"
				+ "          03 Num-2          pic s9V9.\n"
				+ "          03 Num-3          pic s9V99.\n"
				+ "          03 Num-4          pic s9V999.\n"
				+ "          03 Num-5          pic s9V9999.\n"
				+ "          03 Num-6          pic s9V99999.\n";
	
	String xmlDecimalStr
				="<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
				+ "<copybook filename=\"Record\" dialect=\"Mainframe\">\n"
				+ "    <item level=\"03\" name=\"Num-1\" picture=\"s9\" position=\"1\" storage-length=\"1\" display-length=\"1\" numeric=\"true\" signed=\"true\"/>\n"
				+ "    <item level=\"03\" name=\"Num-2\" picture=\"s9V9\" position=\"2\" storage-length=\"2\" display-length=\"2\" scale=\"1\" numeric=\"true\" signed=\"true\"/>\n"
				+ "    <item level=\"03\" name=\"Num-3\" picture=\"s9V99\" position=\"4\" storage-length=\"3\" display-length=\"3\" scale=\"2\" numeric=\"true\" signed=\"true\"/>\n"
				+ "    <item level=\"03\" name=\"Num-4\" picture=\"s9V999\" position=\"7\" storage-length=\"4\" display-length=\"4\" scale=\"3\" numeric=\"true\" signed=\"true\"/>\n"
				+ "    <item level=\"03\" name=\"Num-5\" picture=\"s9V9999\" position=\"11\" storage-length=\"5\" display-length=\"5\" scale=\"4\" numeric=\"true\" signed=\"true\"/>\n"
				+ "    <item level=\"03\" name=\"Num-6\" picture=\"s9V99999\" position=\"16\" storage-length=\"6\" display-length=\"6\" scale=\"5\" numeric=\"true\" signed=\"true\"/>\n"
				+ "</copybook>";
	
	@Test
	public void testDecimal() throws XMLStreamException, LexerException, IOException, ParserException {
		List<? extends IItem> itemTree
				= Cb2Xml3.newBuilder(new StringReader(cblDecimal), "Record")
						 .asCobolItemTree().getChildItems();

		assertEquals(6, itemTree.size());
		
		List<? extends IItem> items = itemTree;
		
		for (int i = 0; i < itemTree.size(); i++) {
			assertEquals("" + i, i, items.get(i).getScale());
			assertEquals("" + i, i+1, items.get(i).getStorageLength());
			assertEquals("" + i, i+1, items.get(i).getDisplayLength());
		}
		
		ICb2XmlBuilder builder = Cb2Xml3.newBuilder(new StringReader(cblDecimal), "Record")
				.setXmlFormat(Cb2xmlConstants.Cb2xmlXmlFormat.CLASSIC)
				.setIndent(true);
		//System.out.println(builder.asXmlString());
		assertEquals(xmlDecimalStr, builder.asXmlString());
	}

	String cblNumeric
			= "          03 Num-1          pic s9V99.\n"
			+ "          03 Num-2          pic s99V99.\n"
			+ "          03 Num-3          pic s999V99.\n"
			+ "          03 Num-4          pic s9999V99.\n"
			+ "          03 Num-5          pic s99999V99.\n"
			+ "          03 Num-6          pic s999999V99.\n"
			;
	String xmlNumericStr
			= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<copybook filename=\"Record\" dialect=\"Mainframe\">\n"
			+ "    <item level=\"03\" name=\"Num-1\" picture=\"s9V99\" position=\"1\" storage-length=\"3\" display-length=\"3\" scale=\"2\" numeric=\"true\" signed=\"true\"/>\n"
			+ "    <item level=\"03\" name=\"Num-2\" picture=\"s99V99\" position=\"4\" storage-length=\"4\" display-length=\"4\" scale=\"2\" numeric=\"true\" signed=\"true\"/>\n"
			+ "    <item level=\"03\" name=\"Num-3\" picture=\"s999V99\" position=\"8\" storage-length=\"5\" display-length=\"5\" scale=\"2\" numeric=\"true\" signed=\"true\"/>\n"
			+ "    <item level=\"03\" name=\"Num-4\" picture=\"s9999V99\" position=\"13\" storage-length=\"6\" display-length=\"6\" scale=\"2\" numeric=\"true\" signed=\"true\"/>\n"
			+ "    <item level=\"03\" name=\"Num-5\" picture=\"s99999V99\" position=\"19\" storage-length=\"7\" display-length=\"7\" scale=\"2\" numeric=\"true\" signed=\"true\"/>\n"
			+ "    <item level=\"03\" name=\"Num-6\" picture=\"s999999V99\" position=\"26\" storage-length=\"8\" display-length=\"8\" scale=\"2\" numeric=\"true\" signed=\"true\"/>\n"
			+ "</copybook>";

	@Test
	public void testNumeric() throws XMLStreamException, LexerException, IOException, ParserException {
		List<? extends IItem> itemTree
				= Cb2Xml3.newBuilder(new StringReader(cblNumeric), "Record")
						 .asCobolItemTree().getChildItems();

		assertEquals(6, itemTree.size());
		
		List<? extends IItem> items = itemTree;
		
		for (int i = 0; i < itemTree.size(); i++) {
			assertEquals("" + i, 2, items.get(i).getScale());
			assertEquals("" + i, i+3, items.get(i).getStorageLength());
			assertEquals("" + i, i+3, items.get(i).getDisplayLength());
		}
		
		ICb2XmlBuilder builder = Cb2Xml3.newBuilder(new StringReader(cblNumeric), "Record")
				.setXmlFormat(Cb2xmlConstants.Cb2xmlXmlFormat.CLASSIC)
				.setIndent(true);
		//System.out.println(builder.asXmlString());
		assertEquals(xmlNumericStr, builder.asXmlString());
	}
}
