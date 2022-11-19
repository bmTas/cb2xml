package zTests.text;

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

public class TstJustify {

	static String expectedXml 
				= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
				+ "<copybook filename=\"Record\" dialect=\"Mainframe\" cb2xml-format=\"2017\">\n"
				+ "    <item level=\"01\" name=\"Record\" position=\"1\" storage-length=\"9\" display-length=\"9\" display-position=\"1\">\n"
				+ "        <item level=\"03\" name=\"pic-x-1\" picture=\"x(3)\" position=\"1\" storage-length=\"3\" display-length=\"3\" display-position=\"1\"/>\n"
				+ "        <item level=\"03\" name=\"pic-x-Justified\" picture=\"x(3)\" justified=\"true\" position=\"4\" storage-length=\"3\" display-length=\"3\" display-position=\"4\"/>\n"
				+ "        <item level=\"03\" name=\"pic-x-Justified-right\" picture=\"x(3)\" justified=\"right\" position=\"7\" storage-length=\"3\" display-length=\"3\" display-position=\"7\"/>\n"
				+ "    </item>\n"
				+ "</copybook>";
	
	static String cblJustified
		=  "        01  Record.\n"
		+ "            03 pic-x-1                 pic x(3).\n"
		+ "            03 pic-x-Justified         pic x(3) justified.\n"
		+ "            03 pic-x-Justified-right   pic x(3) justified right.\n";

	
	/**
	 * Test field values set correctly
	 */
	@Test
	public void testFieldValues() {
		
		List<? extends IItem> itemTree = newBuilder()	.asCobolItemTree()	.getChildItems();
		
		assertEquals(1, itemTree.size());
		
		List<? extends IItem> items = itemTree.get(0).getChildItems();
		assertEquals(3, items.size());
		
		for (int i = 0; i < items.size(); i++) {
			assertEquals(false, items.get(i).getNumericClass().numeric);
			assertEquals(false, items.get(i).getNumericClass().editNumeric);
			assertEquals(0, items.get(i).getScale());
			assertEquals("Field: " + i, 3, items.get(i).getStorageLength());
			assertEquals("Field: " + i, 3, items.get(i).getDisplayLength());
		}

		
		assertEquals(Cb2xmlConstants.Justified.NOT_JUSTIFIED, items.get(0).getJustified());
		assertEquals(Cb2xmlConstants.Justified.JUSTIFIED, items.get(1).getJustified());
		assertEquals(Cb2xmlConstants.Justified.RIGHT, items.get(2).getJustified());

	}
	
	/**
	 * Text Xml generated correctly
	 */
	@Test
	public void testXml() throws XMLStreamException, LexerException, IOException, ParserException {
		ICb2XmlBuilder bldr = newBuilder()	.setIndent(true);
		assertEquals(expectedXml, bldr.asXmlString());
	}

	private ICb2XmlBuilder newBuilder() {
		return Cb2Xml3.newBuilder(new StringReader(cblJustified), "Record");
	}

}
