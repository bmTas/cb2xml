package zTests.options;

import static org.junit.Assert.*;

import java.io.IOException;
import java.io.StringReader;

import javax.xml.stream.XMLStreamException;

import org.junit.Test;

import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.ICb2XmlBuilder;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;

public class TstIndent {
/*	static String expectedNormalXml 
				= "<?xml version=\"1.0\" encoding=\"UTF-8\"?><copybook filename=\"Record\" dialect=\"Mainframe\" cb2xml-format=\"2017\">"
				+ "<item level=\"03\" name=\"pic-x-1\" picture=\"x(3)\" position=\"1\" storage-length=\"3\" display-length=\"3\"/>"
				+ "<item level=\"03\" name=\"pic-x-Justified\" picture=\"x(3)\" justified=\"true\" position=\"4\" storage-length=\"3\" display-length=\"3\"/>"
				+ "</copybook>";
*/	
	static String expectedNormalXml 
				= "<?xml version=\"1.0\" encoding=\"UTF-8\"?><copybook filename=\"Record\" dialect=\"Mainframe\" cb2xml-format=\"2017\">"
				+ "<item level=\"03\" name=\"pic-x-1\" picture=\"x(3)\" position=\"1\" storage-length=\"3\" display-length=\"3\" display-position=\"1\"/>"
				+ "<item level=\"03\" name=\"pic-x-Justified\" picture=\"x(3)\" justified=\"true\" position=\"4\" storage-length=\"3\" display-length=\"3\" display-position=\"4\"/>"
				+ "</copybook>";
	static String expectedIndentedXml 
				= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
				+ "<copybook filename=\"Record\" dialect=\"Mainframe\" cb2xml-format=\"2017\">\n"
				+ "    <item level=\"03\" name=\"pic-x-1\" picture=\"x(3)\" position=\"1\" storage-length=\"3\" display-length=\"3\" display-position=\"1\"/>\n"
				+ "    <item level=\"03\" name=\"pic-x-Justified\" picture=\"x(3)\" justified=\"true\" position=\"4\" storage-length=\"3\" display-length=\"3\" display-position=\"4\"/>\n"
				+ "</copybook>";
//	static String expectedIndentedXml 
//	= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
//	+ "<copybook filename=\"Record\" dialect=\"Mainframe\" cb2xml-format=\"2017\">\n"
//	+ "    <item level=\"03\" name=\"pic-x-1\" picture=\"x(3)\" position=\"1\" storage-length=\"3\" display-length=\"3\"/>\n"
//	+ "    <item level=\"03\" name=\"pic-x-Justified\" picture=\"x(3)\" justified=\"true\" position=\"4\" storage-length=\"3\" display-length=\"3\"/>\n"
//	+ "</copybook>";
	static String cbl
	= "            03 pic-x-1                 pic x(3).\n"
	+ "            03 pic-x-Justified         pic x(3) justified.\n";

	@Test
	public void test() throws XMLStreamException, LexerException, IOException, ParserException {
		ICb2XmlBuilder bldr = Cb2Xml3.newBuilder(new StringReader(cbl), "Record");
		assertEquals(expectedNormalXml, bldr.asXmlString());
		
		bldr.setIndent(true);
		//System.out.println(bldr.asXmlString());
		assertEquals(expectedIndentedXml, bldr.asXmlString());

	}

}
