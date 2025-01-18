package net.sf.cb2xml.zTests.xmlGeneration;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;

import javax.xml.stream.XMLStreamException;

import org.junit.jupiter.api.Test;

import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.copybookReader.ReadCobolCopybook;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;

class Test88levelComments {

	@Test
	void testCommentPosition() throws XMLStreamException, LexerException, IOException, ParserException {
		String copybook = ""
				+ " 01  FOO.\n"
				+ "  * Code assigned by M \n"
				+ "     03 CODE-NAME             pic 999.\n"
				+ "  * comment for Mr Bond\n"
				+ "        88 IS-JAMES-BOND value 007.\n"
				+ "  * comment for the devil\n"
				+ "        88 IS-THE-DEVIL value 666.\n"
				+ "  * comment xx\n"
				+ "       88 xxx value       123\n"
				+ "  * comment xx2\n"
				+ "                          456.\n";
		String expectedXml = ""
				+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
				+ "<copybook filename=\"FOO\" dialect=\"Mainframe\" cb2xml-format=\"2017\">\n"
				+ "    <item level=\"01\" name=\"FOO\" position=\"1\" storage-length=\"3\" display-length=\"3\" display-position=\"1\">\n"
				+ "        <!--* Code assigned by M-->\n"
				+ "        <item level=\"03\" name=\"CODE-NAME\" picture=\"999\" position=\"1\" storage-length=\"3\" display-length=\"3\" numeric=\"COBOL_NUMERIC\" display-position=\"1\">\n"
				+ "            <!--* comment for Mr Bond-->\n"
				+ "            <condition name=\"IS-JAMES-BOND\">\n"
				+ "                <condition value=\"007\"/>\n"
				+ "            </condition>\n"
				+ "            <!--* comment for the devil-->\n"
				+ "            <condition name=\"IS-THE-DEVIL\">\n"
				+ "                <condition value=\"666\"/>\n"
				+ "            </condition>\n"
				+ "            <!--* comment xx-->\n"
				+ "            <condition name=\"xxx\">\n"
				+ "                <condition value=\"123\"/>\n"
				+ "                <condition value=\"456\"/>\n"
				+ "            </condition>\n"
				+ "            <!--* comment xx2-->\n"
				+ "        </item>\n"
				+ "    </item>\n"
				+ "</copybook>";
		
		String s = Cb2Xml3.newBuilder(
				new ReadCobolCopybook()
					.setCopybookName("FOO")
					.addFreeFormatCobolText(copybook)
				)
		    .setIndent(true)
		    .asXmlString();
		
		//System.out.println(s);
		
		assertEquals(expectedXml, s);

	}
	
	@Test
	void testCommentComplexCondition() throws XMLStreamException, LexerException, IOException, ParserException {
		String copybook = ""
				+ " 01  FOO.\n"
				+ "  * Code assigned by M \n"
				+ "     03 CODE-NAME             pic 999.\n"
					+ "  * comment xx 1\n"
				+ "       88 xxx value       123 thru 127, 129 150 thru 160\n"
				+ "  * comment xx2\n"
				+ "                          456.\n";
		String expectedXml = ""
				+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
				+ "<copybook filename=\"FOO\" dialect=\"Mainframe\" cb2xml-format=\"2017\">\n"
				+ "    <item level=\"01\" name=\"FOO\" position=\"1\" storage-length=\"3\" display-length=\"3\" display-position=\"1\">\n"
				+ "        <!--* Code assigned by M-->\n"
				+ "        <item level=\"03\" name=\"CODE-NAME\" picture=\"999\" position=\"1\" storage-length=\"3\" display-length=\"3\" numeric=\"COBOL_NUMERIC\" display-position=\"1\">\n"
				+ "            <!--* comment xx 1-->\n"
				+ "            <condition name=\"xxx\">\n"
				+ "                <condition value=\"123\" through=\"127\"/>\n"
				+ "                <condition value=\"129\"/>\n"
				+ "                <condition value=\"150\" through=\"160\"/>\n"
				+ "                <condition value=\"456\"/>\n"
				+ "            </condition>\n"
				+ "            <!--* comment xx2-->\n"
				+ "        </item>\n"
				+ "    </item>\n"
				+ "</copybook>";
		
		String s = Cb2Xml3.newBuilder(
				new ReadCobolCopybook()
					.setCopybookName("FOO")
					.addFreeFormatCobolText(copybook)
				)
		    .setIndent(true)
		    .asXmlString();
		
		//System.out.println(s);
		
		assertEquals(expectedXml, s);

	}


}
