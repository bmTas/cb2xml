package net.sf.cb2xml.zTests.xmlGeneration;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;

import javax.xml.stream.XMLStreamException;

import org.junit.jupiter.api.Test;

import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.copybookReader.ReadCobolCopybook;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;

class TestComments3 {

	@Test
	void testCommentPosition() throws XMLStreamException, LexerException, IOException, ParserException {
		String copybook = ""
				+ "      **********************************************************************\n"
				+ "      * Test Copybook\n"
				+ "      **********************************************************************\n"
				+ "        01  Test-Copyook.\n"
				+ "            03 field-1                       pic xxx.\n"
				+ "            03 field-2                       pic xx.\n"
				+ "      * Comment 3a    \n"
				+ "      * Comment 3b\n"
				+ "            03 field-3                       pic xx.    \n"
				+ "      * Comment 4a       \n"
				+ "            03 field-4     \n"
				+ "      * Comment 4b      \n"
				+ "      * Comment 4c      \n"
				+ "                                             pic \n"
				+ "      * Comment 4d     \n"
				+ "                                                 x(6). \n"
				+ "                                                   \n"
				+ "           03 field-5                        pic x(4).\n"
				+ "      * Comment 6a\n"
				+ "           03 field-6\n"
				+ "      * Comment 6b\n"
				+ "                                             pic \n"
				+ "      * Comment 6c\n"
				+ "                                                  s9(4)\n"
				+ "      * Comment 6d -usage\n"
				+ "                                                comp-3.       \n"
				+ "      * Comment 7a\n"
				+ "           03 field-7                        pic x(7).\n"
				+ "      * Comment 8a";
		
		String expectedXml = ""
				+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
				+ "<copybook filename=\"FOO\" dialect=\"Mainframe\" cb2xml-format=\"2017\">\n"
				+ "    <!--**********************************************************************-->\n"
				+ "    <!--* Test Copybook-->\n"
				+ "    <!--**********************************************************************-->\n"
				+ "    <item level=\"01\" name=\"Test-Copyook\" position=\"1\" storage-length=\"27\" display-length=\"28\" display-position=\"1\">\n"
				+ "        <item level=\"03\" name=\"field-1\" picture=\"xxx\" position=\"1\" storage-length=\"3\" display-length=\"3\" display-position=\"1\"/>\n"
				+ "        <item level=\"03\" name=\"field-2\" picture=\"xx\" position=\"4\" storage-length=\"2\" display-length=\"2\" display-position=\"4\"/>\n"
				+ "        <!--* Comment 3a-->\n"
				+ "        <!--* Comment 3b-->\n"
				+ "        <item level=\"03\" name=\"field-3\" picture=\"xx\" position=\"6\" storage-length=\"2\" display-length=\"2\" display-position=\"6\"/>\n"
				+ "        <!--* Comment 4a-->\n"
				+ "        <item level=\"03\" name=\"field-4\" picture=\"x(6)\" position=\"8\" storage-length=\"6\" display-length=\"6\" display-position=\"8\"/>\n"
				+ "        <item level=\"03\" name=\"field-5\" picture=\"x(4)\" position=\"14\" storage-length=\"4\" display-length=\"4\" display-position=\"14\"/>\n"
				+ "        <!--* Comment 6a-->\n"
				+ "        <item level=\"03\" name=\"field-6\" picture=\"s9(4)\" usage=\"computational-3\" position=\"18\" storage-length=\"3\" display-length=\"4\" numeric=\"COBOL_NUMERIC\" display-position=\"18\"/>\n"
				+ "        <!--* Comment 7a-->\n"
				+ "        <item level=\"03\" name=\"field-7\" picture=\"x(7)\" position=\"21\" storage-length=\"7\" display-length=\"7\" display-position=\"22\"/>\n"
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
