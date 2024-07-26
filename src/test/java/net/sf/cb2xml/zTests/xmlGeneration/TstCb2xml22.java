package net.sf.cb2xml.zTests.xmlGeneration;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.XMLStreamException;

import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import net.sf.cb2xml.Cb2Xml2;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;
import net.sf.cb2xml.zTests.common.Code;

public class TstCb2xml22 {
	private static String COPBOOK_PREF = "cobolCopybook/cbl2xml_Test";
	private static String XML_FILE_PREF = "xmlCopybook/cb2xml_Output";

	@Test
	public void test() throws IOException, SAXException, ParserConfigurationException, ParserException, LexerException, XMLStreamException {
		String xmlFilename = Code.getFullName(XML_FILE_PREF +  "102.xml");
		String cblFilename = Code.getFullName(COPBOOK_PREF  +  "102.cbl");
		String freeFormatCbl = Code.getFullName("cobolCopybook/FreeFormat.cbl");
		Document expected = Code.fileToDom(xmlFilename);
		
		File cobolCopybookFile = new File(cblFilename);
		File freeFormatFile = new File(freeFormatCbl);
		
		net.sf.cb2xml.zTests.common.Code.compare("Check 1: " , expected, Cb2Xml2.convertToXMLDOM(cobolCopybookFile));
		
		net.sf.cb2xml.zTests.common.Code.compare("Check 2: " , expected, Cb2Xml2.convertToXMLDOM(cobolCopybookFile, false));
		net.sf.cb2xml.zTests.common.Code.compare("Check 3: " , expected, Cb2Xml2.convertToXMLDOM(cobolCopybookFile, true));
		
		net.sf.cb2xml.zTests.common.Code.compare("Check 4: " , expected, Cb2Xml2.convertToXMLDOM(cobolCopybookFile, false, Cb2xmlConstants.USE_PROPERTIES_FILE));
		net.sf.cb2xml.zTests.common.Code.compare("Check 5: " , expected, Cb2Xml2.convertToXMLDOM(cobolCopybookFile, false, Cb2xmlConstants.USE_STANDARD_COLUMNS));
		net.sf.cb2xml.zTests.common.Code.compare("Check 6: " , expected, Cb2Xml2.convertToXMLDOM(cobolCopybookFile, false, 6, 80));
		net.sf.cb2xml.zTests.common.Code.compare("Check 7: " , expected, Cb2Xml2.convertToXMLDOM(freeFormatFile, false, Cb2xmlConstants.FREE_FORMAT));
		net.sf.cb2xml.zTests.common.Code.compare("Check 8: " , expected, Cb2Xml2.convertToXMLDOM(cobolCopybookFile, false, Cb2xmlConstants.USE_COLS_6_TO_80));
		net.sf.cb2xml.zTests.common.Code.compare("Check 9: " , expected, Cb2Xml2.convertToXMLDOM(cobolCopybookFile, false, Cb2xmlConstants.USE_LONG_LINE));
		
		net.sf.cb2xml.zTests.common.Code.compare("Check 14: " , expected, Cb2Xml2.convertToXMLDOM(new FileInputStream(cobolCopybookFile), "Ams-Vendor", false, Cb2xmlConstants.USE_PROPERTIES_FILE));
		net.sf.cb2xml.zTests.common.Code.compare("Check 15: " , expected, Cb2Xml2.convertToXMLDOM(new FileInputStream(cobolCopybookFile), "Ams-Vendor", false, Cb2xmlConstants.USE_STANDARD_COLUMNS)); 
		net.sf.cb2xml.zTests.common.Code.compare("Check 16: " , expected, Cb2Xml2.convertToXMLDOM(new FileInputStream(cobolCopybookFile), "Ams-Vendor", false, 6, 80));
		net.sf.cb2xml.zTests.common.Code.compare("Check 17: " , expected, Cb2Xml2.convertToXMLDOM(new FileInputStream(freeFormatFile), "Ams-Vendor", false, Cb2xmlConstants.FREE_FORMAT));
		net.sf.cb2xml.zTests.common.Code.compare("Check 18: " , expected, Cb2Xml2.convertToXMLDOM(new FileInputStream(cobolCopybookFile), "Ams-Vendor", false, Cb2xmlConstants.USE_COLS_6_TO_80)); 
		net.sf.cb2xml.zTests.common.Code.compare("Check 19: " , expected, Cb2Xml2.convertToXMLDOM(new FileInputStream(cobolCopybookFile), "Ams-Vendor", false, Cb2xmlConstants.USE_LONG_LINE)); 
	}
}
