package tests;


import java.io.File;
import java.io.IOException;

import javax.xml.parsers.ParserConfigurationException;

import net.sf.cb2xml.Cb2Xml;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;
import net.sf.cb2xml.util.XmlUtils;

import org.junit.Test;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import common.Code;

public class TstCb2xml31a {
	

	
	
	private final String[] COPYBOOK_LIST1 = {
			"cpyComp3aInheritence.cbl",
			"cpyComp.cbl",
			"cpyComp3.cbl",
			"cpyComp5.cbl",
			"cpyCompP.cbl",
			"cpyComp3P.cbl",
			"cpyComp5P.cbl",
			"cpyComp3Inheritence.cbl",
			"cpyCompInheritence.cbl",
	};

	private String[] COPYBOOK_LIST2 = {
			"cpyCompSync.cbl",
			"cpyComp3Sync.cbl",
			"cpyComp5Sync.cbl",
	};

	private String[] COPYBOOK_LIST3 = {
			"cpyUnxdetail.cbl",
			"cpyOccurs.cbl",
			"cpyOccursDepending.cbl",
			"cpyOccursDependingOn21.cbl",
			"cpyOccursDependingOn29.cbl",
			"cpyOccursDependingOn42.cbl",
			"cpyBitOfEverything.cbl",
	};

	
	private String[] COPYBOOK_LIST4 = {
			"cpyRedefSize.cbl",
			"cpyValueContinuation.cbl",
			"cpyValueContinuation1.cbl",
			"cpyValueContinuation2.cbl",
			"cpyMsg.cbl",
			"cpyHexValue.cbl",
	};
	
	private String[] COPYBOOK_LIST6 = {
			"cpyUtf8.cbl"
	};



	@Test
	public void testArray1a() throws IOException, SAXException, ParserConfigurationException, ParserException, LexerException {
		tstArray(COPYBOOK_LIST1, null);
	}

	@Test
	public void testArray1b() throws IOException, SAXException, ParserConfigurationException, ParserException, LexerException {
		tstArray(COPYBOOK_LIST1, "");
	}


	@Test
	public void testArray1c() throws IOException, SAXException, ParserConfigurationException, ParserException, LexerException {
		tstArray(COPYBOOK_LIST1, "utf-8");
	}

	@Test
	public void testArray2() throws IOException, SAXException, ParserConfigurationException, ParserException, LexerException {
		tstArray(COPYBOOK_LIST2, null);
	}

	@Test
	public void testArray3() throws IOException, SAXException, ParserConfigurationException, ParserException, LexerException {
		tstArray(COPYBOOK_LIST3, null);
	}


	@Test
	public void testArray4() throws IOException, SAXException, ParserConfigurationException, ParserException, LexerException {
		tstArray(COPYBOOK_LIST4, null);
	}

	@Test
	public void testArray5() throws IOException, SAXException, ParserConfigurationException, ParserException, LexerException {
		tstArray(COPYBOOK_LIST4, "ebcdicCopybook/", "cp037");
	}
	

	@Test
	public void testArray6() throws IOException, SAXException, ParserConfigurationException, ParserException, LexerException {
		tstArray(COPYBOOK_LIST6, "cobolCopybook/", "utf-8");
	}


	public void tstArray(String[] copybooks, String font) 
			throws IOException, SAXException, ParserConfigurationException, ParserException, LexerException  {
		tstArray(copybooks, "cobolCopybook/", font);
	}


	public void tstArray(String[] copybooks,  String dir, String font) throws IOException, SAXException, ParserConfigurationException, ParserException, LexerException  {
		String cblFilename, xmlFilename, xmlOut;
			String tmpDir = System.getProperty("java.io.tmpdir") + File.separator;
		
		for (String c : copybooks) {
			xmlFilename = "xmlCpy" + c.substring(3, c.length() - 3) + "Xml";
			xmlOut = tmpDir + xmlFilename;
			System.out.println("Test: " + c + " " + xmlFilename + " " + xmlOut);
			cblFilename = Code.getFullName(dir + c);
			
			String[] args;
			if (font == null) {
				args = new String[] {
					"-cobol", cblFilename, "-xml", xmlOut
				};
			} else {
				args = new String[] {
						"-cobol", cblFilename, "-xml", xmlOut, "-font", font
					};
			}
			
			
			Cb2Xml.main(args);

			Document doc = Code.fileToDom(xmlOut);
			
			System.out.println(c + " --> " + xmlFilename + ":");
			System.out.println(XmlUtils.domToString(doc));
			System.out.println();
		
			
			xmlFilename = Code.getFullName("xmlCopybook/" + xmlFilename);
			common.Code.compare("File: " + cblFilename, xmlFilename, doc);
			
		}
		//fail("Not yet implemented");
	}
}
