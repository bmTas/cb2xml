package tests;


import java.io.File;
import java.io.IOException;

import javax.xml.parsers.ParserConfigurationException;

import net.sf.cb2xml.Cb2Xml;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;
import net.sf.cb2xml.util.Parms;

import org.junit.Test;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import common.Code;

public class TstCb2xml31a {
	

	private static final boolean DO_COMPARE = true; 
	
	private static final String TEMP_DIR = DO_COMPARE
			? System.getProperty("java.io.tmpdir") + File.separator
			: "/home/bruce/work/XmlDir/";

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
	
	private String[] COPYBOOK_LIST7 = {
			"cpyPointer.cbl"
	};


	@Test
	public void testArray1old() throws IOException, SAXException, ParserConfigurationException, ParserException, LexerException {
		tstArray(COPYBOOK_LIST1, "cobolCopybook/", "", false);
		tstArray(COPYBOOK_LIST2, "cobolCopybook/", "", false);
		tstArray(COPYBOOK_LIST3, "cobolCopybook/", "", false);
		tstArray(COPYBOOK_LIST4, "cobolCopybook/", "", false);
	}


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

	@Test
	public void testArray7() throws IOException, SAXException, ParserConfigurationException, ParserException, LexerException {
		tstArray(COPYBOOK_LIST7, "cobolCopybook/", "utf-8", true, false);
		tstArray(COPYBOOK_LIST7, "cobolCopybook/", "utf-8", true, true);
	}
	
	

	public void tstArray(String[] copybooks, String font) 
			throws IOException, SAXException, ParserConfigurationException, ParserException, LexerException  {
		tstArray(copybooks, "cobolCopybook/", font, true, false);
	}


	public void tstArray(String[] copybooks,  String dir, String font) throws IOException, SAXException, ParserConfigurationException, ParserException, LexerException  {
		tstArray(copybooks, dir, font, true, false);
	}


	public void tstArray(String[] copybooks,  String dir, String font, boolean newFormat)
			throws IOException, SAXException, ParserConfigurationException, ParserException, LexerException  {
		tstArray(copybooks, dir, font, newFormat, false);
	}


	public void tstArray(String[] copybooks,  String dir, String font, boolean newFormat, boolean bit64)
			throws IOException, SAXException, ParserConfigurationException, ParserException, LexerException  {
		String cblFilename, xmlFilename, xmlOut;
		String tmpDir = DO_COMPARE ? TEMP_DIR : TEMP_DIR + "/xmlCopybookFormated/"; 
		System.out.println(tmpDir);
		String xmlCompare = "xmlCopybookFormated/";
		
		for (String c : copybooks) {
			String ext = bit64 ? "64bit.Xml" : "Xml";
			xmlFilename = "xmlCpy" + c.substring(3, c.length() - 3) + ext;
			xmlOut = tmpDir + xmlFilename;
			System.out.println("Test: " + c + " " + xmlFilename + " " + xmlOut);
			cblFilename = Code.getFullName(dir + c);
			
			String[] args;
			if (font == null) {
				args = new String[] {
					"-cobol", cblFilename, "-xml", xmlOut, Parms.INDENT_XML_PRM
				};
			} else if (bit64) {
				args = new String[] {
						"-Dialect", "Mainframe_64_Bit", "-cobol", cblFilename, "-xml", xmlOut, "-font", font, Parms.INDENT_XML_PRM
					};
			} else if (newFormat) {
				args = new String[] {
						"-cobol", cblFilename, "-xml", xmlOut, "-font", font, Parms.INDENT_XML_PRM
					};
			} else {
				xmlCompare = "xmlCopybookClassicFormated/";
				if (! DO_COMPARE) {
					xmlOut = TEMP_DIR + "/" + xmlCompare + xmlFilename;
				}
				args = new String[] {
						"-cobol", cblFilename, "-xml", xmlOut, "-font", font, Parms.XML_FORMAT_PRM, "Classic", Parms.INDENT_XML_PRM
					};
			}
			
			
			Cb2Xml.main(args);

			
			if (DO_COMPARE) {
				System.out.println(c + " --> " + xmlFilename + ":");
				System.out.println(c + " --> " + xmlFilename + ":");
				Document doc = Code.fileToDom(xmlOut);
				
	//			System.out.println(XmlUtils.domToString(doc));
	//			System.out.println();
			
				System.out.println(xmlCompare + xmlFilename);
				xmlFilename = Code.getFullName(xmlCompare + xmlFilename);
				common.Code.compare("File: " + cblFilename, xmlFilename, doc);
			}
			
		}
	}
}
