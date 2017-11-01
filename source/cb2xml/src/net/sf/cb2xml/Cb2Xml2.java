/*************************************************************
 * This file is part of CB2XML.
 * See the file "LICENSE" for copyright information and the
 * terms and conditions for copying, distribution and
 * modification of CB2XML.
 *************************************************************
 */

package net.sf.cb2xml;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

import javax.xml.stream.XMLStreamException;
import javax.xml.transform.dom.DOMResult;

import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;
import net.sf.cb2xml.util.XmlUtils;

import org.w3c.dom.Document;

/**
 * This is a replacement for Cb2xml that does not swallow the exceptions and errors:
 * 
 * main utility for parsing a copybook into XML
 * calls pre-processor, then parser to perform parse
 * note the debug mode to view detailed SableCC debug output
 *
 * @author Bruce Martin
 * 
 * @deprecated Use Cb2xml3 instead !!!. This interface uses the old Cobol parser that 
 * will not be maintained.
 * 
 */

public class Cb2Xml2 {
	
	static final int USE_DEFAULT_THREADSIZE = 0;
	static final int USE_DEFAULT_CB2XML_THREADSIZE = 1;
	static final int ONE_MEG = 1024 * 1024;
	static final int DEFAULT_THREAD_SIZE = 4 * ONE_MEG;

	
	private static final int FIRST_COBOL_COLUMN = 6;
	private static final int LAST_COBOL_COLUMN = 72;
	
	private static int[] END_COLS = new int[Cb2xmlConstants.FREE_FORMAT];
	
	static {
		END_COLS[Cb2xmlConstants.USE_STANDARD_COLUMNS] = LAST_COBOL_COLUMN;
		END_COLS[Cb2xmlConstants.USE_COLS_6_TO_80] =  80;
		END_COLS[Cb2xmlConstants.USE_LONG_LINE] = 16000;
	}




	public static Document convertToXMLDOM(File file) 
	throws ParserException, LexerException, IOException, XMLStreamException {
		return convert(new FileReader(file), file.getName(), false, 
				Cb2xmlConstants.USE_PROPERTIES_FILE, FIRST_COBOL_COLUMN, LAST_COBOL_COLUMN, USE_DEFAULT_CB2XML_THREADSIZE);
		
	}


	// overloaded methods for debug mode
	public static Document convertToXMLDOM(File file, boolean debug) 
	throws ParserException, LexerException, IOException, XMLStreamException {
		//return convert(file, null, null, debug);
		return convert(new FileReader(file), file.getName(), debug,
				Cb2xmlConstants.USE_PROPERTIES_FILE, FIRST_COBOL_COLUMN, LAST_COBOL_COLUMN, USE_DEFAULT_CB2XML_THREADSIZE);
		
	}
	
	/*
	 * used by JRecord
	 */
	public static Document convertToXMLDOM(File file, boolean debug, int format) 
	throws ParserException, LexerException, IOException, XMLStreamException {
		return convert(file, null, null, debug, format, FIRST_COBOL_COLUMN, LAST_COBOL_COLUMN);
	}

	
	public static Document convertToXMLDOM(File file, boolean debug, int firstColumn, int lastColumn) 
	throws ParserException, LexerException, IOException, XMLStreamException {
		return convert(file, null, null, debug, Cb2xmlConstants.USE_SUPPLIED_COLUMNS, firstColumn,  lastColumn);
	}

	/**
	 * used by JRecord
	 * @Deprecated use reader instead of stream
	 */ 
	public static Document convertToXMLDOM(InputStream is, String name, boolean debug, int format) 
	throws ParserException, LexerException, IOException, XMLStreamException {
		return convert(null, is, name, debug, format, FIRST_COBOL_COLUMN, LAST_COBOL_COLUMN);
	}

	
	public static Document convertToXMLDOM(InputStream is, String name, boolean debug, int firstColumn, int lastColumn) 
	throws ParserException, LexerException, IOException, XMLStreamException {
		return convert(null, is, name, debug, Cb2xmlConstants.USE_SUPPLIED_COLUMNS, firstColumn,  lastColumn);
	}


	public static Document convertToXMLDOM(InputStream is, String name) 
	throws ParserException, LexerException, IOException, XMLStreamException { 
		return convert(null, is, name, false, Cb2xmlConstants.USE_PROPERTIES_FILE, FIRST_COBOL_COLUMN, LAST_COBOL_COLUMN);
	}

	// overloaded methods for debug mode
	public static Document convertToXMLDOM(InputStream is, String name, boolean debug) 
	throws ParserException, LexerException, IOException, XMLStreamException {
		//return convert(null, is, name, debug);
		return convert(null, is, name, debug, Cb2xmlConstants.USE_PROPERTIES_FILE, FIRST_COBOL_COLUMN, LAST_COBOL_COLUMN);
	}



	public static String convertToXMLString(Document document) {
		return XmlUtils.domToString(document).toString();
	}

//	private static Document convert(File file, InputStream is, String copybookName, boolean debug) throws ParserException, LexerException, IOException {
//		return convert(file, is, copybookName, debug, Cb2xmlConstants.USE_PROPERTIES_FILE, FIRST_COBOL_COLUMN, LAST_COBOL_COLUMN);
//	}
	
	public static Document convert(File file, InputStream is, String copybookName, boolean debug, int format, int firstColumn, int lastColumn) 
			throws ParserException, LexerException, IOException, XMLStreamException {

		String name = copybookName;

		Reader sr;
		
		if (is == null) {			
			sr = new FileReader(file);
			name = file.getName();
		} else {
			sr = new InputStreamReader(is);
		}
		
		return convert(sr, name, debug, format, firstColumn, lastColumn, USE_DEFAULT_CB2XML_THREADSIZE);
	}
	
	/*
	 * Used by JRecord
	 */
	public static Document convert(Reader r, String copybookName, boolean debug, int format) 
	throws ParserException, LexerException, IOException, XMLStreamException {
		return convert(r, copybookName, debug, format,
				FIRST_COBOL_COLUMN, LAST_COBOL_COLUMN, USE_DEFAULT_CB2XML_THREADSIZE);
	}

	public static Document convert(
			Reader r, String copybookName, boolean debug, 
			int format, int firstColumn, int lastColumn,
			long threadSize) 
	throws ParserException, LexerException, IOException, XMLStreamException {
		
		ICb2XmlBuilder bldr = Cb2Xml3.newBuilder(r, copybookName)
					.setDebug(debug)
					.setXmlFormat(Cb2xmlConstants.Cb2xmlXmlFormat.CLASSIC)
					.setStackSize(threadSize);
		
		switch (format) {
		case Cb2xmlConstants.USE_SUPPLIED_COLUMNS:
			bldr.setCobolColumns(firstColumn, lastColumn);
			break;

		default:
			bldr.setCobolLineFormat(format);
			break;
		}
		
		return bldrToDocument(bldr);

//		DoCblAnalyse da = new DoCblAnalyse(copybookName);
//		
//		da.doAnalysis(debug, format, firstColumn, lastColumn, r, threadSize);
//		
//		return da.document;	
	}
	
	/**
	 * Convert a cb2xml-builder to a Document.
	 * @param bldr cb2xml-builder
	 * @return cb2xml-builder
	 */
	public static Document bldrToDocument(ICb2XmlBuilder bldr) throws XMLStreamException, LexerException, IOException, ParserException {
		Document doc = XmlUtils.getNewXmlDocument();
		
		bldr.writeXml(new DOMResult(doc));
		
		return doc;
	}
//
//	private static final class DoCblAnalyse extends DoCobolAnalyse {
//		
//		private String name;
//		Document document;
//		
//		public DoCblAnalyse(String name) {
//			super(name);
//			this.name = name;
//		}
//
//		protected void analyseCobolCopybook(Parser parser, Start ast) {
//			CopyBookAnalyzer copyBookAnalyzer = new CopyBookAnalyzer(name, parser);
//			ast.apply(copyBookAnalyzer);
//			document = copyBookAnalyzer.getDocument();
//		}
//	}
}