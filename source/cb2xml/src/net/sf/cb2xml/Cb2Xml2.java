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
import java.io.PushbackReader;
import java.io.Reader;
import java.io.StringReader;

import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.sablecc.lexer.Lexer;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.node.Start;
import net.sf.cb2xml.sablecc.parser.Parser;
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
 */

public class Cb2Xml2 {
	
	
	
	private static final int FIRST_COBOL_COLUMN = 6;
	private static final int LAST_COBOL_COLUMN = 72;
	
	private static int[] END_COLS = new int[Cb2xmlConstants.FREE_FORMAT];
	
	static {
		END_COLS[Cb2xmlConstants.USE_STANDARD_COLUMNS] = LAST_COBOL_COLUMN;
		END_COLS[Cb2xmlConstants.USE_COLS_6_TO_80] =  80;
		END_COLS[Cb2xmlConstants.USE_LONG_LINE] = 16000;
	}



	// public API methods
	public static Document convertToXMLDOM(File file) throws ParserException, LexerException, IOException {
		//return convert(file, is, copybookName, debug, Cb2xmlConstants.USE_PROPERTIES_FILE, FIRST_COBOL_COLUMN, LAST_COBOL_COLUMN);
		//return convert(file, null, null, false);
		return convert(new FileReader(file), file.getName(), false, Cb2xmlConstants.USE_PROPERTIES_FILE, FIRST_COBOL_COLUMN, LAST_COBOL_COLUMN);
		
	}


	// overloaded methods for debug mode
	public static Document convertToXMLDOM(File file, boolean debug) throws ParserException, LexerException, IOException {
		//return convert(file, null, null, debug);
		return convert(new FileReader(file), file.getName(), debug, Cb2xmlConstants.USE_PROPERTIES_FILE, FIRST_COBOL_COLUMN, LAST_COBOL_COLUMN);
		
	}
	
	public static Document convertToXMLDOM(File file, boolean debug, int format) throws ParserException, LexerException, IOException {
		return convert(file, null, null, debug, format, FIRST_COBOL_COLUMN, LAST_COBOL_COLUMN);
	}

	
	public static Document convertToXMLDOM(File file, boolean debug, int firstColumn, int lastColumn) throws ParserException, LexerException, IOException {
		return convert(file, null, null, debug, Cb2xmlConstants.USE_SUPPLIED_COLUMNS, firstColumn,  lastColumn);
	}

	/**
	 * @Deprecated use reader instead of stream
	 */ 
	public static Document convertToXMLDOM(InputStream is, String name, boolean debug, int format) throws ParserException, LexerException, IOException {
		return convert(null, is, name, debug, format, FIRST_COBOL_COLUMN, LAST_COBOL_COLUMN);
	}

	
	public static Document convertToXMLDOM(InputStream is, String name, boolean debug, int firstColumn, int lastColumn) throws ParserException, LexerException, IOException {
		return convert(null, is, name, debug, Cb2xmlConstants.USE_SUPPLIED_COLUMNS, firstColumn,  lastColumn);
	}


	public static Document convertToXMLDOM(InputStream is, String name) throws ParserException, LexerException, IOException { 
		return convert(null, is, name, false, Cb2xmlConstants.USE_PROPERTIES_FILE, FIRST_COBOL_COLUMN, LAST_COBOL_COLUMN);
	}

	// overloaded methods for debug mode
	public static Document convertToXMLDOM(InputStream is, String name, boolean debug) throws ParserException, LexerException, IOException {
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
			throws ParserException, LexerException, IOException {

		String name = copybookName;

		

		Reader sr;
		
		if (is == null) {			
//			switch (format) {
//			case Cb2xmlConstants.FREE_FORMAT:
//				sr = new FileReader(file);
//				initialColumn = 0;
//				break;
//			case Cb2xmlConstants.USE_STANDARD_COLUMNS:
//			case Cb2xmlConstants.USE_COLS_6_TO_80:
//			case Cb2xmlConstants.USE_LONG_LINE:
//				preProcessed = CobolPreprocessor.preProcess(new FileInputStream(file), FIRST_COBOL_COLUMN, END_COLS[format]);
//				sr = new StringReader(preProcessed);
//				break;
//			case Cb2xmlConstants.USE_SUPPLIED_COLUMNS:
//			    preProcessed = CobolPreprocessor.preProcess(new FileInputStream(file), firstColumn, lastColumn);
//			    sr = new StringReader(preProcessed);
//			    break;
//			default:
//				preProcessed = CobolPreprocessor.preProcess(new FileInputStream(file));
//				sr = new StringReader(preProcessed);
//				break;
//			}
			sr = new FileReader(file);
			name = file.getName();
		} else {
			sr = new InputStreamReader(is);
//			switch (format) {
//			case Cb2xmlConstants.FREE_FORMAT:
//				sr = new InputStreamReader(is);
//				initialColumn = 0;
//				break;
//			case Cb2xmlConstants.USE_STANDARD_COLUMNS:
//			case Cb2xmlConstants.USE_COLS_6_TO_80:
//			case Cb2xmlConstants.USE_LONG_LINE:
//				preProcessed = CobolPreprocessor.preProcess(is, FIRST_COBOL_COLUMN, END_COLS[format]);
//				sr = new StringReader(preProcessed);
//				break;
//			case Cb2xmlConstants.USE_SUPPLIED_COLUMNS:
//				preProcessed = CobolPreprocessor.preProcess(is, firstColumn, lastColumn);
//				sr = new StringReader(preProcessed);
//				break;
//			default:
//				preProcessed = CobolPreprocessor.preProcess(is);
//				sr = new StringReader(preProcessed);
//				break;
//			}
		}
		
		return convert(sr, name, debug, format, firstColumn, lastColumn);
//		PushbackReader pbr = new PushbackReader(sr, 1000);
//		Parser parser;
//		Start ast;
//		if (debug) {
//			System.err.println("*** debug mode ***");
//			DebugLexer lexer = new DebugLexer(pbr);
//			parser = new Parser(lexer);
//			try {
//				ast = parser.parse(initialColumn);
//			} catch (ParserException pe) {
//				StringBuffer buffer = lexer.getBuffer();
//				String s = "";
//				if (buffer != null) {
//					s = buffer.toString();
//				}
//				throw new DebugParserException(pe, s);
//			}
//		} else {
//			parser = new Parser(new Lexer(pbr));
//			ast = parser.parse(initialColumn);
//		}
//
//
//		CopyBookAnalyzer copyBookAnalyzer = new CopyBookAnalyzer(name, parser);
//		ast.apply(copyBookAnalyzer);
//		document = copyBookAnalyzer.getDocument();
//
//
//		return document;
	}
	
	public static Document convert(Reader r, String copybookName, boolean debug, int format) 
	throws ParserException, LexerException, IOException {
		return convert(r, copybookName, debug, format, 6, 72);
	}

	public static Document convert(Reader r, String copybookName, boolean debug, int format, int firstColumn, int lastColumn) 
	throws ParserException, LexerException, IOException {
		Document document = null;
		
		String preProcessed = null;
		String name = copybookName;
		int initialColumn=FIRST_COBOL_COLUMN;
		

		Reader sr;
		
//		if (is == null) {			
			switch (format) {
			case Cb2xmlConstants.FREE_FORMAT:
				sr = r;
				initialColumn = 0;
				break;
			case Cb2xmlConstants.USE_STANDARD_COLUMNS:
			case Cb2xmlConstants.USE_COLS_6_TO_80:
			case Cb2xmlConstants.USE_LONG_LINE:
				preProcessed = CobolPreprocessor.preProcess(r, FIRST_COBOL_COLUMN, END_COLS[format]);
				sr = new StringReader(preProcessed);
				break;
			case Cb2xmlConstants.USE_SUPPLIED_COLUMNS:
			    preProcessed = CobolPreprocessor.preProcess(r, firstColumn, lastColumn);
			    sr = new StringReader(preProcessed);
			    break;
			default:
				preProcessed = CobolPreprocessor.preProcess(r);
				sr = new StringReader(preProcessed);
				break;
			}
			//name = file.getName();
//		} else {
//			switch (format) {
//			case Cb2xmlConstants.FREE_FORMAT:
//				sr = new InputStreamReader(is);
//				initialColumn = 0;
//				break;
//			case Cb2xmlConstants.USE_STANDARD_COLUMNS:
//			case Cb2xmlConstants.USE_COLS_6_TO_80:
//			case Cb2xmlConstants.USE_LONG_LINE:
//				preProcessed = CobolPreprocessor.preProcess(is, FIRST_COBOL_COLUMN, END_COLS[format]);
//				sr = new StringReader(preProcessed);
//				break;
//			case Cb2xmlConstants.USE_SUPPLIED_COLUMNS:
//				preProcessed = CobolPreprocessor.preProcess(is, firstColumn, lastColumn);
//				sr = new StringReader(preProcessed);
//				break;
//			default:
//				preProcessed = CobolPreprocessor.preProcess(is);
//				sr = new StringReader(preProcessed);
//				break;
//			}
//		}
		PushbackReader pbr = new PushbackReader(sr, 1000);
		Parser parser;
		Start ast;
		if (debug) {
			System.err.println("*** debug mode ***");
			DebugLexer lexer = new DebugLexer(pbr);
			parser = new Parser(lexer);
			try {
				ast = parser.parse(initialColumn);
			} catch (ParserException pe) {
				StringBuffer buffer = lexer.getBuffer();
				String s = "";
				if (buffer != null) {
					s = buffer.toString();
				}
				throw new DebugParserException(pe, s);
			}
		} else {
			parser = new Parser(new Lexer(pbr));
			ast = parser.parse(initialColumn);
		}


		CopyBookAnalyzer copyBookAnalyzer = new CopyBookAnalyzer(name, parser);
		ast.apply(copyBookAnalyzer);
		document = copyBookAnalyzer.getDocument();


		return document;
	
	}

}