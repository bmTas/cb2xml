/*************************************************************
 * This file is part of CB2XML.
 * See the file "LICENSE" for copyright information and the
 * terms and conditions for copying, distribution and
 * modification of CB2XML.
 *************************************************************
 */

package net.sf.cb2xml;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.sablecc.parser.ParserException;
import net.sf.cb2xml.util.Parms;
import net.sf.cb2xml.util.XmlUtils;

import org.w3c.dom.Document;

/**
 * main utility for parsing a copybook into XML
 * calls pre-processor, then parser to perform parse
 * note the debug mode to view detailed SableCC debug output
 *
 * @author Peter Thomas
 */

public class Cb2Xml {
	
	private static final int FIRST_COBOL_COLUMN = 6;
	private static final int LAST_COBOL_COLUMN = 72;
	
//	private static int[] END_COLS = new int[Cb2xmlConstants.FREE_FORMAT];
//	
//	static {
//		END_COLS[Cb2xmlConstants.USE_STANDARD_COLUMNS] = LAST_COBOL_COLUMN;
//		END_COLS[Cb2xmlConstants.USE_COLS_6_TO_80] =  80;
//		END_COLS[Cb2xmlConstants.USE_LONG_LINE] = 16000;
//	}
//

	public static void main(String[] args) {
		Parms p = new Parms(args);
		
		if (p.ok) {
			File cobolFile = new File(p.cobol);

			Document document = null;
			if (p.font == null || p.font.length() == 0) {
				document = convert(cobolFile, null, "", p.debug);			
			} else {
				try {
					Reader r= new InputStreamReader(new FileInputStream(cobolFile), p.font);
					document = Cb2Xml2.convert(r, cobolFile.getName(), p.debug, Cb2xmlConstants.USE_PROPERTIES_FILE);
					r.close();
				} catch (DebugParserException dpe) {
					System.err.println("*** fatal parse error ***");
					System.err.println(dpe.getMessage());
					System.err.println("=== buffer dump start ===");
					System.err.println(dpe.buffer);
					System.err.println("=== buffer dump end ===");

				} catch (ParserException pe) {
					System.err.println("*** fatal parse error ***");
					System.err.println(pe.getMessage());
					
				} catch (Exception e) {
					e.printStackTrace();
				}

			}
			
			if (p.xml == null || p.xml.length() == 0) {
				XmlUtils.writeDocument(System.out, document, p.indentXml);
			} else {
				XmlUtils.writeDocument(p.xml, document, p.indentXml);
			}
		}
//        if (args.length < 1) {
//            System.err.println("Usage:\tcb2xml <copybookFileName> [debug]");
//            return;
//        }
//
//		File file = new File(args[0]);
//		boolean debug = false;
//		if (args.length > 1) {
//			debug = true;
//		}
//		String result = convertToXMLString(file, debug);
//        System.out.println(result);
	}

	// public API methods
	public static Document convertToXMLDOM(File file) {
		return convert(file, null, null, false);
	}

	
	public static String convertToXMLString(File file) {
		Document document = convert(file, null, null, false);
		return XmlUtils.domToString(document).toString();
	}

	// overloaded methods for debug mode
	public static Document convertToXMLDOM(File file, boolean debug) {
		return convert(file, null, null, debug);
	}
	
	public static Document convertToXMLDOM(File file, boolean debug, int format) {
		return convert(file, null, null, debug, format, FIRST_COBOL_COLUMN, LAST_COBOL_COLUMN);
	}

	
	public static Document convertToXMLDOM(File file, boolean debug, int firstColumn, int lastColumn) {
		return convert(file, null, null, debug, Cb2xmlConstants.USE_SUPPLIED_COLUMNS, firstColumn,  lastColumn);
	}


	public static Document convertToXMLDOM(InputStream is, String name, boolean debug, int format) {
		return convert(null, is, name, debug, format, FIRST_COBOL_COLUMN, LAST_COBOL_COLUMN);
	}

	
	public static Document convertToXMLDOM(InputStream is, String name, boolean debug, int firstColumn, int lastColumn) {
		return convert(null, is, name, debug, Cb2xmlConstants.USE_SUPPLIED_COLUMNS, firstColumn,  lastColumn);
	}


//	public static Document convertToXMLDOM(File file, boolean debug, NumericDefinition numericDef) {
//		return convert(file, debug, numericDef);
//	}

	public static String convertToXMLString(File file, boolean debug) {
		Document document = convert(file, null, null, debug);
		return XmlUtils.domToString(document).toString();
	}

	public static Document convertToXMLDOM(InputStream is, String name) {
		return convert(null, is, name, false);
	}

	public static String convertToXMLString(InputStream is, String name) {
		Document document = convert(null, is, name, false); 
		return XmlUtils.domToString(document).toString();
	}

	// overloaded methods for debug mode
	public static Document convertToXMLDOM(InputStream is, String name, boolean debug) {
		return convert(null, is, name, debug);
	}


	public static String convertToXMLString(InputStream is, String name, boolean debug) {
		Document document = convert(null, is, name, debug);
		return XmlUtils.domToString(document).toString();
	}


	private static Document convert(File file, InputStream is, String copybookName, boolean debug) {
		return convert(file, is, copybookName, debug, Cb2xmlConstants.USE_PROPERTIES_FILE, FIRST_COBOL_COLUMN, LAST_COBOL_COLUMN);
	}
	
	public static Document convert(File file, InputStream is, String copybookName, boolean debug, int format, int firstColumn, int lastColumn) {
		Document document = null;
		try {
			document =  Cb2Xml2.convert(file, is, copybookName, debug, format, firstColumn, lastColumn);
			
		} catch (DebugParserException dpe) {
			System.err.println("*** fatal parse error ***");
			System.err.println(dpe.getMessage());
			System.err.println("=== buffer dump start ===");
			System.err.println(dpe.buffer);
			System.err.println("=== buffer dump end ===");

		} catch (ParserException pe) {
			System.err.println("*** fatal parse error ***");
			System.err.println(pe.getMessage());
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return document;
	}
	
//	public static Document convert(File file, InputStream is, String copybookName, boolean debug, int format, int firstColumn, int lastColumn) {
//		Document document = null;
//		Lexer lexer = null;
//		String preProcessed = null;
//		String name = copybookName;
//		int initialColumn=FIRST_COBOL_COLUMN;
//		
//		try {
//			Reader sr;
//			
//			if (is == null) {
//				switch (format) {
//				case Cb2xmlConstants.FREE_FORMAT:
//					sr = new FileReader(file);
//					initialColumn = 0;
//					break;
//				case Cb2xmlConstants.USE_STANDARD_COLUMNS:
//				case Cb2xmlConstants.USE_COLS_6_TO_80:
//				case Cb2xmlConstants.USE_LONG_LINE:
//					preProcessed = CobolPreprocessor.preProcess(file, FIRST_COBOL_COLUMN, END_COLS[format]);
//					sr = new StringReader(preProcessed);
//					break;
//				default:
//					preProcessed = CobolPreprocessor.preProcess(file);
//					sr = new StringReader(preProcessed);
//					break;
//				}
//				name = file.getName();
//			} else {
//				switch (format) {
//				case Cb2xmlConstants.FREE_FORMAT:
//					sr = new InputStreamReader(is);
//					initialColumn = 0;
//					break;
//				case Cb2xmlConstants.USE_STANDARD_COLUMNS:
//				case Cb2xmlConstants.USE_COLS_6_TO_80:
//				case Cb2xmlConstants.USE_LONG_LINE:
//					preProcessed = CobolPreprocessor.preProcess(is, FIRST_COBOL_COLUMN, END_COLS[format]);
//					sr = new StringReader(preProcessed);
//					break;
//				default:
//					preProcessed = CobolPreprocessor.preProcess(is);
//					sr = new StringReader(preProcessed);
//					break;
//				}
//			}
//			PushbackReader pbr = new PushbackReader(sr, 1000);
//			if (debug) {
//				System.err.println("*** debug mode ***");
//				lexer = new DebugLexer(pbr);
//			} else {
//				lexer = new Lexer(pbr);
//			}
//			Parser parser = new Parser(lexer);
//			Start ast = parser.parse(initialColumn);
//			CopyBookAnalyzer copyBookAnalyzer = new CopyBookAnalyzer(name, parser);
//			ast.apply(copyBookAnalyzer);
//			document = copyBookAnalyzer.getDocument();
//		} catch (ParserException pe) {
//			System.err.println("*** fatal parse error ***");
//			System.err.println(pe.getMessage());
//			if (debug) {
//				System.err.println("=== buffer dump start ===");
//				System.err.println(((DebugLexer) lexer).getBuffer());
//				System.err.println("=== buffer dump end ===");
//			}
//		} catch (Exception e) {
//			e.printStackTrace();
//		}
//		return document;
//	}

}