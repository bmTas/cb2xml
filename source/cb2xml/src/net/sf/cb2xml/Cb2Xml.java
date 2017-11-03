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
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;

import javax.xml.stream.XMLStreamException;

import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;
import net.sf.cb2xml.util.Parms;

import org.w3c.dom.Document;

/**
 * main utility for parsing a copybook into XML
 * calls pre-processor, then parser to perform parse
 * note the debug mode to view detailed SableCC output
 *
 * @author Peter Thomas
 * 
 * @version 0.95 Bruce Martin
 * <ul>
 *   <li>Added the option of using -xml -debug etc tags
 *   <li>Calling Cb2xml2 to create the DOM
 * </ul>
* @version 0.96 Bruce Martin (01.Sep.2017)
* Changed to use Cb2xml3 to do the conversion
* @version 1.0 Use Cb2xml3
 */

public class Cb2Xml {
	
	private static final int FIRST_COBOL_COLUMN = 6;
	private static final int LAST_COBOL_COLUMN = 72;
	
	public static void main(String[] args) {
		Parms p = new Parms(args);
		
		if (p.ok) {
			try {
				Reader r;
				if (p.font == null || p.font.length() == 0) {
					r = new FileReader(p.cobol);
				} else {
					r =  new InputStreamReader(new FileInputStream(p.cobol), p.font);
				}
				ICb2XmlBuilder bldr
						= Cb2Xml3.newBuilder(r, new File(p.cobol).getName())
								 .setDebug(p.debug)
								 .setIndent(p.indentXml)
								 .setXmlFormat(p.xmlFormat)
								 .setXmlEncoding(p.font)
								 .setDialect(p.dialect);
				bldr.setStackSize(p.stackSize);
				
				if (p.xml == null || p.xml.length() == 0) {
					bldr.setXmlEncoding("").writeXml(new OutputStreamWriter(System.out));
				} else {
					bldr.writeXml(p.xml);
				}
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

			
			//			File cobolFile = new File(p.cobol);
//
//			Document document = null;
//			if (p.font == null || p.font.length() == 0) {
//				document = convert(cobolFile, null, "", p.debug);			
//			} else {
//				try {
//					Reader r = new InputStreamReader(new FileInputStream(cobolFile), p.font);
//					document = Cb2Xml2.convert(r, cobolFile.getName(), p.debug, Cb2xmlConstants.USE_PROPERTIES_FILE);
//					r.close();
//				} catch (DebugParserException dpe) {
//					System.err.println("*** fatal parse error ***");
//					System.err.println(dpe.getMessage());
//					System.err.println("=== buffer dump start ===");
//					System.err.println(dpe.buffer);
//					System.err.println("=== buffer dump end ===");
//
//				} catch (ParserException pe) {
//					System.err.println("*** fatal parse error ***");
//					System.err.println(pe.getMessage());
//					
//				} catch (Exception e) {
//					e.printStackTrace();
//				}
//
//			}
//			
//			if (p.xml == null || p.xml.length() == 0) {
//				XmlUtils.writeDocument(System.out, document, p.indentXml);
//			} else {
//				XmlUtils.writeDocument(p.xml, document, p.indentXml);
//			}
		}
	}

	// public API methods
	public static Document convertToXMLDOM(File file) {
		return convert(file, null, null, false);
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


	
	public static String convertToXMLString(File file) {
		return convertToXMLString(file, false);
	}

	public static String convertToXMLString(File file, boolean debug) {
			try {
				return Cb2Xml3.newBuilder(file)
							.setDebug(debug)
							.setXmlFormat(Cb2xmlConstants.Cb2xmlXmlFormat.CLASSIC)
							.asXmlString();
			} catch (XMLStreamException e) {
				throw new RuntimeException(e);
			} catch (LexerException e) {
				throw new RuntimeException(e);
			} catch (IOException e) {
				throw new RuntimeException(e);
			} catch (ParserException e) {
				throw new RuntimeException(e);
			}
	}

	public static String convertToXMLString(InputStream is, String name) {
		return convertToXMLString(is, name, false);
	}

	public static String convertToXMLString(InputStream is, String name, boolean debug) {
		try {
			return Cb2Xml3.newBuilder(new InputStreamReader(is), name)
						.setDebug(debug)
						.setXmlFormat(Cb2xmlConstants.Cb2xmlXmlFormat.CLASSIC)
						.asXmlString();
		} catch (XMLStreamException e) {
			throw new RuntimeException(e);
		} catch (LexerException e) {
			throw new RuntimeException(e);
		} catch (IOException e) {
			throw new RuntimeException(e);
		} catch (ParserException e) {
			throw new RuntimeException(e);
		}
	}

	public static Document convertToXMLDOM(InputStream is, String name) {
		return convert(null, is, name, false);
	}

	// overloaded methods for debug mode
	public static Document convertToXMLDOM(InputStream is, String name, boolean debug) {
		return convert(null, is, name, debug);
	}



	private static Document convert(File file, InputStream is, String copybookName, boolean debug) {
		return convert(file, is, copybookName, debug, Cb2xmlConstants.USE_PROPERTIES_FILE, FIRST_COBOL_COLUMN, LAST_COBOL_COLUMN);
	}
	
	@SuppressWarnings("deprecation")
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
}
