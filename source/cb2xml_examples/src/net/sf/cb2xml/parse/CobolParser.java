package net.sf.cb2xml.parse;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;

import javax.xml.bind.JAXBException;

import net.sf.cb2xml.Cb2Xml2;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.jaxb.Copybook;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;

/**
 * This class converts Cobol Copybooks (in various forms)
 * into Java classes
 * 
 * @author Bruce Martin
 *
 */
public class CobolParser extends XmlParser {

	/**
	 * Convert a Cobol Copybook file into a "Copybook" classes
	 * @param cobolCopybookFileName cobol Copybook file name
	 * @return Copybook definition this holds all the Cobol Copybook details in 
	 * easily access recursive Java class structure
	 * 
	 * @throws JAXBException
	 * @throws IOException
	 * @throws ParserException
	 * @throws LexerException
	 */
	public final Copybook parseCobol(String cobolCopybookFileName) throws JAXBException, IOException, ParserException, LexerException{
		return parseCobol(cobolCopybookFileName,  Cb2xmlConstants.USE_STANDARD_COLUMNS);
	}
	
	/**
	 * 
	 * Convert a Cobol Copybook file into a "Copybook" classes
	 * @param cobolCopybookFileName cobol Copybook file name
	 * @param cobolCopybookFormat Format of the cobol copybook, possible values:<ul>
	 * <li>Cb2xmlConstants.USE_STANDARD_COLUMNS - Standard Cobol columns (6 -> 71)
	 * <li>Cb2xmlConstants.USE_COLS_6_TO_80 - Columns 6 -> 80
	 * <li>Cb2xmlConstants.USE_LONG_LINE Very long line 6 --> to what ever
	 * <li>Cb2xmlConstants.USE_PROPERTIES_FILE use cb2xml properties file
	 * <li>Cb2xmlConstants.FREE_FORMAT free format cobol
	 * </ul>
	 * @return Copybook definition this holds all the Cobol Copybook details in 
	 * easily access recursive Java class structure
	 * 
	 * @throws JAXBException
	 * @throws IOException
	 * @throws ParserException
	 * @throws LexerException
	 */
	public final Copybook parseCobol(String cobolCopybookFileName, int cobolCopybookFormat) 
	throws JAXBException, IOException, ParserException, LexerException {
		return parse(
				 Cb2Xml2.convertToXMLDOM(new File(cobolCopybookFileName), false, cobolCopybookFormat)
		);
	} 
	
	/**
	 * Convert a cobol copybook (in the form of a stream) to the Copybook class
	 * 
	 * @param cobolCopybook cobolCopybook stream
	 * @param copybookName copybook name
	 * @return Copybook definition this holds all the Cobol Copybook details in 
	 * easily access recursive Java class structure
	 * 
	 * @throws JAXBException
	 * @throws IOException
	 * @throws ParserException
	 * @throws LexerException
	 */
	public final Copybook parseCobol(InputStream cobolCopybook, String copybookName) 
	throws JAXBException, IOException, ParserException, LexerException {
		return parseCobol(cobolCopybook, copybookName, Cb2xmlConstants.USE_STANDARD_COLUMNS);
	} 
	
	/**
	 * Convert a cobol copybook (in the form of a stream) to the Copybook class
	 * 
	 * @param cobolCopybook cobolCopybook stream
	 * @param copybookName copybook name
	 * @param cobolCopybookFormat Format of the cobol copybook, possible values:<ul>
	 * <li>Cb2xmlConstants.USE_STANDARD_COLUMNS - Standard Cobol columns (6 -> 71)
	 * <li>Cb2xmlConstants.USE_COLS_6_TO_80 - Columns 6 -> 80
	 * <li>Cb2xmlConstants.USE_LONG_LINE Very long line 6 --> to what ever
	 * <li>Cb2xmlConstants.USE_PROPERTIES_FILE use cb2xml properties file
	 * <li>Cb2xmlConstants.FREE_FORMAT free format cobol
	 * </ul>
	 * @return Copybook definition this holds all the Cobol Copybook details in 
	 * easily access recursive Java class structure
	 * 
	 * @throws JAXBException
	 * @throws IOException
	 * @throws ParserException
	 * @throws LexerException
	 */
	public final Copybook parseCobol(InputStream cobolCopybook, String copybookName, int cobolCopybookFormat) 
	throws JAXBException, IOException, ParserException, LexerException {
		return parse(
				 Cb2Xml2.convertToXMLDOM(cobolCopybook, copybookName, false, cobolCopybookFormat)
		);
	}
	
	/**
	 * 
	 * @param cobolCopybookReader Copybook reader
	 * @param copybookName copybookname
	 * @return Copybook definition this holds all the Cobol Copybook details in 
	 * easily access recursive Java class structure
	 * 
	 * @throws JAXBException
	 * @throws IOException
	 * @throws ParserException
	 * @throws LexerException
	 */
	public final Copybook parseCobol(Reader cobolCopybookReader, String copybookName) 
	throws JAXBException, IOException, ParserException, LexerException {
		return parseCobol(cobolCopybookReader, copybookName,  Cb2xmlConstants.USE_STANDARD_COLUMNS);
	}

	/**
	 * 
	 * @param cobolCopybookReader Copybook reader
	 * @param copybookName copybookname
	 * @param cobolCopybookFormat Format of the Copybook
	 * @return Copybook definition this holds all the Cobol Copybook details in 
	 * easily access recursive Java class structure
	 * 
	 * @throws JAXBException
	 * @throws IOException
	 * @throws ParserException
	 * @throws LexerException
	 */
	public final Copybook parseCobol(Reader cobolCopybookReader, String copybookName, int cobolCopybookFormat) 
	throws JAXBException, IOException, ParserException, LexerException {
		return parse(
				 Cb2Xml2.convert(cobolCopybookReader, copybookName, false, cobolCopybookFormat)
		);
	}

	/**
	 * Create a Cobol-copybook parser
	 * @return  Cobol-copybook parser
	 */
	public static CobolParser newParser() {
		return new CobolParser();
	}

}
