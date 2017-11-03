package net.sf.cb2xml;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;

import javax.xml.stream.XMLStreamException;
import javax.xml.transform.Result;

import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.ICopybook;
import net.sf.cb2xml.def.IBasicDialect;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;


/**
 * Main program interface to cb2xml. It is builder style interface
 * with
 * <ol>
 *   <li>set.. methods (e.g. setCobolLineFormat, setCobolColumns) for updating options.
 *   <li>Several output methods (asCobolItemTree(), asXmlString writeXml)
 * </ol>
 * @author Bruce Martin
 *
 */
public interface ICb2XmlBuilder {

	/**
	 * set debug option
	 * @param debug wether to run in debug mode
	 * @return cb2xml Builder for further update
	 */
	public ICb2XmlBuilder setDebug(boolean debug); 
	
	/**
	 * Define the format of the copybook
	 * @param format Copybook Format; possible values include:<ul>
	 *   <li><b>Cb2xmlConstants.FREE_FORMAT</b> Free format copybook
	 *   <li><b>Cb2xmlConstants.USE_STANDARD_COLUMNS</b> - Standard Cobol Columns
	 *   <li><b>Cb2xmlConstants.USE_COLS_6_TO_80</b> - use columns 6 -> 80
	 *   <li><b>Cb2xmlConstants.USE_LONG_LINE</b> - Long line starting at column 6
	 * </ul>
	 * 
	 * @return builder for further updates
	 */
	public ICb2XmlBuilder setCobolLineFormat(int format);
	
	/**
	 * Define the Cobol Columns
	 * @param startingColumn Column cobol starts in
	 * @param lastColumn last column to use
	 * @return builder for further updates
	 */
	public ICb2XmlBuilder setCobolColumns(int startingColumn, int lastColumn);
	
	/**
	 * Indent the Xml. Only applicable when producing Xml
	 * 
	 * @param indent - wether to indent the xml.
	 * 
	 * @return builder for further updates
	 */
	public ICb2XmlBuilder setIndent(boolean indent);

	/**
	 * Set the encoding to be used for the Xml Stream
	 * @param encoding encoding to be defined in the Xml
	 * 
	 * @return builder for further updates
	 */
	public ICb2XmlBuilder setXmlEncoding(String encoding);

	/**
	 * Set the stack size 
	 * @param stackSize
	 */
	public ICb2XmlBuilder setStackSize(long stackSize);

	/**
	 * Set the Xml formats
	 * @param xmlFormat Format of the generated Xml. Options include<ul>
	 * <ul>
	 *   <li><b>Cb2xmlConstants.Cb2xmlXmlFormat.CLASSIC</b> - classic original Xml format
	 *   <li><b>Cb2xmlConstants.Cb2xmlXmlFormat.FORMAT_2017</b> - new Xml format introduced in 2017.
	 *   For
	 * </ul> 
	 */
	public ICb2XmlBuilder setXmlFormat(Cb2xmlConstants.Cb2xmlXmlFormat xmlFormat);

	/**
	 * Set the Cobol Dialect
	 * @param dialect Cobol Dialect, options are: <ul>
	 *  <li><b>DialectManager.MAINFRAME_COBOL</b>
	 *  <li><b>DialectManager.GNU_COBOL</b>
	 *  <li><b>DialectManager.FUJITSU_COBOL</b>
	 * </ul>
	 * @return builder for further updates
	 */
	public ICb2XmlBuilder setDialect(IBasicDialect dialect);

	/**
	 * Set wether Cobol comments will loaded / Written as
	 * Xml-Comments
	 * @param loadComments load Cobol comments
	 * @return builder for further updates
	 */
	public ICb2XmlBuilder setLoadComments(boolean loadComments);
	
	/**
	 * Return the Cobol copybook as an <i>Item Tree</i>.
	 * @return Cobol Item Tree 
	 */
	public ICopybook asCobolItemTree();
	
	/**
	 * Convert the Cobol-Copybook to a Xml and return it as a StringString
	 * @return Xml in String form
	 */
	public String asXmlString() throws XMLStreamException, LexerException, IOException, ParserException;
	/**
	 * Write the Xml to a specified file
	 * @param filename
	 */
	public void writeXml(String filename) throws XMLStreamException, LexerException, IOException, ParserException;

	/**
	 * Write Xml to a stream
	 */
	public void writeXml(OutputStream out) throws XMLStreamException, LexerException, IOException, ParserException;
	
	/**
	 * Write the Cobol as a Xml item Tree.
	 * @param writer used to write the Xml
	 */
	public void writeXml(Writer writer) throws XMLStreamException, LexerException, IOException, ParserException;

	/**
	 * Write the Xml to a Xml-Result
	 * 
	 * @param result Xml-Result to update
	 */
	public void writeXml(Result result) throws XMLStreamException, LexerException, IOException, ParserException;

}
