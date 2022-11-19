package net.sf.cb2xml;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;

import javax.xml.stream.XMLStreamException;
import javax.xml.transform.Result;

import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.IBasicDialect;
import net.sf.cb2xml.def.ICopybookJrUpd;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;
import net.sf.cb2xml.walker.CobolCopybookWalker;

public interface IBasicCb2XmlBuilder {

	/**
	 * set debug option
	 * @param debug wether to run in debug mode
	 * @return cb2xml Builder for further update
	 */
	ICb2XmlBuilder setDebug(boolean debug);

	/**
	 * Indent the Xml. Only applicable when producing Xml
	 * 
	 * @param indent - wether to indent the xml.
	 * 
	 * @return builder for further updates
	 */
	ICb2XmlBuilder setIndent(boolean indent);

	/**
	 * Set the encoding to be used for the Xml Stream
	 * @param encoding encoding to be defined in the Xml
	 * 
	 * @return builder for further updates
	 */
	ICb2XmlBuilder setXmlEncoding(String encoding);

	/**
	 * Set the stack size 
	 * @param stackSize
	 */
	ICb2XmlBuilder setStackSize(long stackSize);

	/**
	 * Set the Xml formats
	 * @param xmlFormat Format of the generated Xml. Options include<ul>
	 * <ul>
	 *   <li><b>Cb2xmlConstants.Cb2xmlXmlFormat.CLASSIC</b> - classic original Xml format
	 *   <li><b>Cb2xmlConstants.Cb2xmlXmlFormat.FORMAT_2017</b> - new Xml format introduced in 2017.
	 *   For
	 * </ul> 
	 */
	ICb2XmlBuilder setXmlFormat(Cb2xmlConstants.Cb2xmlXmlFormat xmlFormat);

	/**
	 * Set the Cobol Dialect
	 * @param dialect Cobol Dialect, options are: <ul>
	 *  <li><b>DialectManager.MAINFRAME_COBOL</b>
	 *  <li><b>DialectManager.MAINFRAME_COBOL_64_BIT</b> for 64 bit pointers
	 *  <li><b>DialectManager.GNU_COBOL</b>
	 *  <li><b>DialectManager.FUJITSU_COBOL</b>
	 * </ul>
	 * @return builder for further updates
	 */
	ICb2XmlBuilder setDialect(IBasicDialect dialect);

	/**
	 * Set wether Cobol comments will loaded / Written as
	 * Xml-Comments
	 * @param loadComments load Cobol comments
	 * @return builder for further updates
	 */
	ICb2XmlBuilder setLoadComments(boolean loadComments);

	/**
	 * Return the Cobol copybook as an <i>Item Tree</i>.
	 * @return Cobol Item Tree 
	 */
	ICopybookJrUpd asCobolItemTree();

	/**
	 * Convert the Cobol-Copybook to a Xml and return it as a StringString
	 * @return Xml in String form
	 */
	String asXmlString() throws XMLStreamException, LexerException, IOException, ParserException;

	/**
	 * Write the Xml to a specified file
	 * @param filename
	 */
	void writeXml(String filename) throws XMLStreamException, LexerException, IOException, ParserException;

	/**
	 * Write Xml to a stream
	 */
	void writeXml(OutputStream out) throws XMLStreamException, LexerException, IOException, ParserException;

	/**
	 * Write the Cobol as a Xml item Tree.
	 * @param writer used to write the Xml
	 */
	void writeXml(Writer writer) throws XMLStreamException, LexerException, IOException, ParserException;

	/**
	 * Write the Xml to a Xml-Result
	 * 
	 * @param result Xml-Result to update
	 */
	void writeXml(Result result) throws XMLStreamException, LexerException, IOException, ParserException;

	/**
	 * Create a Cobol-Copybook-Walker. This class lets you walk the Cobol Copybook with 
	 * a supplied listner
	 * @return  Cobol-Copybook-Walker
	 */
	CobolCopybookWalker asCobolCopybookWalker();

}