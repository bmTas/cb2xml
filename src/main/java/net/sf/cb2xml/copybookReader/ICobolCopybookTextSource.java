package net.sf.cb2xml.copybookReader;

import java.io.Reader;

/**
 * Class to retrieve a COBOL copybook and copybook name.
 * 
 * @author Bruce Martin
 *
 */
public interface ICobolCopybookTextSource {

	/**
	 * @return the Cobol copybookName as supplied by the user
	 */
	String getCopybookName();

	/**
	 * </b> Current <i>Cobol-Copybook-Store</i> as a Reader
	 * 
	 * <p><b>Usage:</b>
	 * <pre>
	 *	String xml = Cb2Xml3.newBuilder(readCobolCopybook.getFreeFormatCopybookReader(), "Main01")
	 *			.setCobolLineFormat(Cb2xmlConstants.FREE_FORMAT)
	 *			.setIndent(true)
	 *			.asXmlString()
	 *
	 *</pre>
	 *</p>
	 * @return the Current <i>Cobol-Copybook-Store</i> as a Reader
	 * in Free format Text format  (i.e no Line numbers). In cb2xml/JRecord use the
	 * <b>Free Format</b> option (Cb2xmlConstants.FREE_FORMAT)
	 *  
	 */
	Reader getFreeFormatCopybookReader();

	/**
	 * @return
	 * @see java.lang.AbstractStringBuilder#length()
	 */
	int length();

	/**
	 * Convert a line/position in the expanded copybook store to a line-number in a copybook
	 * @param lineNumber line number in the combined Copybook
	 * @param columnNumber Column number in the free form text
	 * @return
	 */
	String toPositionMessage(int lineNumber, int columnNumber);

}