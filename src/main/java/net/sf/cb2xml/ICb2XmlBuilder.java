package net.sf.cb2xml;

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
public interface ICb2XmlBuilder extends IBasicCb2XmlBuilder {

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

}
