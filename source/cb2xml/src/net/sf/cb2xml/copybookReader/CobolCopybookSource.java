package net.sf.cb2xml.copybookReader;

import java.io.Reader;
import java.io.StringReader;


/**
 * Basic Static implementation of {@link ICobolCopybookTextSource}
 * @author Bruce Martin
 *
 */
public class CobolCopybookSource implements ICobolCopybookTextSource {

	private final String cobolCopybookSource, copybookname;
	
	/**
	 * Basic Static implementation of {@link ICobolCopybookTextSource}
	 * @param copybookname Name of the copybook
	 * @param cobolCopybookSource Free form text of the copybook
	 */
	public CobolCopybookSource(String copybookname, String cobolCopybookSource) {
		super();
		this.copybookname = copybookname;
		this.cobolCopybookSource = cobolCopybookSource;
	}

	@Override
	public String getCopybookName() {
		return copybookname;
	}

	@Override
	public Reader getFreeFormatCopybookReader() {
		return new StringReader(cobolCopybookSource);
	}

	@Override
	public int length() {
		return cobolCopybookSource.length();
	}

	@Override
	public String toPositionMessage(int lineNumber, int columnNumber) {
		return "LineNumber: " + lineNumber + "\tColumn Number: " + columnNumber;
	}

}
