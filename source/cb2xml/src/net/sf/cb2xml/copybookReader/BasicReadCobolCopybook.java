package net.sf.cb2xml.copybookReader;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.Properties;

public class BasicReadCobolCopybook implements ICobolCopybookTextSource {

	private String copybookDetails;
	private int firstColumn;
	//private IReadLine readCopybook;
	private final Reader reader;
	private final String copybookName;
	
	public static BasicReadCobolCopybook newReaderUsePropertiesFile(Reader reader, String copybookName) throws IOException {
	  	int columnStart = 6;
	  	int columnEnd = 72;
	  	File properties = new File("cb2xml.properties");
	  	
	  	if (properties.exists()) {
		  	try {
		  	  	FileInputStream propsStream = new FileInputStream(properties);
			  	Properties props = new Properties();
			  	props.load(propsStream);
			  	propsStream.close();
			  	String columnStartProperty = props.getProperty("column.start");
			  	String columnEndProperty = props.getProperty("column.end");
			  	if (columnStartProperty != null) {
			  		columnStart = Integer.parseInt(columnStartProperty);
			  	}
			  	if (columnEndProperty != null) {
			  		columnEnd = Integer.parseInt(columnEndProperty);
			  	}
		  	} catch (Exception e) {
		  		e.printStackTrace();
		  	}
	  	} else {
	  		System.err.println("*** -------------------------------------------------------");
	  		System.err.println("*** Warning there was no \"cb2xml.properties\" file, ");
	  		System.err.println("*** using the default Cobol columns of 6 to 72 ");
	  		System.err.println("*** -------------------------------------------------------");
	  	}
	  	System.err.println("*** using start column = " + columnStart + ", end column = " + columnEnd);
	  	
		return new BasicReadCobolCopybook(reader, copybookName)
						.setColumns(columnStart, columnEnd);
	}
	
	public static BasicReadCobolCopybook newCopybookReader(String fileName) throws FileNotFoundException {
		return new BasicReadCobolCopybook(new FileReader(fileName), new File(fileName) .getName());
	}
	public static BasicReadCobolCopybook newCopybookReader(Reader reader, String copybookName) {
		return new BasicReadCobolCopybook(reader, copybookName);
	}
	
	public BasicReadCobolCopybook(Reader reader, String copybookName) {
		super();
		this.reader = reader;
		this.copybookName = copybookName;
	}
	
	public BasicReadCobolCopybook setColumns(int firstColumn, int lastColumn) throws IOException {
		IReadLine readCopybook = new ReadColumnFromLine(reader, firstColumn, lastColumn);
		
		this.firstColumn = firstColumn;
		StringBuilder b = new StringBuilder();
		String s;
		
		while ((s = readCopybook.readLine()) != null) {
			b.append(s).append('\n');
			//sep = "\n";
		}
		readCopybook.close();
		
		copybookDetails= b.toString();
		return this;
	}

	@Override
	public String getCopybookName() {
		return copybookName;
	}

	@Override
	public Reader getFreeFormatCopybookReader() {
		checkCopybook();
		return new StringReader(copybookDetails);
	}
	
	/**
	 * Get the copybook (as Free Format Text)
	 * @return the Copybook
	 */
	public String getFreeFormatCopybookText() {
		checkCopybook();
		return copybookDetails;

	}

	@Override
	public int length() {
		return copybookDetails.length();
	}

	@Override
	public String toPositionMessage(int lineNumber, int columnNumber) {
		checkCopybook();
		return "Line Number = " + lineNumber + ", Column = " + (columnNumber - firstColumn);
	}

	private void checkCopybook() {
		if (copybookDetails == null || copybookDetails.length() == 0) {
			throw new RuntimeException("No columns supplied");
		}
	}
}
 