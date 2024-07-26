package net.sf.cb2xml.copybookReader;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

/**
 * <b>Purpose:</b> Read one or more COBOL copybooks and expand basic <i>COBOL Copy</i>
 * statements (<i>Copy Replacing</i> is <b>not</b> supported).
 * 
 * <p.<b>Usage:</b>
 * <pre>
 *		ReadCobolCopybook copybook = new ReadCobolCopybook()
 *				.setDirectoriesToSearch("/home/bruce/work/Cobol/CopyBooks")
 *				.setColumns(CopybookColumns.STANDARD_COLUMNS)
 *				.addCobolCopybook("/home/bruce/work/Cobol/Main01.cbl")
 *				.setColumns(CopybookColumns.FREE_FORMAT)	// load following copybooks as freeformat	
 *				.addCobolCopybook("/home/bruce/work/Cobol/Main02.cbl")
 *				.addCobolCopybook(new StringReader("\n"
 *						+ "   03 Extra-Field-01   pic x(10)."
 *						+ "   03 Extra-Field-02   pic s9(7)v99 comp-3."))
 *				;
 *
 *		System.out.println(
 *			Cb2Xml3.newBuilder(copybook.getFreeFormatCopybookReader(), "Main01")
 *					.setCobolLineFormat(Cb2xmlConstants.FREE_FORMAT)
 *					.setIndent(true)
 *					.asXmlString()
 *				);
 * </pre></p>
 * 
 * @author Bruce Martin
 *
 */
public class ReadCobolCopybook implements ICobolCopybookTextSource {

	private StringBuilder copybookData = new StringBuilder();
	private String copybookName;
	
	private int 
			firstColumn = CopybookColumns.STANDARD_COLUMNS.firstColumn,
			lastColumn  = CopybookColumns.STANDARD_COLUMNS.lastColumn;
	private CopybookColumns copybookColumns = CopybookColumns.STANDARD_COLUMNS;
	private String copybookFilenameExtension = "cbl";
	
	private List<String> directoriesToSearch; // = new ArrayList<String>();
	private List<CopybookDtls> copybookDtls = new ArrayList<CopybookDtls>();
	private int lineNumber = 0, readerNumber = 1, freeFormatTextNumber=1;
	
	
	/**
	 * Set the exact Cobol Columns to be used. In the old standard for COBOL only Columns 6-72 are
	 * processed (and that is the default
	 * @param firstColumn First Column to be processed 
	 * @param lastColumn Last Column
	 * @return ReadCobolCopybook object so the user can chain more commands
	 */
	public ReadCobolCopybook setColumns(int firstColumn, int lastColumn) {
		this.firstColumn = firstColumn;
		this.lastColumn = lastColumn;
		this.copybookColumns = CopybookColumns.USE_SUPPLIED_COLUMNS;
		
		return this;
	}
	
	/**
	 * Set the Cobol Columns. In the old standard for COBOL only Columns 6-72 are
	 * processed
	 * @param copybookColumns Specify the format of the Cobol Copybook. Options include<ul>
	 * <li>CopybookColumns.STANDARD_COLUMNS Standard Cobol Columns (6-72)
	 * <li>CopybookColumns.FREE_FORMAT - free format text no specific columns
	 * </ul>
	 * @return ReadCobolCopybook object so the user can chain more commands
	 */
	public ReadCobolCopybook setColumns(CopybookColumns copybookColumns) {
		if (copybookColumns == CopybookColumns.USE_SUPPLIED_COLUMNS) {
			throw new RuntimeException("Do not use 'USE_SUPPLIED_COLUMNS', use setColumns(firstColumn, lastColumn) method");
		} else if (copybookColumns.useColumns) {
			this.firstColumn = copybookColumns.firstColumn;
			this.lastColumn = copybookColumns.lastColumn;
		} else if (copybookColumns == CopybookColumns.PROPERTIES_FILE) {
			this.firstColumn = CopybookColumns.STANDARD_COLUMNS.firstColumn;
			this.lastColumn = CopybookColumns.STANDARD_COLUMNS.lastColumn;
	  	  	try {
				FileInputStream propsStream = new FileInputStream("cb2xml.properties");
				Properties props = new Properties();
				props.load(propsStream);
				propsStream.close();
				String columnStartProperty = props.getProperty("column.start");
				String columnEndProperty = props.getProperty("column.end");
				if (columnStartProperty != null) {
					firstColumn = Integer.parseInt(columnStartProperty);
				}
				if (columnEndProperty != null) {
					lastColumn = Integer.parseInt(columnEndProperty);
				}
			} catch (NumberFormatException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		this.copybookColumns = copybookColumns;
		return this;
	}
	
	

	/**
	 * Set the file name extension to be added to the Copybook name specified in the b>Cobol COPY</b> statement
	 * if the copybook contains
	 * <pre>
	 * 
	 *        COPY ABC.
	 *        
	 * </pre>
	 * and the copy book file is <b>ABC.cbl</b> then set <i>copybookFilenameExtension</i> to <b>cbl</b><p/>
	 * 
	 *  
	 * @param copybookFilenameExtension the copybookFilenameExtension to set
	 * @return ReadCobolCopybook object so the user can chain more commands
	 */
	public ReadCobolCopybook setCopybookFilenameExtension(String copybookFilenameExtension) {
		this.copybookFilenameExtension = copybookFilenameExtension;
		
		return this;
	}

	/**
	 * Set the directories to be searched for Cobol Copybooks that are referenced in 
	 * in <b>Cobol COPY</b> statements
	 *  
	 * @param directoriesToSearch directories to search for Cobol Copybooks
	 * 
	 * @return ReadCobolCopybook object so the user can chain more commands
	 */
	public ReadCobolCopybook setDirectoriesToSearch(String... directoriesToSearch) {
		this.directoriesToSearch = Arrays.asList(directoriesToSearch);
		return this;
	}

	/**
	 * Set the directories to be searched for Cobol Copybooks that are referenced in 
	 * in <b>Cobol COPY</b> statements
	 *  
	 * @param directoriesToSearch directories to search for Cobol Copybooks
	 * 
	 * @return ReadCobolCopybook object so the user can chain more commands
	 */
	public ReadCobolCopybook setDirectoriesToSearch(List<String> directoriesToSearch) {
		this.directoriesToSearch = directoriesToSearch;
		return this;
	}
	
	/**
	 * Empty the <i>Cobol-Copybook-Store</i> the ReadCobolCopybook object can be reused
	 * to load new Cobol Copybooks
	 * 
	 * @return this ReadCobolCopybook object so the user can chain more commands
	 */
	public ReadCobolCopybook emptyCopybookData() {
		copybookData.setLength(0);
		copybookDtls.removeAll(copybookDtls);
		lineNumber = 0;
		readerNumber = 1;
		return this;
	}
	
	/**
	 * @return the Cobol copybookName as supplied by the user
	 */
	@Override
	public String getCopybookName() {
		if (copybookName ==null) { throw new RuntimeException("The Copybook name has not be set !!!");}
		return copybookName;
	}

	/**
	 * Set the Combined Cobol-Copybook-Name. By default it is the first 
	 * file read using the {@link #addCobolCopybook(String)}
	 * 
	 * @param copybookName the copybookName to set by the {@link addCopybook(String)}
	 * @return this ReadCobolCopybook object so the user can chain more commands
	 */
	public ReadCobolCopybook setCopybookName(String copybookName) {
		this.copybookName = copybookName;
		return this;
	}

	/**
	 * Add a Cobol Copybook file and expand <b>basic COBOL copy</b> statements to the
	 * <i>Cobol-Copybook-Store</i>. The COBOL <b>COPY Replacing</b> option is <b>not</b> supported.
	 * <p>This method can be called multiple times to merge multiple COBOL copybooks.
	 * @param copybookFileName copybook file to be read
	 * @return this ReadCobolCopybook object so the user can chain more commands
	 * @throws IOException any io exception that occurs during the read
	 */
	public ReadCobolCopybook addCobolCopybook(String copybookFileName) throws IOException {
		if (copybookName == null) {
			copybookName = new File(copybookFileName) .getName();
		}
		List<String> dirsToSearch = directoriesToSearch;
		if (dirsToSearch == null) {
			dirsToSearch = new ArrayList<String>(2);
			dirsToSearch.add(".");
			dirsToSearch.add(Paths.get(copybookFileName).getParent().toAbsolutePath().toString());
		}
		readACopybook(copybookFileName, copybookData, dirsToSearch);
		return this;
	}
	
	/**
	 * Add a Cobol Copybook and expand <b>basic COBOL copy</b> statements from a reader to the <i>Cobol-Copybook-Store</i>.
	 * The COBOL <b>COPY Replacing</b> option is <b>not</b> supported.
	 * <p>This method can be called multiple times to merge multiple COBOL copybooks.
	 * @param copybookReader where to read the Cobol Copybook from
	 * @return this ReadCobolCopybook object so the user can chain more commands
	 * @throws IOException any io exception that occurs during the read
	 */
	public ReadCobolCopybook addCobolCopybook(Reader copybookReader) throws IOException {
		readACopybook(copybookReader, copybookData, getDirsToSearch());
		return this;
	}

	/**
	 * @return
	 */
	protected List<String> getDirsToSearch() {
		List<String> dirsToSearch = directoriesToSearch;
		if (dirsToSearch == null) {
			dirsToSearch = new ArrayList<String>(2);
			dirsToSearch.add(".");
		}
		return dirsToSearch;
	}
	
	/**
	 * Add Free Format cobol text to the <i>Cobol Copybook Store</i>.
	 * 
	 *  <b>Usage:</b>
	 *  
	 *  <pre>
	 *   ReadCobolCopybook copybook = Cb2Xml3.newCobolCopybookReader()
	 *				.setColumns(CopybookColumns.STANDARD_COLUMNS)
	 *				.addCobolCopybook("/home/bruce/work/Cobol/Main01.cbl")
	 *              .addFreeFormatCobolText(""
	 *                  +  "    03 Extra-field-1    pic x(10).\n"   
	 *                  +  "    03 Extra-field-2    pic x(20).\n");
	 *  </pre>
	 * @param cobolText Cobol Field Definition
	 * @return this ReadCobolCopybook object so the user can chain more commands
	 * @throws IOException 
	 */
	public ReadCobolCopybook addFreeFormatCobolText(CharSequence cobolText) throws IOException {
		readCopybook(new StringReader(cobolText.toString()), CopybookColumns.FREE_FORMAT, 
				copybookData, getDirsToSearch(), 
				new CopybookDtls("FreeFormatText " + freeFormatTextNumber++, lineNumber, firstColumn));
		//copybookData.append(cobolText);
		return this;
	}
	
	
	/**
	 * 
	 * @return the Current <i>Cobol-Copybook-Store</i> as a String
	 * in Free format Text format (i.e no Line numbers)
	 */
	public String getFreeFormatCopybookText() {
		if (copybookData == null || copybookData.length() == 0) {
			throw new RuntimeException("You Have not read a Copybook Yet");
		}
//		for (CopybookDtls cd : copybookDtls) {
//			System.out.println(cd.copybookName + "\t" + cd.firstLine + "\t\t" + cd.firstColumn);
//		}
		return copybookData.toString();
	}
	
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
	@Override
	public Reader getFreeFormatCopybookReader() {
		return new StringReader(getFreeFormatCopybookText());
	}
	
	/**
	 * @return
	 * @see java.lang.AbstractStringBuilder#length()
	 */
	@Override
	public int length() {
		return copybookData.length();
	}

	/**
	 * Read a Copybook file
	 * @param filename filename of the Cobol copybook to be read
	 * @param copybook copybook data store
	 * @param dirsToSearch directories to search for Cobol copybooks
	 * @throws IOException
	 * @throws FileNotFoundException
	 */
	private void readACopybook( String filename, StringBuilder copybook, List<String> dirsToSearch)
			throws IOException, FileNotFoundException {
			readCopybook(new FileReader(filename), copybookColumns, copybook, dirsToSearch, new CopybookDtls(new File(filename).getName(), lineNumber, firstColumn));
	}

	/**
	 * Read a Copybook from a reader
	 * @param reader Cobol Copybook reader
	 * @param copybook copybook data store
	 * @param dirsToSearch directories to search for Cobol copybooks
	 * @throws IOException
	 * @throws FileNotFoundException
	 */
	private void readACopybook( Reader reader, StringBuilder copybook, List<String> dirsToSearch)
			throws IOException, FileNotFoundException {
		readCopybook(reader, copybookColumns, copybook, dirsToSearch, new CopybookDtls("Reader " + readerNumber++, lineNumber, firstColumn));
	}
	
	private void readCopybook(Reader reader, CopybookColumns cobolColumns, StringBuilder copybook, 
			List<String> dirsToSearch, CopybookDtls dtls) throws IOException {
		IReadLine copybookReader;
		String line, trimmedLine;
		CopybookDtls parent = copybookDtls.size() == 0 ? null : copybookDtls.get(copybookDtls.size() - 1);
		
		addCopybookDtls(dtls);
		if (cobolColumns == CopybookColumns.FREE_FORMAT) {
			copybookReader =  new FreeFormatReader(reader);
		} else {
			copybookReader= new ReadColumnFromLine(reader, firstColumn, lastColumn);
		}
		while ((line = copybookReader.readLine()) != null) {
			trimmedLine = line.trim();
			if (trimmedLine.toUpperCase().startsWith("COPY ")) {
				int pos = trimmedLine.indexOf('.');
				if (pos > 0) {
					trimmedLine = trimmedLine.substring(0, pos);
				}
				trimmedLine = trimmedLine.substring(5);
			
				for (String fn : dirsToSearch) {
					if (".".equals(fn)) {
						fn = FileSystems.getDefault().getPath(".").toAbsolutePath().toString();
					}
					if (fn.endsWith(".")) {
						fn = fn.substring(0, fn.length() - 1);
					}
					if ( ! (fn.endsWith("/") || fn.endsWith("\\"))) {
						fn = fn + "/" + trimmedLine;
					} else {
						fn = fn + trimmedLine;
					}
					if (copybookFilenameExtension != null && copybookFilenameExtension.length() > 0) {
						fn = fn + "." + copybookFilenameExtension;
					}
//					System.out.println(fn + " " + Files.exists(Paths.get(fn)));
					if (Files.exists(Paths.get(fn))) {
						readACopybook(fn, copybook, dirsToSearch);
//						copybook.append("\n");
					}
				}
			} else {
				lineNumber += 1;
				copybook.append(line).append('\n');
				//sep = "\n";
			}
		}
		lineNumber += 1;
		
		int idx = copybook.length() - 1;
		if (idx > 0 && copybook.charAt(idx) == '\n'  && copybook.charAt(idx-1) == '\n') {
			copybook.setLength(idx);
		}
		//copybook.append(sep);
		if (parent != null) {
			CopybookDtls next = new CopybookDtls(parent.copybookName, lineNumber, firstColumn);
			addCopybookDtls(next);
		}
	}

	/**
	 * @param next
	 */
	protected void addCopybookDtls(CopybookDtls next) {
		int lastIndex = copybookDtls.size() - 1;
		if (lastIndex >= 0 && copybookDtls.get(lastIndex).firstLine == lineNumber) {
			copybookDtls.set(lastIndex, next);
		} else {
			copybookDtls.add(next);
		}
	}
	
	/**
	 * Convert a line/position in the expanded copybook store to a line-number in a copybook
	 * @param lineNumber line number in the combined Copybook
	 * @param columnNumber Column number in the free form text
	 * @return
	 */
	@Override
	public String toPositionMessage(int lineNumber, int columnNumber) {
		CopybookDtls dtls;
		int idx = 0;
		
		if (copybookDtls == null || copybookDtls.size() == 0) {
			return " line-number=" + (lineNumber)
					+ " column-number=" + (columnNumber);
		} 
		
		do {
			dtls = copybookDtls.get(idx);
		} while (++idx < copybookDtls.size() && copybookDtls.get(idx).firstLine < lineNumber);
		
		return "Copybook: " + dtls.copybookName + " line-number=" + (lineNumber - dtls.firstLine)
				+ " column-number=" + (columnNumber + dtls.firstColumn);
	}


	
	private static class FreeFormatReader implements IReadLine {
		private final BufferedReader reader;

		public FreeFormatReader(Reader reader) {
			this.reader = new BufferedReader(reader);
		}

		/**
		 * @throws IOException
		 */
		@Override
		public void close() throws IOException {
			reader.close();
		}

		/**
		 * @return next line from the file
		 * @throws IOException
		 */
		@Override
		public String readLine() throws IOException {
			return reader.readLine();
		}
	}
	

	
	private static class CopybookDtls {
		final String copybookName;
		final int firstLine, firstColumn;
		
		
		public CopybookDtls(String copybookName, int firstLine, int firstColumn) {
			super();
			this.copybookName = copybookName;
			this.firstLine = firstLine;
			this.firstColumn = firstColumn;
		}

	}

//	public static void main(String[] args) {
//		String line = "      copy abc. *>";
//		String trimmedLine = line.trim();
//		if (trimmedLine.toUpperCase().startsWith("COPY ")) {
//			int pos = trimmedLine.indexOf('.');
//			if (pos > 0) {
//				trimmedLine = trimmedLine.substring(0, pos);
//			}
//			trimmedLine = trimmedLine.substring(5);
//		}
//		System.out.println(">>" + trimmedLine + "<<");
//	}
}
