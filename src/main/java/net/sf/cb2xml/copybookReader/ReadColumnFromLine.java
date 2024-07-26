package net.sf.cb2xml.copybookReader;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;

public class ReadColumnFromLine implements IReadLine {
	private final BufferedReader reader;
	final int firstColumn, lastColumn;

	public ReadColumnFromLine(Reader reader, int firstColumn, int lastColumn) {
		
		this.reader = new BufferedReader(reader);
		this.firstColumn = firstColumn;
		this.lastColumn = lastColumn;
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
		String line = reader.readLine();
		
		if (line == null) {
			return null;
		}
      	if (line.length() <= firstColumn) {
      		return "";
      	}
		StringBuffer sb = new StringBuffer();
  		int thisColumnStart = firstColumn;
  		int tabPos = line.indexOf('\t');
  		if (tabPos >= 0 && tabPos <= firstColumn ) {
  			line = "        " + line.substring(tabPos + 1);
  		}
      	if (line.charAt(firstColumn) == '/') {
      		sb.append('*');
      		thisColumnStart++;
      	}	      		
  		if (line.length() < lastColumn) {
  			sb.append(line.substring(thisColumnStart));
  		} else {
  			sb.append(line.substring(thisColumnStart, lastColumn));
  		}  	
      	
		return sb.toString();
	}

}
