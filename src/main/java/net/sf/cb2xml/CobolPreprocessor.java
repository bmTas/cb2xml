/*************************************************************
 * This file is part of CB2XML.  
 * See the file "LICENSE" for copyright information and the
 * terms and conditions for copying, distribution and
 * modification of CB2XML.
 *************************************************************
 */

package net.sf.cb2xml;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Properties;


/**
 * Very simple COBOL pre-processor that chops the left and right margins. 
 * Column start and end positions are configurable using a properties file. 
 * Linefeeds are retained as these are required by the main parser.
 * COBOL files typically contain some junk characters and 
 * comment indicators in the "margins" and this routine removes those. 
 * 
 * @author Peter Thomas
 * 
 * @deprecated has been replaced by {@link net.sf.cb2xml.copybookReader.ICobolCopybookTextSource} interface and its
 * implementations {@link net.sf.cb2xml.copybookReader.ReadCobolCopybook} and {@link net.sf.cb2xml.copybookReader.BasicReadCobolCopybook}
 */
public class CobolPreprocessor {

	  public static String preProcess(File file) {
		  try {
			  return preProcess(new FileReader(file));
		  } catch (Exception e) {
			  e.printStackTrace();
			  return null;
		 }
	  }
	  
	  public static String preProcess(File file, int columnStart, int columnEnd) {
		  try {
			  return preProcess(new FileReader(file), columnStart, columnEnd);
		  } catch (Exception e) {
			  e.printStackTrace();
			  return null;
		 }
	  }
	  
	  public static	 String preProcess(InputStream fis) {
		  return preProcess((new InputStreamReader(fis)));
	  }
	  
	  public static	 String preProcess(Reader r) {
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
	  	
	  	return preProcess(r, columnStart, columnEnd);
	  }
	  
	  public static	 String preProcess(InputStream fis, int columnStart, int columnEnd) {
		  return preProcess(new InputStreamReader(fis), columnStart, columnEnd);
	  }
	  
	  public static	 String preProcess(Reader r, int columnStart, int columnEnd) {
		  StringBuffer sb = new StringBuffer();
		  BufferedReader buffer;
		  String s;
		  int tabPos;
		    try {
			      buffer = new BufferedReader(r);
			      while ((s = buffer.readLine()) != null) {	      	
			      	if (s.length() > columnStart) {
			      		int thisColumnStart = columnStart;
			      		tabPos = s.indexOf('\t');
			      		if (tabPos >= 0 && tabPos <= columnStart ) {
			      			s = "        " + s.substring(tabPos + 1);
			      		}
				      	if (s.charAt(columnStart) == '/') {
				      		sb.append('*');
				      		thisColumnStart++;
				      	}	      		
			      		if (s.length() < columnEnd) {
			      			sb.append(s.substring(thisColumnStart));
			      		} else {
			      			sb.append(s.substring(thisColumnStart, columnEnd));
			      		}  	
			      	}
			        sb.append("\n");
			      }
			    }
			    catch (Exception e) {
			      e.printStackTrace();
			      return null;
			    }
			    finally {
			      if (r != null) {
			        try {
			          r.close();
			        } catch (IOException e) {}
			      }
			    }
			    return sb.toString();
	  }

}