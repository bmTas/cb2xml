/*************************************************************
 * This file is part of CB2XML.  
 * See the file "LICENSE" for copyright information and the
 * terms and conditions for copying, distribution and
 * modification of CB2XML.
 *************************************************************
 */

package net.sf.cb2xml.convert;

import net.sf.cb2xml.util.FileUtils;
import net.sf.cb2xml.util.XmlUtils;

import org.w3c.dom.Document;

/**
 * utility that demonstrates how to convert an XML data file into its mainframe (COBOL) equivalent
 * the copybook (in its XML form) is required for the conversion of course
 * 
 * note that files within the "net.sf.cb2xml.convert" package are not stable
 * 
 * @author Peter Thomas
 */

public class Convert {
  public static void main(String[] args) {
    Document copyBookXml = XmlUtils.fileToDom(args[1]);
    Document sourceDocument = XmlUtils.fileToDom(args[0]);
    // params 1) XML source document 2) Copybook as XML
    String result = new XmlToMainframe().convert(sourceDocument, copyBookXml);
    FileUtils.writeFile(result, "mfresult.txt", false);
  }
}