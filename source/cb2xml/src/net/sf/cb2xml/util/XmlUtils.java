/*************************************************************
 * This file is part of CB2XML.  
 * See the file "LICENSE" for copyright information and the
 * terms and conditions for copying, distribution and
 * modification of CB2XML.
 *************************************************************
 */

package net.sf.cb2xml.util;

import java.io.File;
import java.io.StringWriter;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.w3c.dom.Document;

/**
* quick and easy XML utilities to ease the trouble of DOM parsing, XSL transformations
* or converting XML to Strings / StringBuffer and vice-versa
* @author Peter Thomas
*/

public class XmlUtils {

  public static Document getNewXmlDocument() {
    try {
      return DocumentBuilderFactory.newInstance().
          newDocumentBuilder().newDocument();
    }
    catch (Exception e) {
      e.printStackTrace();
      return null;
    }
  }

  public static StringBuffer domToString(Document document) {
  	if (document == null) {
  		return new StringBuffer();
  	}
    return transformToString(new DOMSource(document), null);
  }

  public static Document fileToDom(String fileName) {
    try {
      DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
      return factory.newDocumentBuilder().parse(new File(fileName));
    }
    catch (Exception e) {
      e.printStackTrace();
      return null;
    }
  }

  public static StringBuffer transformToString(Source xmlSource,
                                               Source xslSource) {
    StringWriter writer = new StringWriter();
    Transformer transformer;
    try {
      if (xslSource == null) {
        transformer = TransformerFactory.newInstance().newTransformer();
      }
      else {
        transformer = TransformerFactory.newInstance().newTransformer(xslSource);
      }
      transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
      transformer.transform(xmlSource, new StreamResult(writer));
      return writer.getBuffer();
    }
    catch (Exception e) {
      e.printStackTrace();
      return writer.getBuffer();
    }
    finally {
      try {
        writer.close();
      }
      catch (Exception e) {
        e.printStackTrace();
      }
    }
  }

  public static StringBuffer transformToString(String xmlFileName,
                                               String xslFileName) {
    return transformToString(new StreamSource(new File(xmlFileName)),
                             new StreamSource(new File(xslFileName)));
  }

  public static StringBuffer transformToString(Document document,
                                               String xslFileName) {
    return transformToString(new DOMSource(document),
                             new StreamSource(new File(xslFileName)));
  }


}