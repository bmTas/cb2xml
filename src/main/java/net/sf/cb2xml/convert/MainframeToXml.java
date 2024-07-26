/*************************************************************
 * This file is part of CB2XML.  
 * See the file "LICENSE" for copyright information and the
 * terms and conditions for copying, distribution and
 * modification of CB2XML.
 *************************************************************
 */

package net.sf.cb2xml.convert;

import java.io.BufferedReader;
import java.io.Reader;
import java.io.StringReader;

import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.util.XmlUtils;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

/**
* routines to convert a copybook equivalent mainframe buffer into its XML form
* given the XML form of the copybook
* to-do: all the stuff related to COMP fields, packed decimal amd all other non-straightforward field types
* 
*  * note that files within the "net.sf.cb2xml.convert" package are not stable
* 
* @author Peter Thomas
*/

public class MainframeToXml {

  private String mainframeBuffer = null;
  private Document resultDocument = null;

  private static String stripNullChars(String in) {
    try {
      Reader reader = new BufferedReader(new StringReader(in));
      StringBuffer buffer = new StringBuffer();
      int ch;
      while ( (ch = reader.read()) > -1) {
        if (ch != 0) {
          buffer.append( (char) ch);
        } else {
          buffer.append(' ');
        }
      }
      reader.close();
      return buffer.toString();
    }
    catch (Exception e) {
      e.printStackTrace();
      return null;
    }
  }

  public Document convert(String mainframeTextBuffer, Document copyBookXml) {
    this.mainframeBuffer = stripNullChars(mainframeTextBuffer);
    this.resultDocument = XmlUtils.getNewXmlDocument();
    int bufferLength = mainframeTextBuffer.length();
    Element documentElement = copyBookXml.getDocumentElement();
    //Element recordNode = (Element) documentElement.
    //                                     getFirstChild();
    
    Element recordNode = Utils.getFirstElement(documentElement);
    Element resultRoot = resultDocument.createElement(documentElement.getTagName());
    int recordLength = Integer.parseInt(recordNode.getAttribute(Cb2xmlConstants.STORAGE_LENGTH));

    for (int offset = 0; offset < bufferLength; offset += recordLength) {
        Element resultTree = convertNode(recordNode, offset);
        resultRoot.appendChild(resultTree);
    }
    resultDocument.appendChild(resultRoot);
    return resultDocument;
  }

  private Element convertNode(Element element, int offset) {
    String resultElementName = element.getAttribute(Cb2xmlConstants.NAME);
    Element resultElement = resultDocument.createElement(resultElementName);
    int position = Integer.parseInt(element.getAttribute(Cb2xmlConstants.POSITION));
    int length = Integer.parseInt(element.getAttribute(Cb2xmlConstants.STORAGE_LENGTH));
    boolean numeric = Cb2xmlConstants.TRUE.equalsIgnoreCase(element.getAttribute(Cb2xmlConstants.NUMERIC));
    int childElementCount = 0;
    NodeList nodeList = element.getChildNodes();
    for (int i = 0; i < nodeList.getLength(); i++) {
      org.w3c.dom.Node node = nodeList.item(i);
      if (node.getNodeType() == org.w3c.dom.Node.ELEMENT_NODE) {
        Element childElement = (Element) node;
        if (!childElement.getAttribute(Cb2xmlConstants.LEVEL).equals("88")) {
          childElementCount++;
          if (childElement.hasAttribute(Cb2xmlConstants.OCCURS)) {
            int childOccurs = Integer.parseInt(childElement.getAttribute(Cb2xmlConstants.OCCURS));
//            int childPosition = Integer.parseInt(childElement.getAttribute(
//                Cb2xmlAttributes.POSITION));
            int childLength = Integer.parseInt(childElement.getAttribute(
                Cb2xmlConstants.STORAGE_LENGTH));
            int singleChildLength = childLength; // / childOccurs; // possible error - bug 10 occurs error
            for (int j = 0; j < childOccurs; j++) {
              resultElement.appendChild(
            		  convertNode(
            				  childElement,
            				  /*childPosition +*/offset + j * singleChildLength));
            }
          }
          else {
            resultElement.appendChild(convertNode(childElement, offset));
          }
        }
      }
    }
    if (childElementCount == 0) {  
      if (offset > 0) {
        position = position + offset;
      }
      String text = null;
      try {
        text = mainframeBuffer.substring(position - 1, position + length - 1);
        if (numeric) {
        	text = text.trim();
        }
      }
      catch (Exception e) {
        System.err.println(e);
        System.err.println("element = " + element.getAttribute(Cb2xmlConstants.NAME));
        System.err.println("position = " + position);
        System.err.println("length = " + length);
        System.err.println("Mainframe buffer length = " +
                           mainframeBuffer.length());
      }
      Text textNode = resultDocument.createTextNode(text);
      resultElement.appendChild(textNode);
      //resultElement.setAttribute(Attributes.POSITION, position + "");
      //resultElement.setAttribute("length", length + "");
    }
    return resultElement;
  }

}