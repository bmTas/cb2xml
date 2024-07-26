/*************************************************************
 * This file is part of CB2XML.  
 * See the file "LICENSE" for copyright information and the
 * terms and conditions for copying, distribution and
 * modification of CB2XML.
 *************************************************************
 */

package net.sf.cb2xml.convert;

import java.util.Hashtable;

import net.sf.cb2xml.def.Cb2xmlConstants;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
* this class handles converting a Hashtable representation of data into
* its copybook equivalent StringBuffer (mainframe equivalent)
* it recurses the DOM of the copybook definition (as XML) and checks if a
* hashtable key exists for each data node.  if found, it packs the hashtable
* value into the output string buffer and continues
* the weird logic you may see would be to
*
* 1) checking for a leading 1 or 0 in the StringBuffer to identify if there was a child
*    element value in the hashtable corresponding to the branch (or child branches) being traversed
*    resorted to this 'hack' to simplify the signature of the method being recursed
*
* 2) due to 'REDEFINES' complications, a decision has to be made if the hashtable
*    has data for more than one of the redefined instances (should be XOR)
*    below, the code selects the first XOR instance, so attempts to ignore inconsistent inputs
*
* TODO : the special mainframe formats of COMP, packed-decimal etc are not supported yet
* 
*  * note that files within the "net.sf.cb2xml.convert" package are not stable
*
* @author Peter Thomas
*/

public class HashtableToMainframe {

  private Hashtable keyValuePairs = null;

  private StringBuffer getRepeatedChars(char charToRepeat, int count) {
    StringBuffer sb = new StringBuffer();
    for (int i = 0; i < count; i++) {
      sb.append(charToRepeat);
    }
    return sb;
  }

  public String convert(Hashtable keyAndValuePairs, Document copyBookXml) {
    this.keyValuePairs = keyAndValuePairs;
    Element documentElement = copyBookXml.getDocumentElement();
    //Element element = (Element) documentElement.getFirstChild();
    Element element = Utils.getFirstElement(documentElement);
    String documentTagName = documentElement.getTagName();
    String tagName = element.getAttribute(Cb2xmlConstants.NAME);
    String xpath = "/" + documentTagName + "/" + tagName;
    return convertNode(element, xpath).deleteCharAt(0).toString();
  }

  private StringBuffer convertNode(Element element, String xpath) {
    StringBuffer segment = new StringBuffer();
    segment.append('0');
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
          String childElementName = childElement.getAttribute(Cb2xmlConstants.NAME);
          childElementCount++;
          int childPosition = Integer.parseInt(childElement.getAttribute(
              Cb2xmlConstants.POSITION));
          StringBuffer tempBuffer = null;
          if (childElement.hasAttribute(Cb2xmlConstants.OCCURS)) {
            tempBuffer = new StringBuffer();
            tempBuffer.append('0');
            int childOccurs = Integer.parseInt(childElement.getAttribute(
                Cb2xmlConstants.OCCURS));
            //int childLength = Integer.parseInt(childElement.getAttribute(
            //    "length"));
           // int singleChildLength = childLength / childOccurs;
            for (int j = 0; j < childOccurs; j++) {
              StringBuffer occursBuffer = convertNode(childElement,
                  xpath + "/" + childElementName + "[" + j + "]");
              if (occursBuffer.charAt(0) == '1') {
                tempBuffer.setCharAt(0, '1');
              }
              occursBuffer.deleteCharAt(0);
              tempBuffer.append(occursBuffer);
            }
          }
          else {
            tempBuffer = convertNode(childElement,
                                     xpath + "/" + childElementName);
          }
          if (childElement.hasAttribute(Cb2xmlConstants.REDEFINES) &&
              tempBuffer.charAt(0) == '1') {
            tempBuffer.deleteCharAt(0);
            int replacePosition = childPosition - position;
            segment.replace(replacePosition,
                            replacePosition + tempBuffer.length(),
                            tempBuffer.toString());
          }
          else {
            if (tempBuffer.charAt(0) == '1') {
              segment.setCharAt(0, '1');
            }
            tempBuffer.deleteCharAt(0);
            segment.append(tempBuffer);
          }
        }
      }
    }
    if (childElementCount == 0) {
      if (keyValuePairs.containsKey(xpath)) {
        segment.setCharAt(0, '1');
        
        Object obj = keyValuePairs.get(xpath);
        String s = obj==null ? "" : obj.toString();
        if (s.length() < length) {
        	if (numeric) {
        		if (s.startsWith("-")) {
        			segment	.append('-')
        					.append(getRepeatedChars('0', length-s.length()))
        					.append(s);
        		} else {
        			if (s.startsWith("+")) {
        				s = s.substring(1);
        			}
        			segment	.append(getRepeatedChars('0', length-s.length()))
        					.append(s);
        		}
        	} else {
        		segment	.append(s)
        				.append(getRepeatedChars(' ', length-s.length()));
        	}
        } else if (s.length() < length) {
        	segment.append( s.substring(0, length));
        } else {
        	segment.append(s);
        }
      }
      else {
        if (element.hasAttribute(Cb2xmlConstants.VALUE)) {
          segment.append(element.getAttribute(Cb2xmlConstants.VALUE));
        }
//        else if (element.hasAttribute("spaces")) {
//          segment.append(getRepeatedChars(' ', length));
//        }
        else if (element.hasAttribute("zeros")) {
          segment.append(getRepeatedChars('0', length));
        } else {
          segment.append(getRepeatedChars(' ', length));
        }
      }
    }
    return segment;
  }

}