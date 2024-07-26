package net.sf.cb2xml.zTests.common;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import net.sf.cb2xml.util.XmlUtils;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;


/**
 * Common Code to compare Xml Documents
 * @author Bruce Martin
 *
 */
public class Code {

	public static String toString(String[] lines) {
		StringBuilder b = new StringBuilder();
		String sep = "";
		
		for (String s : lines) {
			b.append(sep).append(s);
			sep = "\n";
		}
		
		return b.toString();
	}
	
	public static InputStream toStream(String[] lines) {
		return toStream(toString(lines));
	}
	
	public static InputStream toStream(String s) {
		return new ByteArrayInputStream(s.getBytes());
	}
	
	public static void compare(String id, Document doc, String fileName) throws IOException, SAXException, ParserConfigurationException {
		compare(id, doc, fileToDom(fileName));
	}
	
	public static void compare(String id, Document doc,Document doc2) {
		String s1 = XmlUtils.domToString(doc).toString();
		String s2 = XmlUtils.domToString(doc2).toString();
		if (s1.equals(s2) || compareDocs(id, doc, doc2)) {
		} else {
			System.out.println("Lengths= " + s2.length() + ", " + s1.length());
			org.junit.jupiter.api.Assertions.assertEquals(s1, s2, id);
		}
	}
	
	
	public static void compare(String id, String fileName, Document doc) throws IOException, SAXException, ParserConfigurationException {
		compare(id, fileToDom(fileName), doc);
	}
	
	public static boolean compareDocs(String id, Document doc1, Document doc2) {	
		System.out.println("---------------- " + id);
		System.out.println(XmlUtils.domToString(doc2).toString());
		System.out.println();
		return compare(doc1.getDocumentElement(), 
				doc2.getDocumentElement());
	}
	
	
	private static boolean compare(Element element1, Element element2) {
        NodeList nodeList1 = element1.getChildNodes();
        NodeList nodeList2 = element2.getChildNodes();
        boolean ret = true;
       //level += 1;

        if (nodeList1.getLength() == nodeList2.getLength()) {
	        for (int i = 0; ret && i < nodeList1.getLength(); i++) {
	        	ret = checkNode(nodeList1.item(i), nodeList2.item(i));
	        }
  //      } else if (nodeList1.getLength() < nodeList2.getLength()) {
        } else {
        	System.out.println(":: Node Length >>" + element1.getNodeName() + " " + nodeList1.getLength()  + " " + nodeList2.getLength());
        	return false;
        }
        

        return ret;
	}
	
	private static boolean checkNode(org.w3c.dom.Node node1, org.w3c.dom.Node node2) {
        if (node1.getNodeType() == org.w3c.dom.Node.ELEMENT_NODE 
        &&  node2.getNodeType() == org.w3c.dom.Node.ELEMENT_NODE) {
            Element childElement1 = (Element) node1;
            Element childElement2 = (Element) node2;
            NamedNodeMap attributes = childElement1.getAttributes();
            //System.out.println();
            
            for (int i = 0; i < attributes.getLength(); i++) {
            	Node item = attributes.item(i);
            	
            	if (item instanceof Attr) {
            		Attr attr = (Attr) item;
					String value1 = attr.getValue();
					if (childElement2.hasAttribute(attr.getName())){
	            		String value2 = childElement2.getAttribute(attr.getName());
	            		System.out.print("\t" +attr.getName() + " ~ " + value1 + "==" + value2);
						if (value1 == null) {
							if (value2 != null) {
								System.out.println();
					        	System.out.println(":: Attribute >>" + attr.getName() + " ~ " + value1 + "==" + value2);
								return false;
							}
	            		} else if (! value1.equals(value2)) {
							System.out.println();
				        	System.out.println(":: Attribute >>" + attr.getName() + " ~ " + value1 + "==" + value2);
				        	return false;
	            		}
					}
            	}
            }
            
            return compare(childElement1, childElement2);
        } else if (node1.getNodeType() == org.w3c.dom.Node.TEXT_NODE 
        	   &&  node2.getNodeType()  == org.w3c.dom.Node.TEXT_NODE) {
        	String s1 = node1.getNodeValue();
        	String s2 = node2.getNodeValue();
        	if ((s1 == null && s2 != null)
        	|| ! s1.equals(s2)) {
        		return false;
        	}
        } else if (node1.getNodeType() != node2.getNodeType()) {
        	return false;
        }
        return true;
	}
	
    public static Document fileToDom(String fileName)
	throws IOException, SAXException, ParserConfigurationException {

        DocumentBuilderFactory factory
           		= DocumentBuilderFactory.newInstance();
        return factory.newDocumentBuilder().parse(new File(fileName));
    }

    public static String getFullName(String filename) {
    	URL resource = Code.class.getResource(filename);
    	if (resource == null) {
    		System.out.println(" --> Can not find: " + filename);
    		throw new RuntimeException(" --> Can not find: " + filename);
    	}
		return resource.getFile();
    }
}
