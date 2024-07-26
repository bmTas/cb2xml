package net.sf.cb2xml.convert;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class Utils {

	public static final Element getFirstElement(Element documentElement) {
		NodeList childNodes = documentElement.getChildNodes();
	    Node elementNode = null;
	    for (int i = 0; 
	    	(		i < childNodes.getLength() 
	    		 && ! ((elementNode = childNodes.item(i)) instanceof Element));
	    	i++) { 
	    }
	    
	    return (Element) elementNode;
	}
}
