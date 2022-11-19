package net.sf.cb2xml.util;

import java.util.List;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import net.sf.cb2xml.analysis.Copybook;
import net.sf.cb2xml.analysis.Item;
import net.sf.cb2xml.analysis.ItemBuilder;


/**
 * This class will load Cobol-Items from a cb2xml-Xml Document (new format)
 * <b>It has not been tested !!!</b>
 * 
 * @author Bruce Martin
 *
 *@deprecated use Cb2xmlReader instead
 */
public class CobolItemReader {

	
	/**
	 * Process Items 
	 * @param reader
	 * @param itmBldr
	 * @return
	 * @throws XMLStreamException
	 */
	public List<? extends Item> parseCobolItems(XMLStreamReader reader, ItemBuilder itmBldr) throws XMLStreamException {
		Copybook copybook = (new Cb2xmlReader()).parseCobolCopybook(reader, itmBldr);
		return copybook == null ? null : copybook.getChildItems();
	}

}