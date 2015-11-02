package net.sf.cb2xml.parse;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.stream.StreamSource;

import net.sf.cb2xml.jaxb.Condition;
import net.sf.cb2xml.jaxb.Copybook;
import net.sf.cb2xml.jaxb.Item;

import org.w3c.dom.Document;


/**
 * This class will parse cb2xml - Xml into Copybook / Item classes
 * 
 * @author Bruce Martin
 *
 */
public class XmlParser {

	/**
	 * Parse a cb2xml document
	 * @param cb2XmlDocument cb2xml document
	 * @return Copybook definition this holds all the Cobol Copybook details in 
	 * easily access recursive Java class structure
	 * @throws JAXBException
	 */
	public final Copybook parse(Document cb2XmlDocument) throws JAXBException {
	    JAXBContext jc = JAXBContext.newInstance(Condition.class, Copybook.class, Item.class);
	       
	    Unmarshaller unmarshaller = jc.createUnmarshaller();
		JAXBElement<Copybook> copybook = unmarshaller.unmarshal(cb2XmlDocument, Copybook.class);
		return copybook.getValue();
	}

	/**
	 * Parse a cb2xml - Xml file into a <i>Copybook</i> class
	 * @param fileName Xml filename
	 * @return Copybook definition this holds all the Cobol Copybook details in 
	 * easily access recursive Java class structure
	 * 
	 * @throws JAXBException
	 * @throws IOException
	 */
	public final Copybook parseXml(String fileName) throws JAXBException, IOException {
	    return parseXml(new FileInputStream(fileName));
	}

	/**
	 * Convert Xml (from a stream) into <i>Copybook</i> class
	 * @param xml xml stream
	 * @return Copybook definition this holds all the Cobol Copybook details in 
	 * easily access recursive Java class structure
	 * 
	 * @throws JAXBException
	 * @throws IOException
	 */
	public final Copybook parseXml(InputStream xml) throws JAXBException, IOException {
	    JAXBContext jc = JAXBContext.newInstance(Condition.class, Copybook.class, Item.class);
	    
	    Unmarshaller unmarshaller = jc.createUnmarshaller();
	    JAXBElement<Copybook> copybook = unmarshaller.unmarshal(new StreamSource(xml), Copybook.class);
	    xml.close();
	
	    return copybook.getValue();
	}
	
	
	public static XmlParser newParser() {
		return new XmlParser();
	}


}