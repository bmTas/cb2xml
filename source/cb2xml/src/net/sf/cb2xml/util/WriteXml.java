package net.sf.cb2xml.util;

import java.io.IOException;
import java.io.Writer;
import java.util.List;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import javax.xml.transform.Result;

import net.sf.cb2xml.analysis.Item;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.ICondition;
import net.sf.cb2xml.def.ICopybook;
import net.sf.cb2xml.def.IItem;

/**
 * This class will write Copybook / Item / Condition clauses
 * as Xml using Stax writer 
 * @author Bruce Martin
 *
 */
public class WriteXml {

	private final boolean newFormat, writeEmptyElements, recEdit;
	private final String encoding;

	

	public WriteXml(boolean newFormat, boolean writeEmptyElements, String encoding) {
		this(newFormat, false, writeEmptyElements, encoding);
	}
	public WriteXml(boolean newFormat, boolean recEdit, boolean writeEmptyElements, String encoding) {
		super();
		this.newFormat = newFormat;
		this.recEdit = recEdit;
		this.writeEmptyElements = writeEmptyElements;
		this.encoding = encoding;
	}

	/**
	 * Write a Cobol Copybook in Standard 
	 * @param fileWriter File Writer
	 * @param copybook Cobol Copybook Item
	 * @param prettyPrint
	 * @throws XMLStreamException
	 * @throws IOException 
	 */
	public final void writeCopybook(Writer fileWriter, ICopybook copybook, boolean indent) throws XMLStreamException, IOException {
		XMLOutputFactory xmlFactory = XMLOutputFactory.newInstance();
		
		XMLStreamWriter writer = xmlFactory.createXMLStreamWriter(fileWriter);
		
		writeCopybook(writer, copybook, indent);
		
		fileWriter.close();
	}

	public final void writeCopybook(Result result, ICopybook copybook) throws XMLStreamException, IOException {
		XMLOutputFactory xmlFactory = XMLOutputFactory.newInstance();
		
		writeCopybook(xmlFactory.createXMLStreamWriter(result), copybook, false);
	}

	public final void writeCopybook(XMLStreamWriter xmlWriter, ICopybook copybook, boolean indent) throws XMLStreamException {

		if (indent) {
			xmlWriter = new IndentXmlWriter(xmlWriter);
		}
		if (encoding == null || encoding.length() == 0) {
			xmlWriter.writeStartDocument("1.0");
		} else {
			xmlWriter.writeStartDocument(encoding, "1.0");
		}

		xmlWriter.writeStartElement(Cb2xmlConstants.COPYBOOK);

		writeAttr(xmlWriter, Cb2xmlConstants.FILENAME, copybook.getFilename());
		writeAttr(xmlWriter, Cb2xmlConstants.DIALECT, copybook.getDialect());
		if (newFormat) {
			writeAttr(xmlWriter, Cb2xmlConstants.CB2XML_FORMAT, "2017");
		}
		writeChildren(xmlWriter, copybook.getAllElements());

		xmlWriter.writeEndElement();

		xmlWriter.writeEndDocument();
		xmlWriter.close();
	}
	
	
	/**
	 * Write a list of Items (and there child items / conditions) to a Xml stream
	 * @param writer Xml-Writer
	 * @param items Cobol Items to be written
	 * 
	 * @throws XMLStreamException
	 */
	public final void writeIItems(XMLStreamWriter writer, List<? extends IItem> items)
			throws XMLStreamException {
		for (IItem itm : items) {
			writeIItem(writer, itm);
		}
	}

	
	private void writeChildren(XMLStreamWriter writer, List<Object> allElements) throws XMLStreamException {

		if (allElements != null) {
			for (Object o : allElements) {				
				if (o instanceof Item) {
					writeItem(writer, (Item) o);
				} else if (o instanceof ICondition) {
					writeCondition(writer, (ICondition) o);
				} else if (o.getClass() == String.class) {
					writer.writeComment(o.toString());
				} else if (o instanceof IItem) {
					writeIItem(writer, (IItem) o);
				} 
			}
		}
	}


	private void writeCondition(XMLStreamWriter writer, ICondition condition) throws XMLStreamException {

		List<? extends ICondition> childConditions = condition.getChildConditions();

		if (writeEmptyElements && (childConditions == null || childConditions.size() == 0)) {
			writer.writeEmptyElement(Cb2xmlConstants.CONDITION);
			writeAttr(writer, Cb2xmlConstants.NAME, condition.getName());
			writeAttr(writer, Cb2xmlConstants.VALUE, condition.getValue());
			writeAttr(writer, Cb2xmlConstants.THROUGH, condition.getThrough());
		} else {
			writer.writeStartElement(Cb2xmlConstants.CONDITION);
			writeAttr(writer, Cb2xmlConstants.NAME, condition.getName());
			writeAttr(writer, Cb2xmlConstants.VALUE, condition.getValue());
			writeAttr(writer, Cb2xmlConstants.THROUGH, condition.getThrough());
	
			writeChildConditions(writer, childConditions);
			
			writer.writeEndElement();
		}
	}


	private void writeChildConditions(XMLStreamWriter writer, List<? extends ICondition> childConditions)
			throws XMLStreamException {
		for (ICondition c : childConditions) {
			writeCondition(writer, c);
		}
	}


	private void writeItem(XMLStreamWriter writer, Item item) throws XMLStreamException {
		
		List<Object> allElements = item.getAllElements();
		if (writeEmptyElements && (allElements == null || allElements.size() == 0)) {
			writer.writeEmptyElement(Cb2xmlConstants.ITEM);
			writeItemAttributes(writer, item);
		} else {
			writer.writeStartElement(Cb2xmlConstants.ITEM);
			writeItemAttributes(writer, item);
			
			writeChildren(writer, allElements);
			
			writer.writeEndElement();
		}
	}

	private void writeItemAttributes(XMLStreamWriter writer, Item item) throws XMLStreamException {
		writeIItemAttributes(writer, item);
		
		if (item.isSigned() && ! newFormat) {
			writeAttr(writer, Cb2xmlConstants.SIGNED, true);				
		}
	}
	
	/**
	 * Write a IItem and its child conditions and items.
	 * 
	 * @param writer Xml-Writer
	 * @param item Cobol-Item to be written
	 * 
	 * @throws XMLStreamException
	 */
	private void writeIItem(XMLStreamWriter writer, IItem item) throws XMLStreamException {
		
		List<? extends ICondition> conditions = item.getConditions();
		List<? extends IItem> childItems = item.getChildItems();
		
		if (writeEmptyElements && conditions.size() == 0 && childItems.size() == 0) {
			writer.writeEmptyElement(Cb2xmlConstants.ITEM);
			writeIItemAttributes(writer, item);
		} else {
			writer.writeStartElement(Cb2xmlConstants.ITEM);
			writeIItemAttributes(writer, item);
			
			writeChildConditions(writer, conditions);
			writeIItems(writer, childItems);
			
			writer.writeEndElement();
		}
	}

	
	/**
	 * Write the attributes defined by IItem interface
	 * 
	 * @param writer XmlWriter
	 * @param item item to write trhe attributes for
	 * @throws XMLStreamException
	 */
	private void writeIItemAttributes(XMLStreamWriter writer, IItem item) throws XMLStreamException {
	
//		System.out.println(item.getLevelString() + "\t" + item.getFieldName() + "\t" + item.getOccurs());
		writeAttr(writer, Cb2xmlConstants.LEVEL, item.getLevelString());
		writeAttribute(writer, Cb2xmlConstants.NAME, item.getFieldName());
		writeAttr(writer, Cb2xmlConstants.OCCURS, item.getOccurs(), -1);
		writeAttr(writer, Cb2xmlConstants.OCCURS_MIN, item.getOccursMin(), -1);
		writeAttr(writer, Cb2xmlConstants.DEPENDING_ON, item.getDependingOn());
		writeAttr(writer, Cb2xmlConstants.PICTURE, item.getPicture());
		
//		writeAttr(writer, Cb2xmlConstants.INSERT_DECIMAL_POINT, item.isInsertDecimal());
		writeAttr(writer, Cb2xmlConstants.USAGE, item.getUsage());
		writeAttr(writer, Cb2xmlConstants.JUSTIFIED, item.getJustified());
		writeAttr(writer, Cb2xmlConstants.POSITION, item.getPosition());
		writeAttr(writer, Cb2xmlConstants.STORAGE_LENGTH, item.getStorageLength(), 0);
		writeAttr(writer, Cb2xmlConstants.DISPLAY_LENGTH, item.getDisplayLength(), -1);

		if (item.getScale() != 0) {
			writeAttr(writer, Cb2xmlConstants.SCALE, item.getScale(), IItem.NULL_INT_VALUE);
		}
		
		if (newFormat) {
			writeAttr(writer, Cb2xmlConstants.NUMERIC, item.getNumericClass());
			writeAttr(writer, Cb2xmlConstants.SIGN_CLAUSE, item.getSignClause());
			if (! recEdit) {
				writeAttr(writer, Cb2xmlConstants.DISPLAY_POSITION, item.getDisplayPosition());
			}
		} else {
			writeAttr(writer, Cb2xmlConstants.NUMERIC, item.getNumericClass().numeric);
			writeAttr(writer, Cb2xmlConstants.EDITTED_NUMERIC, item.getNumericClass().editNumeric);
			writeAttr(writer, Cb2xmlConstants.SIGN_POSITION, item.getSignClause().signPosition);
			writeAttr(writer, Cb2xmlConstants.SIGN_SEPARATE, item.getSignClause().signSeparate);
		}
		writeAttr(writer, Cb2xmlConstants.SYNC, item.isSync());
		writeAttr(writer, Cb2xmlConstants.VALUE, item.getValue());
		writeAttr(writer, Cb2xmlConstants.REDEFINED, item.isFieldRedefined());
		writeAttr(writer, Cb2xmlConstants.REDEFINES, item.getRedefinesFieldName());
		writeAttr(writer, Cb2xmlConstants.INHERITED_USAGE, item.isInheritedUsage());
		writeAttr(writer, Cb2xmlConstants.BLANK_WHEN_ZERO, item.isBlankWhenZero());
	}

	private void writeAttr(XMLStreamWriter writer, String attr, boolean value)
	throws XMLStreamException {
		if (value) {
			 writer.writeAttribute(attr, Cb2xmlConstants.TRUE);
		}
	}

	private void writeAttr(XMLStreamWriter writer, String attr, Cb2xmlConstants.IGetName value)
	throws XMLStreamException {
		if (value != null && value.getName() != null) {
			 writer.writeAttribute(attr, value.getName());
		}
	}


	private void writeAttribute(XMLStreamWriter writer, String attr, String value)
	throws XMLStreamException {
		if (value != null ) {
			 writer.writeAttribute(attr, value);
		}
	}


	private void writeAttr(XMLStreamWriter writer, String attr, String value)
	throws XMLStreamException {
		if (value != null && value.length() > 0) {
			 writer.writeAttribute(attr, value);
		}
	}
	
	private void writeAttr(XMLStreamWriter writer, String attr, int value)
	throws XMLStreamException {
		writeAttr(writer, attr, value, 0);
	}
	
	
	private void writeAttr(XMLStreamWriter writer, String attr, int value, int min)
	throws XMLStreamException {
		if (value > min) {
			writer.writeAttribute(attr, Integer.toString(value));
		}
	}

}
