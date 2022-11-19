package net.sf.cb2xml.util;

import java.io.Reader;
import java.util.ArrayList;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import net.sf.cb2xml.analysis.BaseItem;
import net.sf.cb2xml.analysis.Condition;
import net.sf.cb2xml.analysis.Copybook;
import net.sf.cb2xml.analysis.Item;
import net.sf.cb2xml.analysis.ItemBuilder;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.IItem;


public class Cb2xmlReader {
	
	public Copybook parseCobolCopybook(Reader reader) throws XMLStreamException {
    	XMLInputFactory f = XMLInputFactory.newInstance();
		return parseCobolCopybook(
				f.createXMLStreamReader(reader), 
				new ItemBuilder());
	}

	public Copybook parseCobolCopybook(XMLStreamReader reader) throws XMLStreamException {
		return parseCobolCopybook(reader, new ItemBuilder());
	}

	/**
	 * Process Items 
	 * @param reader
	 * @param itmBldr
	 * @return
	 * @throws XMLStreamException
	 */
	public Copybook parseCobolCopybook(XMLStreamReader reader, ItemBuilder itmBldr) throws XMLStreamException {
		ReaderBuilder readerBldr = new ReaderBuilder(reader, itmBldr);
		int type = reader.getEventType();
//		ArrayList<Item> items = new ArrayList<Item>();
		String name;
		
		while (type != XMLStreamConstants.END_DOCUMENT) {
			switch (type) {
	        case (XMLStreamConstants.START_ELEMENT) :
	            name = reader.getName().toString().toLowerCase();
	        
	        	switch (name) {
	        	case Cb2xmlConstants.COPYBOOK:
	        		processCopybook(readerBldr);
	        		break;
	        	case Cb2xmlConstants.CONDITION:
	        		processCondition(readerBldr);
	        		break;
	        	case Cb2xmlConstants.ITEM:
	        		processItem(readerBldr);
	        		break;
	        	default:
//	        		if (readerBldr.list.size() < 2) {
//	        			return readerBldr.copybook;
//	        		}
	        	}
	        	break;
	        case (XMLStreamConstants.END_ELEMENT):
	            name = reader.getName().toString().toLowerCase();
	        
	        	switch (name) {
	        	case Cb2xmlConstants.COPYBOOK: return readerBldr.copybook;
//	        	case "items" : return readerBldr.topItem.getChildItems();
	        	case Cb2xmlConstants.CONDITION:
	        	case Cb2xmlConstants.ITEM:
	        		readerBldr.pop();
	        		break;
	        	}
	        	break;
	        case XMLStreamConstants.COMMENT:
	        	if (reader.hasText()) {
	        		readerBldr.baseItem.addComment(reader.getText());
	        	}
			}
        	type = reader.next();
		}
		return readerBldr.copybook;
	}
	
	
	private void processCopybook(ReaderBuilder rBldr) {
		String filename    = rBldr.getAttr(Cb2xmlConstants.FILENAME);
		String dialect = rBldr.getAttr(Cb2xmlConstants.DIALECT);
		
		rBldr.setCopybook(new Copybook(filename, dialect));

	}

	
	private void processCondition(ReaderBuilder rBldr) {
		String name    = rBldr.getAttr(Cb2xmlConstants.NAME);
		String through = rBldr.getAttr(Cb2xmlConstants.THROUGH);
		String value   = rBldr.getAttr(Cb2xmlConstants.VALUE);
		
		rBldr.add(new Condition(name, through, value));

	}
	
	private void processItem(ReaderBuilder rBldr) {
		ItemBuilder itmBldr = rBldr.itmBldr;
		int val;
		
		
		itmBldr.newItem();
		itmBldr.setLevelString(rBldr.getAttr(Cb2xmlConstants.LEVEL));
		itmBldr.setFieldName(rBldr.getAttr(Cb2xmlConstants.NAME));
		itmBldr.setBlankWhenZero(rBldr.getBooleanAttr(Cb2xmlConstants.BLANK_WHEN_ZERO));
		itmBldr.setDependingOn(rBldr.getAttr(Cb2xmlConstants.DEPENDING_ON));
		
		
		
		
		if ((val = rBldr.getIntAttr(Cb2xmlConstants.DISPLAY_LENGTH)) != IItem.NULL_INT_VALUE) {
			itmBldr.setDisplayLength(rBldr.getIntAttr(Cb2xmlConstants.DISPLAY_LENGTH));
		}
		itmBldr.setFieldRedefined(rBldr.getBooleanAttr(Cb2xmlConstants.REDEFINED));
		itmBldr.setInheritedUsage(rBldr.getBooleanAttr(Cb2xmlConstants.INHERITED_USAGE));
		itmBldr.setJustified(rBldr.getJustified());
		itmBldr.setNumericClass(rBldr.getNumericClass());
		itmBldr.setPicture(rBldr.getAttr(Cb2xmlConstants.PICTURE));
		itmBldr.setRedefines(rBldr.getAttr(Cb2xmlConstants.REDEFINES));

		if ((val = rBldr.getIntAttr(Cb2xmlConstants.OCCURS)) != IItem.NULL_INT_VALUE) {
			itmBldr.setOccurs(val);
		}
		if ((val = rBldr.getIntAttr(Cb2xmlConstants.OCCURS_MIN)) != IItem.NULL_INT_VALUE) {
			itmBldr.setOccursMin(val);
		}
		if ((val = rBldr.getIntAttr(Cb2xmlConstants.POSITION)) != IItem.NULL_INT_VALUE) {
			itmBldr.setPosition(val);
		}
		if ((val = rBldr.getIntAttr(Cb2xmlConstants.SCALE)) != IItem.NULL_INT_VALUE) {
			itmBldr.setScale(val);
		}
		itmBldr.setSignClause(rBldr.getSignClause());
		itmBldr.setSigned(rBldr.getBooleanAttr(Cb2xmlConstants.SIGNED));
		itmBldr.setSync(rBldr.getBooleanAttr(Cb2xmlConstants.SYNC));
		itmBldr.setUsage(rBldr.getUsage());
		itmBldr.setValue(rBldr.getAttr(Cb2xmlConstants.VALUE));
		if ((val = rBldr.getIntAttr(Cb2xmlConstants.STORAGE_LENGTH)) != IItem.NULL_INT_VALUE) {
			itmBldr.setStorageLength(val);
		}
		if ((val = rBldr.getIntAttr(Cb2xmlConstants.DISPLAY_POSITION)) != IItem.NULL_INT_VALUE) {
			itmBldr.setDisplayPosition(val);
		}
		
		rBldr.add(itmBldr.build(rBldr.baseItem));
	}
	
	
	/**
	 * Holds either an Item or Condition
	 * @author Bruce Martin	
	 *
	 */
	private static class ItemCondition {
		final Item itm;
		final Condition condition;
		
		public ItemCondition(Item itm) {

			this.itm = itm;
			this.condition = null;
		}
		
		public ItemCondition(Condition condition) {

			this.itm = null;
			this.condition = condition;
		}
	}
	
	/**
	 * 
	 * @author bruce
	 *
	 */
	private static class ReaderBuilder {
		final XMLStreamReader reader; 
		final ItemBuilder itmBldr;
		
		Copybook copybook;
		public BaseItem topItem;
		public BaseItem baseItem;
		public final ArrayList<ItemCondition> list = new ArrayList<ItemCondition>();
		
		public ReaderBuilder(XMLStreamReader reader, ItemBuilder itmBldr) {
			super();
			this.reader = reader;
			this.itmBldr = itmBldr;
			this.topItem = itmBldr.build(null);
			this.baseItem = topItem;
		}
		
		public String getAttr(String localName) {
			return reader.getAttributeValue(null, localName);
		}
		
		public int getIntAttr(String localName) {
			String s = getAttr(localName);
			if (s != null && s.length() > 0) {
				return Integer.parseInt(s);
			}
			return IItem.NULL_INT_VALUE;
		}
		
		public boolean getBooleanAttr(String localName) {
			String s = getAttr(localName);
			return Cb2xmlConstants.TRUE.equalsIgnoreCase(s);
		}
		
		public Cb2xmlConstants.Justified getJustified() {
			String s = getAttr(Cb2xmlConstants.JUSTIFIED);
			return Cb2xmlConstants.toJustified(s);
		}
		
		public Cb2xmlConstants.Usage getUsage() {
			String s = getAttr(Cb2xmlConstants.USAGE);
			return Cb2xmlConstants.toUsage(s);
		}
		
		public Cb2xmlConstants.SignClause getSignClause() {
			String s = getAttr(Cb2xmlConstants.SIGN_CLAUSE);
			return Cb2xmlConstants.toSignClause(s);
		}
		
		public Cb2xmlConstants.NumericClass getNumericClass() {
			String s = getAttr(Cb2xmlConstants.NUMERIC);
			return Cb2xmlConstants.toNumeric(s);
		}

		public void setCopybook(Copybook copybook) {
			this.copybook = copybook;
			this.topItem = copybook;
			this.baseItem = copybook;
		}

		public void add(Item itm) {
			list.add(new ItemCondition(itm));
			baseItem = itm;
		}

		public void add(Condition condition) {
			if (list.size() > 0) {
				ItemCondition ic = list.get(list.size() - 1);
				if (ic.itm != null) {
					ic.itm.addCondition(condition);
				} else {
					ic.condition.addCondition(condition);
				}
			}
			list.add(new ItemCondition(condition));
		}
	
		public void pop() {
			ItemCondition itm = list.remove(list.size() - 1);
			if (itm.itm != null) {
				baseItem = topItem;
				for (int i = list.size() - 1; i >= 0; i++) {
					if (list.get(i).itm != null) {
						baseItem = list.get(i).itm;
						return;
					}
				}
				
			}
		}

	}
}
