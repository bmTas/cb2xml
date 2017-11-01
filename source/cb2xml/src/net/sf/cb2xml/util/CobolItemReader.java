package net.sf.cb2xml.util;

import java.util.ArrayList;
import java.util.List;

import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import net.sf.cb2xml.analysis.BaseItem;
import net.sf.cb2xml.analysis.Condition;
import net.sf.cb2xml.analysis.Item;
import net.sf.cb2xml.analysis.ItemBuilder;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.IItem;


/**
 * This class will load Cobol-Items from a cb2xml-Xml Document (new format)
 * <b>It has not been tested !!!</b>
 * 
 * @author Bruce Martin
 *
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
		return parseCobolItems(reader, itmBldr, itmBldr.build(null));
	}

	private List<? extends Item> parseCobolItems(XMLStreamReader reader, ItemBuilder itmBldr, BaseItem base) throws XMLStreamException {
		int type = reader.getEventType();
//		ArrayList<Item> items = new ArrayList<Item>();
		String name;
		ReaderBuilder readerBldr = new ReaderBuilder(reader, itmBldr, base);
		
		while (type != XMLStreamConstants.END_DOCUMENT) {
			switch (type) {
	        case (XMLStreamConstants.START_ELEMENT) :
	            name = reader.getName().toString().toLowerCase();
	        
	        	switch (name) {
	        	case Cb2xmlConstants.CONDITION:
	        		processCondition(readerBldr);
	        		break;
	        	case Cb2xmlConstants.ITEM:
	        		processItem(readerBldr);
	        		break;
	        	default:
	        		if (readerBldr.list.size() < 2) {
	        			return readerBldr.topItem.getChildItems();
	        		}
	        	}
	        	break;
	        case (XMLStreamConstants.END_ELEMENT):
	            name = reader.getName().toString().toLowerCase();
	        
	        	switch (name) {
	        	case Cb2xmlConstants.COPYBOOK:
	        	case "items" : return readerBldr.topItem.getChildItems();
	        	case Cb2xmlConstants.CONDITION:
	        	case Cb2xmlConstants.ITEM:
	        		readerBldr.pop();
	        		break;
	        	}
	        	break;
			}
        	type = reader.next();
		}
		return readerBldr.topItem.getChildItems();
	}
	
	private void processCondition(ReaderBuilder rBldr) {
		String name    = rBldr.getAttr(Cb2xmlConstants.NAME);
		String through = rBldr.getAttr(Cb2xmlConstants.THROUGH);
		String value   = rBldr.getAttr(Cb2xmlConstants.VALUE);
		
		rBldr.add(new Condition(name, through, value));

	}
	
	private void processItem(ReaderBuilder rBldr) {
		ItemBuilder itmBldr = rBldr.itmBldr;
		
		itmBldr.setLevelString(rBldr.getAttr(Cb2xmlConstants.LEVEL));
		itmBldr.setFieldName(rBldr.getAttr(Cb2xmlConstants.NAME));
		itmBldr.setBlankWhenZero(rBldr.getBooleanAttr(Cb2xmlConstants.BLANK_WHEN_ZERO));
		itmBldr.setDependingOn(rBldr.getAttr(Cb2xmlConstants.DEPENDING_ON));
		itmBldr.setDisplayLength(rBldr.getIntAttr(Cb2xmlConstants.DISPLAY_LENGTH));
		itmBldr.setFieldRedefined(rBldr.getBooleanAttr(Cb2xmlConstants.REDEFINED));
		itmBldr.setInheritedUsage(rBldr.getBooleanAttr(Cb2xmlConstants.INHERITED_USAGE));
		itmBldr.setJustified(rBldr.getJustified());
		itmBldr.setNumericClass(rBldr.getNumericClass());
		itmBldr.setOccursMin(rBldr.getIntAttr(Cb2xmlConstants.OCCURS_MIN));
		itmBldr.setPicture(rBldr.getAttr(Cb2xmlConstants.PICTURE));
		itmBldr.setPosition(rBldr.getIntAttr(Cb2xmlConstants.POSITION));
		itmBldr.setRedefines(rBldr.getAttr(Cb2xmlConstants.REDEFINES));
		itmBldr.setScale(rBldr.getIntAttr(Cb2xmlConstants.SCALE));
		itmBldr.setSignClause(rBldr.getSignClause());
		itmBldr.setSigned(rBldr.getBooleanAttr(Cb2xmlConstants.SIGNED));
		itmBldr.setSync(rBldr.getBooleanAttr(Cb2xmlConstants.SYNC));
		itmBldr.setUsage(rBldr.getUsage());
		itmBldr.setValue(rBldr.getAttr(Cb2xmlConstants.VALUE));
		
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
		
		public final BaseItem topItem;
		public BaseItem baseItem;
		public final ArrayList<ItemCondition> list = new ArrayList<ItemCondition>();
		
		public ReaderBuilder(XMLStreamReader reader, ItemBuilder itmBldr, BaseItem topItem) {
			super();
			this.reader = reader;
			this.itmBldr = itmBldr;
			this.topItem = topItem;
			this.baseItem = topItem;

			if (topItem instanceof Item) {
				add((Item) topItem);
			}
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
			return Cb2xmlConstants.TRUE.equals(s);
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
