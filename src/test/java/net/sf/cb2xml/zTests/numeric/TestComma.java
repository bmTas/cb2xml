package net.sf.cb2xml.zTests.numeric;

import static org.junit.jupiter.api.Assertions.*;

import java.io.StringReader;
import java.util.List;

import org.junit.jupiter.api.Test;

import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.def.IItem;

/**
 * This class ensures trailing commas in a picture clause are ignored 
 * ie the comma in the following picture should be treated as white space
 *    05 CALC-SALES PIC 9(6)V99, VALUE 0
 */
class TestComma {

	private static String commaCobolxx = ""
			+ "         01  CALC-COMMISSION-FIELDS.\n"
			+ "             05 EMP-TYPE PIC X.\n"
			+ "             05 CALC-SALES PIC 9(6)V99, VALUE 0.\n"
			+ "             05 CALC-COMMISSION PIC 9(5)V99 COMP-3 VALUE 0.";
	private static String commaCobol1 = ""
			+ "        01  CALC-COMMISSION-FIELDS.\n"
			+ "            05 CALC-SALES PIC 9(6)V99, VALUE 0.\n";
	@Test
	void test1() {
		List<? extends IItem> itemTree
				= Cb2Xml3.newBuilder(new StringReader(commaCobol1), "Commission")
						.asCobolItemTree().getChildItems();
		
//		System.out.println(itemTree.size() + " " + itemTree.get(0).getDisplayLength()  +  " " + itemTree.get(0).getStorageLength());
		assertEquals(1, itemTree.size());
		IItem item = itemTree.get(0);
		assertEquals(8, item.getDisplayLength());
		assertEquals(8, item.getStorageLength());
	
		assertEquals(1, itemTree.get(0).getChildItems().size());
		item = itemTree.get(0).getChildItems().get(0);
		assertEquals("CALC-SALES", item.getFieldName());
		assertEquals(8, item.getDisplayLength());
		assertEquals(8, item.getStorageLength());

	}
	@Test
	void testXX() {
		List<? extends IItem> itemTree
				= Cb2Xml3.newBuilder(new StringReader(commaCobolxx), "Commission")
						.asCobolItemTree().getChildItems();
		
//		System.out.println(itemTree.size() + " " + itemTree.get(0).getDisplayLength()  +  " " + itemTree.get(0).getStorageLength());
		assertEquals(1, itemTree.size());
		
		IItem item = itemTree.get(0);
		assertEquals(16, item.getDisplayLength());
		assertEquals(13, item.getStorageLength());
		
		assertEquals(3, itemTree.get(0).getChildItems().size());
		item = itemTree.get(0).getChildItems().get(1);
		assertEquals("CALC-SALES", item.getFieldName());
		assertEquals(8, item.getDisplayLength());
		assertEquals(8, item.getStorageLength());

	}

}
