package zTests.copybookUpdater;

import static org.junit.Assert.assertEquals;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;

import javax.xml.stream.XMLStreamException;

import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.analysis.BaseItem;
import net.sf.cb2xml.analysis.Condition;
import net.sf.cb2xml.analysis.Copybook;
import net.sf.cb2xml.analysis.Item;
import net.sf.cb2xml.analysis.ItemBuilder;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.DialectManager;
import net.sf.cb2xml.def.ICondition;
import net.sf.cb2xml.def.ICopybook;
import net.sf.cb2xml.def.IItem;
import net.sf.cb2xml.def.Cb2xmlConstants.Usage;
import net.sf.cb2xml.update.IUpdateItem;
import net.sf.cb2xml.update.UpdateItemAdapter;

public class RunUpdateTest {
	
	public static final String[] COMP_TESTS = {
					  "        01  Rec.\n"
					+ "            05  Field-1                  pic s9(4).\n"
					+ "            05  Field-2                  pic s9(4) comp.\n"
					+ "            05  Field-3                  pic s9(5)V99 comp-3.\n"
					+ "            05  Field-4                  pic s9(7)V99 comp-5.\n"
					+ "            05  Field-5                  pic s9(6)V99.\n"
					,
					  "            05 occurs 5.\n"
					+ "               15 Field-11              pic s9(4).\n"
					+ "               15 Field-12              pic s9(4) comp.\n"
					+ "               15 Field-13              pic s9(5)V99 comp-3.\n"
					+ "               15 Field-14              pic s9(6)V99.\n"
					,
					  "            05 occurs 6.\n"
					+ "               10 occurs 3.                        \n"
					+ "                  15 Field-21           pic s9(4).\n"
					+ "                  15 Field-22           pic s9(4) comp.\n"
					+ "                  15 Field-23           pic s9(5)V99 comp-3.\n"
					+ "                  15 Field-24           pic s9(6)V99.\n"
	};
	
	public static final String LAST_FIELD = "            05  Field-6                  pic x(4).\n";
	
	public static final  IUpdateItem REMOVE_USAGE = new UpdateItemAdapter() {
		@Override public Item updateItem(BaseItem parent, IItem itemToBeUpdated) {
			ItemBuilder itemBuilder = ItemBuilder.newBuilder();
			itemBuilder.setFrom(itemToBeUpdated);
			itemBuilder.setUsage(Usage.NONE);
			return itemBuilder.build(parent);
		}
	};
	
	public static final  IUpdateItem BASIC_UPDATE = new UpdateItemAdapter() {

		@Override
		public Condition updateCondition(ICondition conditionToBeUpdated) {
			return new Condition(conditionToBeUpdated);
		}
		
	};

	private IUpdateItem updater = REMOVE_USAGE;
	boolean updateComp = true;
	
	public RunUpdateTest() {
		
	}
	
	public RunUpdateTest(boolean updateComp) {
		this.updateComp = updateComp;
		if (! updateComp) {
			this.updater = BASIC_UPDATE;
		}
	}
	
	
	/**
	 * Run a Cobol-Copybook String Thrugh
	 * @param copybookStr
	 * @param cpy
	 * @param updater
	 * @throws FileNotFoundException
	 * @throws XMLStreamException
	 * @throws IOException
	 */
	protected void doTestCopybookStr(String copybookStr, String cpy)
			throws FileNotFoundException, XMLStreamException, IOException {
		String noUsageCopybookStr = copybookStr;
		
		if (updateComp) {
			noUsageCopybookStr = copybookStr
					.replace("COMP-3", "")
					.replace("comp-3", "")
					.replace("COMP-5", "")
					.replace("comp-5", "")
					.replace("COMP.", ".")
					.replace("comp", "");
		}
		
		System.out.println(noUsageCopybookStr);
		ICopybook copybook = toCopybookClass(new StringReader(copybookStr), cpy);
		ICopybook noUsageCopybook = toCopybookClass(new StringReader(noUsageCopybookStr), cpy);
		
		Copybook updatedCopybook = Cb2Xml3.updateCopybook(
						copybook, 
						updater, 
						DialectManager.MAINFRAME_COBOL.getNumericDefinition());
		
		String xmlString = toXml(updatedCopybook);
		String noUsageXmlString = toXml(noUsageCopybook);
		
		System.out.println(xmlString);
		
		assertEquals(noUsageXmlString, xmlString);
	}

	/**
	 * @param copybookFilename
	 * @return
	 * @throws FileNotFoundException
	 */
	protected ICopybook toCopybookClass(Reader copybookReader, String copybookName) throws FileNotFoundException {
		return Cb2Xml3.newBuilder(copybookReader, copybookName)
			.setIndent(true)
			.asCobolItemTree();
	}
	
	/**
	 * @param updatedCopybook
	 * @return
	 * @throws XMLStreamException
	 * @throws IOException
	 */
	protected String toXml(ICopybook updatedCopybook) throws XMLStreamException, IOException {
		StringWriter writer = new StringWriter();
		new net.sf.cb2xml.util.WriteXml(true, true, Cb2xmlConstants.STANDARD_FONT)
				.writeCopybook(writer, updatedCopybook, true);

		return writer.toString();
	}
}
