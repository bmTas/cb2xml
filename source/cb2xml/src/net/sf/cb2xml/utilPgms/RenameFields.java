package net.sf.cb2xml.utilPgms;

import java.io.FileWriter;
import java.io.IOException;

import javax.xml.stream.XMLStreamException;

import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.analysis.BaseItem;
import net.sf.cb2xml.analysis.Condition;
import net.sf.cb2xml.analysis.Copybook;
import net.sf.cb2xml.analysis.Item;
import net.sf.cb2xml.analysis.ItemBuilder;
import net.sf.cb2xml.def.ICondition;
import net.sf.cb2xml.def.ICopybookJrUpd;
import net.sf.cb2xml.def.IItem;
import net.sf.cb2xml.def.Cb2xmlConstants.Usage;
import net.sf.cb2xml.update.IUpdateItem;

public class RenameFields {
	
	public void renameFields(String filename) throws XMLStreamException, IOException {
		ICopybookJrUpd copybook = Cb2Xml3.newBuilder(filename)
			.asCobolItemTree();
		
		Copybook updateCopybook = Cb2Xml3.updateCopybook(copybook, 
				new UpdateFieldNames(), 
				null);
		
		String xmlFileName = filename + ".xml";
		System.out.println(xmlFileName);
		new net.sf.cb2xml.util.WriteXml(true, false, null)
				.writeCopybook(new FileWriter(xmlFileName), updateCopybook, true);
	}

	public static void main(String[] args) throws XMLStreamException, IOException {
		RenameFields renameFields = new RenameFields();
		renameFields.renameFields(args[0]);
	}

	
	public static class UpdateFieldNames implements IUpdateItem {
		int fieldNum = 1;
		
		@Override
		public Item updateItem(BaseItem parent, IItem itemToBeUpdated) {
			ItemBuilder itemBuilder = ItemBuilder.newBuilder();
			itemBuilder.setFrom(itemToBeUpdated);
			itemBuilder.setUsage(Usage.NONE);
			if (itemToBeUpdated.getChildItems().size() > 0) {
				itemBuilder.setFieldName("G-" + fieldNum);
			} else {
				itemBuilder.setFieldName("Field-" + fieldNum);
			}
			fieldNum += 1;
			return itemBuilder.build(parent);
		}

		@Override
		public Condition updateCondition(ICondition conditionToBeUpdated) {
			return new Condition(conditionToBeUpdated);
		}
		
	}
}
