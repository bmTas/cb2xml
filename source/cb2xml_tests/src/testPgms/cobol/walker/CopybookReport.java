package testPgms.cobol.walker;

import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.ICondition;
import net.sf.cb2xml.def.ICopybook;
import net.sf.cb2xml.def.IItem;
import net.sf.cb2xml.walker.CopybookListnerAdapter;

public class CopybookReport extends CopybookListnerAdapter {
	private String spaces = "                                                                                                       ";
	
	@Override
	public void startCondition(ICondition condition) {
		StringBuilder sb = new StringBuilder(condition.getValue());
		String thru = condition.getThrough();
		
		if (thru != null && thru.length() > 0) {
			sb.append(" ==> ").append(thru).append(' ');
		}
		sb.append(' ');
		System.out.print(sb);
	}

	@Override
	public void startItem(IItem item) {
		StringBuilder b = new StringBuilder(
				'\n'
			+	spaces.substring(0, 2 * item.getRelativeLevel())
			+	item.getLevelString()
			+	' '
			+	item.getFieldName());
		
		b	.append(spaces.substring(0, 50 - b.length()))
			.append(item.getPosition()).append('\t')
			.append(item.getStorageLength()).append('\t');
		
		if (item.getPicture() != null) {
			b.append(item.getPicture());
		}
		
		if (item.getUsage() != null && item.getUsage() != Cb2xmlConstants.Usage.NONE) {
			b.append('\t').append(item.getUsage().getName());
		}
		b.append("    ");
		System.out.print(b);
	}

	@Override
	public void endCopybook(ICopybook copybook) {
		System.out.println();
	}

}
