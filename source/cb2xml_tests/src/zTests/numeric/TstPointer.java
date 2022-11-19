package zTests.numeric;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.io.StringReader;
import java.util.List;

import javax.xml.stream.XMLStreamException;

import org.junit.Test;

import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.ICb2XmlBuilder;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.DialectManager;
import net.sf.cb2xml.def.ICopybookJrUpd;
import net.sf.cb2xml.def.IItemJrUpd;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;

public class TstPointer {

	private static final String[] POINTER_USAGE = {
	    	Cb2xmlConstants.POINTER,
	    	Cb2xmlConstants.PROCEDURAL_POINTER,
	    	Cb2xmlConstants.FUNCTION_POINTER,
	};
//	private String ptrCpy = ""
//			+ "       01  Pointer-Tst.\n"
//			+ "           03 Location-Name       Pic X(35).\n"
//			+ "           03 before-blah         PIC S9(8) COMP.\n"
//			+ "           03 blah                POINTER.\n"
//			+ "           03 afterblah           PIC X(12).\n";

	String numericXmlStr
		=   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
			"<copybook filename=\"Record\" dialect=\"Mainframe\">\n" + 
			"    <item level=\"01\" name=\"Pointer-Tst\" position=\"1\" storage-length=\"55\" display-length=\"55\">\n" + 
			"        <item level=\"03\" name=\"Location-Name\" picture=\"X(35)\" position=\"1\" storage-length=\"35\" display-length=\"35\"/>\n" + 
			"        <item level=\"03\" name=\"before-blah\" picture=\"S9(8)\" usage=\"computational\" position=\"36\" storage-length=\"4\" display-length=\"8\" numeric=\"true\" signed=\"true\"/>\n" + 
			"        <item level=\"03\" name=\"blah\" usage=\"pointer\" position=\"40\" storage-length=\"4\" display-length=\"0\"/>\n" + 
			"        <item level=\"03\" name=\"afterblah\" picture=\"X(12)\" position=\"44\" storage-length=\"12\" display-length=\"12\"/>\n" + 
			"    </item>\n" + 
			"</copybook>";
	
	
	@Test
	public void testPointer32bit() throws XMLStreamException, LexerException, IOException, ParserException {

		for (String usage : POINTER_USAGE) {
			tstPointer(4, usage);
		}
	}

	
	@Test
	public void testPointer64bit() throws XMLStreamException, LexerException, IOException, ParserException {

		for (String usage : POINTER_USAGE) {
			tstPointer(8, usage);
		}
	}


	/**
	 * @param pointerSize
	 * @param usage
	 * @throws XMLStreamException
	 * @throws LexerException
	 * @throws IOException
	 * @throws ParserException
	 */
	protected void tstPointer(int pointerSize, String usage)
			throws XMLStreamException, LexerException, IOException, ParserException {
		String numericXml = getXml(pointerSize, usage);
		
		String ptrCpy = ""
				+ "       01  Pointer-Tst.\n"
				+ "           03 Location-Name       Pic X(35).\n"
				+ "           03 before-blah         PIC S9(8) COMP.\n"
				+ "           03 blah                " + usage.toUpperCase() + ".\n"
				+ "           03 afterblah           PIC X(12).\n";

		
		ICb2XmlBuilder builder = Cb2Xml3.newBuilder(new StringReader(ptrCpy), "Record")
				.setXmlFormat(Cb2xmlConstants.Cb2xmlXmlFormat.CLASSIC)
				.setIndent(true);
		
		if (pointerSize == 8) {
			builder.setDialect(DialectManager.MAINFRAME_COBOL_64_BIT);
		} else {
			builder.setDialect(DialectManager.MAINFRAME_COBOL);
		}

		//System.out.println(builder.asXmlString());
		assertEquals(numericXml, builder.asXmlString());
		
		ICopybookJrUpd cobolItemTree = builder.asCobolItemTree();
		List<? extends IItemJrUpd> childItems = cobolItemTree.getChildItems().get(0).getChildItems();
//			System.out.println(cobolItemTree.getChildItems().size()
//					+ " " + childItems.size());
//			for (IItemJrUpd child : childItems) {
//				System.out.print(child.getStorageLength() + ", " );
//			}
		
		int[] sizes = {35, 4, pointerSize, 12, };
		int idx = 0, pos = 1; 
		for (IItemJrUpd child : childItems) {
			assertEquals(sizes[idx++], child.getStorageLength());
			assertEquals(pos, child.getPosition());
			pos += child.getStorageLength();
		}
		assertEquals(usage, childItems.get(2).getUsage().getName());
	}

	
	private String getXml(int pointerSize, String usage) {
		String dialect = pointerSize == 4 ? "Mainframe" : "Mainframe_64_Bit";
		return
			"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
			"<copybook filename=\"Record\" dialect=\"" 	+ dialect +"\">\n" + 
			"    <item level=\"01\" name=\"Pointer-Tst\" position=\"1\" storage-length=\""
			+ 		(51 + pointerSize) +"\" display-length=\"55\">\n" + 
			"        <item level=\"03\" name=\"Location-Name\" picture=\"X(35)\" position=\"1\" storage-length=\"35\" display-length=\"35\"/>\n" + 
			"        <item level=\"03\" name=\"before-blah\" picture=\"S9(8)\" usage=\"computational\" position=\"36\" storage-length=\"4\" display-length=\"8\" numeric=\"true\" signed=\"true\"/>\n" + 
			"        <item level=\"03\" name=\"blah\" usage=\""
					+ usage + "\" position=\"40\" storage-length=\""
					+  pointerSize +"\" display-length=\"0\"/>\n" + 
			"        <item level=\"03\" name=\"afterblah\" picture=\"X(12)\" position=\""
				+ (40 + pointerSize) +"\" storage-length=\"12\" "
			+ "display-length=\"12\"/>\n" + 
			"    </item>\n" + 
			"</copybook>";

	}
		
}
