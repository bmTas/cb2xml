package testPgms.cobol.walker;

import java.io.FileNotFoundException;

import common.Code;
import net.sf.cb2xml.Cb2Xml3;

public class TstCopybookWalker1 {

	public static void main(String[] args) throws FileNotFoundException {
		 CopybookReport copybookListner = new CopybookReport();
		 Cb2Xml3.newBuilder(Code.getFullName("cobolCopybook/cpyComp3_88a.cbl"))
			.setIndent(true)
			.asCobolCopybookWalker()
				.walk(copybookListner);
		 
		 System.out.println("\n");
		 
		 Cb2Xml3.newBuilder(Code.getFullName("cobolCopybook/cpyComp3.cbl"))
			.setIndent(true)
			.asCobolCopybookWalker()
				.walk(copybookListner);

	}

	

}
