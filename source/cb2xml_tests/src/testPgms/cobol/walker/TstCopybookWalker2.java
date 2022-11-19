package testPgms.cobol.walker;

import java.io.FileNotFoundException;

import common.Code;
import net.sf.cb2xml.Cb2Xml3;

public class TstCopybookWalker2 {

	public static void main(String[] args) throws FileNotFoundException {
		 CopybookReport copybookListner = new CopybookReport();
		 Cb2Xml3.newBuilder(Code.getFullName("cobolCopybook/cbl2xml_Test101.cbl"))
			.setIndent(true)
			.asCobolCopybookWalker()
				.walk(copybookListner);
		 
		 System.out.println("\n");
		 

	}

	

}
