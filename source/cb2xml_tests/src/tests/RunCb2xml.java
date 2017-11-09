package tests;

import net.sf.cb2xml.Cb2Xml;

public class RunCb2xml {

	public RunCb2xml() {
		// TODO Auto-generated constructor stub
	}

	public static void main(String[] args) {
		//String[] a = {"/media/sf_Shared/JRecordLib/Xmpl.cbl"};
		//String[] a = {"-cobol", "/media/sf_Shared/JRecordLib/Xmpl.cbl", "-stacksize", "normal"};
		String[] a = {"-cobol", "/media/sf_Shared/JRecordLib/Xmpl.cbl"};
		Cb2Xml.main(a);
	}

}
