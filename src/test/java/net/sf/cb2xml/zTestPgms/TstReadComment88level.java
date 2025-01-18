package net.sf.cb2xml.zTestPgms;

import java.io.IOException;

import javax.xml.stream.XMLStreamException;

import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.copybookReader.ReadCobolCopybook;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;

public class TstReadComment88level {

	String copybook = ""
			+ " 01  FOO.\n"
			+ "  * Code assigned by M \n"
			+ "     03 CODE-NAME             pic 999.\n"
			+ "  * comment for Mr Bond\n"
			+ "        88 IS-JAMES-BOND value 007.\n"
			+ "  * comment for the devil\n"
			+ "        88 IS-THE-DEVIL value 666.\n"
			+ "  * comment xx\n"
			+ "       88 xxx value       123\n"
			+ "  * comment xx2\n"
			+ "                          456.\n";
	
	public TstReadComment88level() throws XMLStreamException, LexerException, IOException, ParserException {
		String s = Cb2Xml3.newBuilder(
				new ReadCobolCopybook()
					.setCopybookName("FOO")
					.addFreeFormatCobolText(copybook)
				)
		    .setIndent(true)
		    .asXmlString();
		
		System.out.println(s);
	}
	public static void main(String[] args) throws XMLStreamException, LexerException, IOException, ParserException {
		new TstReadComment88level() ;
	}

}
