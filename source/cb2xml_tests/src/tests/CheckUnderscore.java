package tests;

import java.io.IOException;

import javax.xml.stream.XMLStreamException;

import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.copybookReader.ReadCobolCopybook;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;

public class CheckUnderscore {

	public static void main(String[] args) throws XMLStreamException, LexerException, IOException, ParserException {
		ReadCobolCopybook copybook = Cb2Xml3.newCobolCopybookReader()
				.addFreeFormatCobolText("" +
						" 05 RECORD-Name        PIC X.\n" + 
						"    88 REC_Val         VALUE \"1\".\n");
		
		System.out.println(
				Cb2Xml3.newBuilder(copybook)
					.setIndent(true)
					.asXmlString());
	}
	
	
}
