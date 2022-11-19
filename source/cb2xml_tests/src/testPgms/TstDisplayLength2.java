package testPgms;

import java.io.IOException;
import java.io.StringReader;

import javax.xml.stream.XMLStreamException;

import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;

public class TstDisplayLength2 {

	static String cbl =
			"       01  cbl.\n"
			+ "         03  f1   pic s9(7) comp-3.\n"
			+ "         03  f2   pic 9(2) comp-3.\n"
			+ "         03  f3   pic s9(3) comp-3.\n"
			+ "         03  f4   pic s9(7) comp-3.\n"
			+ "         03  g1.\n"
			+ "             05  f6   pic xx.\n"
			+ "             05  f7   pic 9(7) comp-3.\n"
			+ "         03  f8  pic xx.\n "
					;
	public static void main(String[] args) throws XMLStreamException, LexerException, IOException, ParserException {
		String s =
				Cb2Xml3.newBuilder(new StringReader(cbl), "tst")
					.setIndent(true)
					.asXmlString();
		
		System.out.println(s);
	}

}
