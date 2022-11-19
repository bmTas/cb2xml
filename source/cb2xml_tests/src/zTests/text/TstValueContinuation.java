package zTests.text;

import static org.junit.Assert.*;

import java.io.IOException;
import java.io.StringReader;

import javax.xml.stream.XMLStreamException;

import org.junit.Test;

import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.ICb2XmlBuilder;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;

public class TstValueContinuation {

	static final String CBL
				= "       01  ROOT.                                                    \n"
				+ "           05 SUBLEVEL             PIC X.\n"
				+ "                88 FIRST-VALUE     VALUE 'S'.\n"
				+ "                88 SECOND-VALUE  \n"
				+ "                                   VALUE 'S' 'T' 'U' 'V' 'W' 'X' 'Y' 'Z\n"
				+ "      -             '' 'B' 'C' 'D' 'E' 'F' 'G'.\n";

	static final String EXPECTED_XML
				= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
				+ "<copybook filename=\"Record\" dialect=\"Mainframe\" cb2xml-format=\"2017\">\n"
				+ "    <item level=\"01\" name=\"ROOT\" position=\"1\" storage-length=\"1\" display-length=\"1\" display-position=\"1\">\n"
				+ "        <item level=\"05\" name=\"SUBLEVEL\" picture=\"X\" position=\"1\" storage-length=\"1\" display-length=\"1\" display-position=\"1\">\n"
				+ "            <condition name=\"FIRST-VALUE\">\n"
				+ "                <condition value=\"&quot;S&quot;\"/>\n"
				+ "            </condition>\n"
				+ "            <condition name=\"SECOND-VALUE\">\n"
				+ "                <condition value=\"&quot;S&quot;\"/>\n"
				+ "                <condition value=\"&quot;T&quot;\"/>\n"
				+ "                <condition value=\"&quot;U&quot;\"/>\n"
				+ "                <condition value=\"&quot;V&quot;\"/>\n"
				+ "                <condition value=\"&quot;W&quot;\"/>\n"
				+ "                <condition value=\"&quot;X&quot;\"/>\n"
				+ "                <condition value=\"&quot;Y&quot;\"/>\n"
				+ "                <condition value=\"&quot;Z&quot;\"/>\n"
				+ "                <condition value=\"&quot;B&quot;\"/>\n"
				+ "                <condition value=\"&quot;C&quot;\"/>\n"
				+ "                <condition value=\"&quot;D&quot;\"/>\n"
				+ "                <condition value=\"&quot;E&quot;\"/>\n"
				+ "                <condition value=\"&quot;F&quot;\"/>\n"
				+ "                <condition value=\"&quot;G&quot;\"/>\n"
				+ "            </condition>\n"
				+ "        </item>\n"
				+ "    </item>\n"
				+ "</copybook>";
	
	@Test
	public void test() throws XMLStreamException, LexerException, IOException, ParserException {
		ICb2XmlBuilder bldr = Cb2Xml3.newBuilder(new StringReader(CBL), "Record")
				//.setDebug(true)
				.setIndent(true);

		assertEquals(EXPECTED_XML, bldr.asXmlString());
	}

}
