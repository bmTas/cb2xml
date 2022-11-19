package zTests.cb2xmlReader;

import static org.junit.Assert.*;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;

import javax.xml.stream.XMLStreamException;

import org.junit.Test;

import common.Code;
import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.analysis.Copybook;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;
import net.sf.cb2xml.util.Cb2xmlReader;
import net.sf.cb2xml.util.WriteXml;

public class TestCb2XmlReader01 {

	
	private final String[] COPYBOOK_LIST1 = {
			"cpyRightJust.cbl",
			"cpyComp3_88a.cbl",
			"cpyComp3_88b.cbl",
			"cpyComp3aInheritence.cbl",
			"cpyComp.cbl",
			"cpyComp3.cbl",
			"cpyComp5.cbl",
			"cpyCompP.cbl",
			"cpyComp3P.cbl", 
			"cpyComp5P.cbl",
			"cpyComp3Inheritence.cbl",
			"cpyCompInheritence.cbl",
	};

	private String[] COPYBOOK_LIST2 = {
			"cpyCompSync.cbl",
			"cpyComp3Sync.cbl",
			"cpyComp5Sync.cbl",
	};

	private String[] COPYBOOK_LIST3 = {
			"cpyUnxdetail.cbl",
			"cpyOccurs.cbl",
			"cpyOccursDepending.cbl",
			"cpyOccursDependingOn21.cbl",
			"cpyOccursDependingOn29.cbl",
			"cpyOccursDependingOn42.cbl",
			"cpyBitOfEverything.cbl",
	};
	
	
	private String[] COPYBOOK_LIST4 = {
			"cpyRedefSize.cbl",
			"cpyRedefSize01.cbl",
			"cpyRedefSize02.cbl",
			"cpyRedefSize03.cbl",
			"cpyRedefSize08.cbl",
			"cpyRedefSize11.cbl",
			"cpyRedefSize12.cbl",
			"cpyRedefSize14.cbl",
			"cpyRedefSize15.cbl",
			"cpyRedefSize21.cbl",
			"cpyRedefSize23.cbl",
			"cpyRedefSize24.cbl",
			"cpyRedefSize25.cbl",
			"cpyRedefSize31.cbl",
			"cpyValueContinuation1.cbl",
			"cpyValueContinuation2.cbl",
			"cpyValueContinuation.cbl",
			"cpyMsg.cbl",
			"cpyHexValue.cbl",
			"cpyPicNG01.cbl",
			"cpyPicNG02.cbl",
			"cpyPicNG03.cbl",
	};


	@Test
	public void test1() throws FileNotFoundException, XMLStreamException, LexerException, IOException, ParserException {
		
		doTests(COPYBOOK_LIST1);
	}

	@Test
	public void test2() throws FileNotFoundException, XMLStreamException, LexerException, IOException, ParserException {
		
		doTests(COPYBOOK_LIST2);
	}
	@Test
	public void test3() throws FileNotFoundException, XMLStreamException, LexerException, IOException, ParserException {
		
		doTests(COPYBOOK_LIST3);
	}

	@Test
	public void test4() throws FileNotFoundException, XMLStreamException, LexerException, IOException, ParserException {
		
		doTests(COPYBOOK_LIST4);
	}

	private void doTests(String[] copybookList)
			throws FileNotFoundException, XMLStreamException, LexerException, IOException, ParserException {
		for (String cpy : copybookList) {
			doTest(Code.getFullName("cobolCopybook/" + cpy));
		}
	}
	
	
	private void doTest(String copybookFilename) throws FileNotFoundException, XMLStreamException, LexerException, IOException, ParserException {
		StringWriter writer = new StringWriter();
		Cb2Xml3.newBuilder(copybookFilename)
			.setIndent(true)
			.writeXml(writer);
		String cb2xml1 = writer.toString();
		
		Copybook copybook = (new Cb2xmlReader()).parseCobolCopybook(new StringReader(cb2xml1));
		
		writer = new StringWriter();
		(new WriteXml(true, true, Cb2xmlConstants.STANDARD_FONT))
				.writeCopybook(writer, copybook, true);
		String cb2xml2 = writer.toString();
		
		assertEquals(cb2xml1, cb2xml2);
	}

}
