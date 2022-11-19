package zTests.copybookUpdater;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;

import javax.xml.stream.XMLStreamException;

import org.junit.Test;

import common.Code;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;


public class TestCopybookUpdate2 {

	private static final String[] COPYBOOK_LIST1 = {
			"cpyComp.cbl",
			"cpyComp3.cbl",
			"cpyComp5.cbl",
			"cpyComp5P.cbl",
			"cpyCompSync.cbl",
			"cpyComp5Sync.cbl",
	};
	
	private static final String[] INHERRETED_COMP_LIST1 = {
			"cpyComp3Inheritence.cbl",
			"cpyComp3aInheritence.cbl"
	};
	
	
	private static final String[] LEVEL_88_LIST1 = {
			"Test_88.cbl",
			"cpyComp3_88a.cbl",
			"cpyComp3_88b.cbl",
//			"cbl2xml_Test101.cbl",
	};

	
	@Test
	public void test1() throws FileNotFoundException, XMLStreamException, LexerException, IOException, ParserException {
		
		doTests(COPYBOOK_LIST1, true);
	}

	
	@Test
	public void test2() throws FileNotFoundException, XMLStreamException, LexerException, IOException, ParserException {
		
		doTests(INHERRETED_COMP_LIST1, true);
	}

	
	@Test
	public void test3() throws FileNotFoundException, XMLStreamException, LexerException, IOException, ParserException {
		
		doTests(LEVEL_88_LIST1, true);
	}
	
	@Test
	public void test11() throws FileNotFoundException, XMLStreamException, LexerException, IOException, ParserException {
		
		doTests(COPYBOOK_LIST1, false);
	}

	
	@Test
	public void test12() throws FileNotFoundException, XMLStreamException, LexerException, IOException, ParserException {
		
		doTests(INHERRETED_COMP_LIST1, false);
	}

	
	@Test
	public void test13() throws FileNotFoundException, XMLStreamException, LexerException, IOException, ParserException {
		
		doTests(LEVEL_88_LIST1, false);
	}

	
	private void doTests(String[] copybookList, boolean std)
			throws FileNotFoundException, XMLStreamException, LexerException, IOException, ParserException {
		
		RunUpdateTest runUpdateTest = new RunUpdateTest(std);
		for (String cpy : copybookList) {
			String copybookStr = readFile(Code.getFullName("cobolCopybook/" + cpy));
			runUpdateTest.doTestCopybookStr(copybookStr, cpy);
		}
	}

	
	private String readFile(String fileName) throws IOException {
		StringBuffer fileStr = new StringBuffer(5000);
		
		String s;
		
		BufferedReader r = new BufferedReader(new InputStreamReader(new FileInputStream(fileName)));
		
		while ((s = r.readLine()) != null) {
			fileStr.append(s).append('\n');
		}
		r.close();
	
		return fileStr.toString();
	}

}
