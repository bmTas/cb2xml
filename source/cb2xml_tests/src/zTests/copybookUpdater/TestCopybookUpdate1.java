package zTests.copybookUpdater;

import java.io.FileNotFoundException;
import java.io.IOException;

import javax.xml.stream.XMLStreamException;

import org.junit.Test;

public class TestCopybookUpdate1 {

	@Test
	public void test1() throws FileNotFoundException, XMLStreamException, IOException {
		RunUpdateTest rt = new RunUpdateTest();
		RunUpdateTest rtBasic = new RunUpdateTest(false);
	
		for (String cpy : RunUpdateTest.COMP_TESTS) {
			rt.doTestCopybookStr(cpy + RunUpdateTest.LAST_FIELD, "Test");
			rtBasic.doTestCopybookStr(cpy + RunUpdateTest.LAST_FIELD, "Test");
		}
	}

	@Test
	public void test2() throws FileNotFoundException, XMLStreamException, IOException {
		RunUpdateTest rt = new RunUpdateTest();
		RunUpdateTest rtBasic = new RunUpdateTest(false);

		for (int i = 1; i < RunUpdateTest.COMP_TESTS.length; i++) {
			String cpy = RunUpdateTest.COMP_TESTS[0] + RunUpdateTest.COMP_TESTS[i] + RunUpdateTest.LAST_FIELD;
			rt.doTestCopybookStr(cpy, "Test");
			rtBasic.doTestCopybookStr(cpy, "Test");
		}
	}

	@Test
	public void test3() throws FileNotFoundException, XMLStreamException, IOException {
		RunUpdateTest rt = new RunUpdateTest();
		RunUpdateTest rtBasic = new RunUpdateTest(false);
		StringBuilder copybook = new StringBuilder();
		
		for (String cpy : RunUpdateTest.COMP_TESTS) {
			copybook.append(cpy);
			rt.doTestCopybookStr(copybook + RunUpdateTest.LAST_FIELD, "Test");
			rtBasic.doTestCopybookStr(copybook + RunUpdateTest.LAST_FIELD, "Test");
		}
	}


}
