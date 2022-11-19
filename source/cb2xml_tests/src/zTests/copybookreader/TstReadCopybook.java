package zTests.copybookreader;

import static org.junit.Assert.*;

import java.io.IOException;
import java.io.StringReader;

import org.junit.Test;

import common.Code;
import net.sf.cb2xml.copybookReader.CopybookColumns;
import net.sf.cb2xml.copybookReader.ReadCobolCopybook;

public class TstReadCopybook {

	private String dtar020Cpybook = Code.getFullName("cobolCopybook/DTAR020.cbl");
	
	private String dtar020FreeFormat = ""
			+ "*   RECORD LENGTH IS 27.                                          \n"
			+ "*                                                                 \n"
			+ "        03  DTAR020-KCODE-STORE-KEY.                              \n"
			+ "            05 DTAR020-KEYCODE-NO      PIC X(08).                 \n"
			+ "            05 DTAR020-STORE-NO        PIC S9(03)   COMP-3.       \n"
			+ "        03  DTAR020-DATE               PIC S9(07)   COMP-3.       \n"
			+ "        03  DTAR020-DEPT-NO            PIC S9(03)   COMP-3.       \n"
			+ "        03  DTAR020-QTY-SOLD           PIC S9(9)    COMP-3.       \n"
			+ "        03  DTAR020-SALE-PRICE         PIC S9(9)V99 COMP-3.       \n"
			;
	
	@Test
	public void testDTAR020() throws IOException {
		ReadCobolCopybook rc = new ReadCobolCopybook();
		
		rc.addCobolCopybook(dtar020Cpybook);
		
		//System.out.print(rc.getFreeFormatCopybookText());
		
		assertEquals(dtar020FreeFormat, rc.getFreeFormatCopybookText());
		
		rc = new ReadCobolCopybook();
		rc.setColumns(CopybookColumns.FREE_FORMAT)
			.addCobolCopybook(new StringReader(dtar020FreeFormat));
		assertEquals(dtar020FreeFormat, rc.getFreeFormatCopybookText());
	}
	
	@Test
	public void testMultipleAdditions() throws IOException {
		String cpy1 = " 01  rec-1       pic x(21).\n";
		String cpy2 = " 01  rec-2       pic x(22).\n";
		String cpy3 = " 01  rec-3       pic x(23).\n";
		
		ReadCobolCopybook rc = new ReadCobolCopybook();
		
		rc.addCobolCopybook(new StringReader("123456" + cpy1))
		  .addFreeFormatCobolText(cpy2)
		  .setColumns(CopybookColumns.FREE_FORMAT)
		  .addCobolCopybook(new StringReader(cpy3))
		  ;
		
		assertEquals(cpy1 + cpy2 + cpy3, rc.getFreeFormatCopybookText());
		
		rc = new ReadCobolCopybook();
		
		rc
		  .setColumns(CopybookColumns.FREE_FORMAT)
		  .addCobolCopybook(new StringReader(cpy1))
		  .setColumns(CopybookColumns.STANDARD_COLUMNS)
		  .addCobolCopybook(new StringReader("123456" + cpy2))
		  .addFreeFormatCobolText(cpy3)
		  ;
		
		assertEquals(cpy1 + cpy2 + cpy3, rc.getFreeFormatCopybookText());
		
		rc = new ReadCobolCopybook()
				.setColumns(CopybookColumns.STANDARD_COLUMNS)
				.addCobolCopybook(new StringReader("123456" + cpy1));
		assertEquals(cpy1, rc.getFreeFormatCopybookText());
	}
	
	@Test
	public void testCopy() throws IOException {
		String copyTstCpybook = Code.getFullName("cobolCopybook/xcopy.cbl");
		String expected = ""
				+ "  01  rec-1                 pic x(21).\n"
				+ "\n"
				+ "*  \n"
				+ "  01  rec-2                 pic x(22).\n"
				+ "\n";
		ReadCobolCopybook rc = new ReadCobolCopybook()
									.addCobolCopybook(copyTstCpybook);
		
		//System.out.print(rc.getFreeFormatCopybookText());
		assertEquals(expected, rc.getFreeFormatCopybookText());
		
		copyTstCpybook = Code.getFullName("cobolCopybook/xcopy2.cbl");
		rc = new ReadCobolCopybook()
				.addCobolCopybook(copyTstCpybook);

		//System.out.print(rc.getFreeFormatCopybookText());
		
		expected = ""
				+ "*     copy xcpy2. \n"
				+ "  01  rec-1                 pic x(21).\n"
				+ "\n"
				+ "*     copy xcpy3. \n"
				+ "  01  rec-2                 pic x(22).\n"
				+ "\n";
		assertEquals(expected, rc.getFreeFormatCopybookText());

	}

}
