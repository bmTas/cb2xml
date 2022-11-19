package testPgms;

import java.io.IOException;

import net.sf.cb2xml.copybookReader.CopybookColumns;
import net.sf.cb2xml.copybookReader.ReadCobolCopybook;

public class TstCopybookReader {

	public static void main(String[] args) throws IOException {
		ReadCobolCopybook copybook = new ReadCobolCopybook()
				.setColumns(CopybookColumns.STANDARD_COLUMNS)
				.addCobolCopybook("/home/bruce/work/Cobol/CopyBooks/Main.cbl");
		System.out.println(copybook.getFreeFormatCopybookText());

	}

}
