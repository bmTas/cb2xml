package testPgms;

import java.io.IOException;

import javax.xml.stream.XMLStreamException;

import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.copybookReader.CopybookColumns;
import net.sf.cb2xml.copybookReader.ReadCobolCopybook;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;

public class TstCopybookReader2 {

	public static void main(String[] args) throws IOException, XMLStreamException, LexerException, ParserException {
		ReadCobolCopybook copybook = new ReadCobolCopybook()
				.setDirectoriesToSearch("/home/bruce/work/Cobol/CopyBooks")
				.setColumns(CopybookColumns.STANDARD_COLUMNS)
				.addCobolCopybook("/home/bruce/work/Cobol/Main01.cbl");
		System.out.println(copybook.getFreeFormatCopybookText());
		System.out.println(
			Cb2Xml3.newBuilder(copybook.getFreeFormatCopybookReader(), "Main01")
					.setCobolLineFormat(Cb2xmlConstants.FREE_FORMAT)
					.setIndent(true)
					.asXmlString()
				);

	}

}
