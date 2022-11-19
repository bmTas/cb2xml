package testPgms;

import java.io.IOException;
import java.io.StringReader;

import javax.xml.stream.XMLStreamException;

import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.copybookReader.CopybookColumns;
import net.sf.cb2xml.copybookReader.ReadCobolCopybook;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;

public class TstCopybookReader3 {

	public static void main(String[] args) throws IOException, XMLStreamException, LexerException, ParserException {
		ReadCobolCopybook copybook = new ReadCobolCopybook()
				.setDirectoriesToSearch("/home/bruce/work/Cobol/CopyBooks")
				.setColumns(CopybookColumns.STANDARD_COLUMNS)
				.addCobolCopybook("/home/bruce/work/Cobol/Main01.cbl")
				.setColumns(CopybookColumns.FREE_FORMAT)		
				.addCobolCopybook("/home/bruce/work/Cobol/Main02.cbl")
				.addCobolCopybook(new StringReader("\n"
						+ "   03 Extra-Field-01   pic x(10)."
						+ "   03 Extra-Field-02   pic s9(7)v99 comp-3."))
				;

		System.out.println(
			Cb2Xml3.newBuilder(copybook.getFreeFormatCopybookReader(), "Main01")
					.setCobolLineFormat(Cb2xmlConstants.FREE_FORMAT)
					.setIndent(true)
					.asXmlString()
				);

	}

}
