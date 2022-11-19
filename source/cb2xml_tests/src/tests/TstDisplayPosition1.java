package tests;

import static org.junit.Assert.*;

import java.io.IOException;
import java.io.StringReader;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import javax.xml.stream.XMLStreamException;

import org.junit.Test;

import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.ICb2XmlBuilder;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.ICopybook;
import net.sf.cb2xml.def.IItem;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;

public class TstDisplayPosition1 {

	String end1
			= "          03  field-5              pic spvpp999.\n"
			+ "          03  field-6              pic s999.\n"
			+ "          03  field-7              pic xxx.\n";
	String group1
			= "          03  Group-1.\n"
			+ "              05 Group-2.\n"
			+ "                 10  area-1               pic x(10).\n"
			+ "                 10  redef-1    redefines area-1.\n"
			+ "                     15  field-2          pic s9(7) comp-3.\n"
			+ "                     15  field-3          pic s9(5)V99 comp-3.\n"
			+ "                 10  redef-2    redefines area-1\n"
			+ "                                   pic x(5).\n";
	
	String group2
			= "            03  Group-10.\n"
			+ "                05 array-1 occurs 10 times.\n"
			+ "                   10 field-10              pic s9(4).\n"
			+ "                   10 array-2   occurs 5.\n"
			+ "                      15 field-11           pic s99 comp.\n"
			+ "                      15 field-12  occurs 5 pic xx.\n"
			+ "                      15 field-14           pic s9(4).\n"
			+ "                   10 group-11.\n"
			+ "                      15 field-15           pic s99 comp.\n"
			+ "                      15 field-16  occurs 5 pic xx.\n"
			+ "                   10 array-2      occurs 5.\n"
			+ "                      15 field-17           pic s99 comp.\n"
			+ "                05 field-20                 pic xx.\n"
			+ "                05 group-12.\n"
			+ "                   10 field-21   occurs 5   pic xxx.\n"
			+ "                   10 field-22              pic 999.\n";
	String start2
			= "      01  tst-1.\n"
			+ "          03  field-1              pic s9(5) comp-3.\n"
			+ group1;
	
	String displayPosCobol1
			= "      01  tst-1.\n"
			+ "          03  field-1              pic s9(5) comp-3.\n"
			+ "          03  area-1               pic x(10).\n"
			+ "          03  redef-1    redefines area-1.\n"
			+ "              05  field-2          pic s9(7) comp-3.\n"
			+ "              05  field-3          pic s9(5)V99 comp-3.\n"
			+ "          03  redef-2    redefines area-1\n"
			+ "                                   pic x(5).\n"
			+ "          03  field-4              pic s999ppp.\n"
			+ end1;
	String displayPosCobol2
			= start2
			+ "          03  field-4              pic s999ppp.\n"
			+ end1;
	String displayPosCobol3
			= start2
			+ "              05  field-4              pic s999ppp.\n"
			+ end1;
	String displayPosCobol4
			= "      01  tst-1.\n"
			+ "          03  field-1              pic s9(5) comp-3.\n"
			+ group1
			+ group2
			+ group1
			+ group2
			+ "          03  field-40             pic s999ppp.\n"

			;
	
	String displayPosXml1c
			= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<copybook filename=\"Record\" dialect=\"Mainframe\">\n"
			+ "    <item level=\"01\" name=\"tst-1\" position=\"1\" storage-length=\"25\" display-length=\"37\">\n"
			+ "        <item level=\"03\" name=\"field-1\" picture=\"s9(5)\" usage=\"computational-3\" position=\"1\" storage-length=\"3\" display-length=\"5\" numeric=\"true\" signed=\"true\"/>\n"
			+ "        <item level=\"03\" name=\"area-1\" picture=\"x(10)\" position=\"4\" storage-length=\"10\" display-length=\"10\" redefined=\"true\"/>\n"
			+ "        <item level=\"03\" name=\"redef-1\" position=\"4\" storage-length=\"8\" display-length=\"14\" redefines=\"area-1\">\n"
			+ "            <item level=\"05\" name=\"field-2\" picture=\"s9(7)\" usage=\"computational-3\" position=\"4\" storage-length=\"4\" display-length=\"7\" numeric=\"true\" signed=\"true\"/>\n"
			+ "            <item level=\"05\" name=\"field-3\" picture=\"s9(5)V99\" usage=\"computational-3\" position=\"8\" storage-length=\"4\" display-length=\"7\" scale=\"2\" numeric=\"true\" signed=\"true\"/>\n"
			+ "        </item>\n"
			+ "        <item level=\"03\" name=\"redef-2\" picture=\"x(5)\" position=\"4\" storage-length=\"5\" display-length=\"5\" redefines=\"area-1\"/>\n"
			+ "        <item level=\"03\" name=\"field-4\" picture=\"s999ppp\" position=\"14\" storage-length=\"3\" display-length=\"6\" scale=\"-3\" numeric=\"true\" signed=\"true\"/>\n"
			+ "        <item level=\"03\" name=\"field-5\" picture=\"spvpp999\" position=\"17\" storage-length=\"3\" display-length=\"6\" scale=\"5\" numeric=\"true\" signed=\"true\"/>\n"
			+ "        <item level=\"03\" name=\"field-6\" picture=\"s999\" position=\"20\" storage-length=\"3\" display-length=\"3\" numeric=\"true\" signed=\"true\"/>\n"
			+ "        <item level=\"03\" name=\"field-7\" picture=\"xxx\" position=\"23\" storage-length=\"3\" display-length=\"3\"/>\n"
			+ "    </item>\n"
			+ "</copybook>";
	
	String displayPosXml1n
			= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<copybook filename=\"Record\" dialect=\"Mainframe\" cb2xml-format=\"2017\">\n"
			+ "    <item level=\"01\" name=\"tst-1\" position=\"1\" storage-length=\"25\" display-length=\"37\" display-position=\"1\">\n"
			+ "        <item level=\"03\" name=\"field-1\" picture=\"s9(5)\" usage=\"computational-3\" position=\"1\" storage-length=\"3\" display-length=\"5\" numeric=\"COBOL_NUMERIC\" display-position=\"1\"/>\n"
			+ "        <item level=\"03\" name=\"area-1\" picture=\"x(10)\" position=\"4\" storage-length=\"10\" display-length=\"10\" display-position=\"6\" redefined=\"true\"/>\n"
			+ "        <item level=\"03\" name=\"redef-1\" position=\"4\" storage-length=\"8\" display-length=\"14\" display-position=\"6\" redefines=\"area-1\">\n"
			+ "            <item level=\"05\" name=\"field-2\" picture=\"s9(7)\" usage=\"computational-3\" position=\"4\" storage-length=\"4\" display-length=\"7\" numeric=\"COBOL_NUMERIC\" display-position=\"6\"/>\n"
			+ "            <item level=\"05\" name=\"field-3\" picture=\"s9(5)V99\" usage=\"computational-3\" position=\"8\" storage-length=\"4\" display-length=\"7\" scale=\"2\" numeric=\"COBOL_NUMERIC\" display-position=\"13\"/>\n"
			+ "        </item>\n"
			+ "        <item level=\"03\" name=\"redef-2\" picture=\"x(5)\" position=\"4\" storage-length=\"5\" display-length=\"5\" display-position=\"6\" redefines=\"area-1\"/>\n"
			+ "        <item level=\"03\" name=\"field-4\" picture=\"s999ppp\" position=\"14\" storage-length=\"3\" display-length=\"6\" scale=\"-3\" numeric=\"COBOL_NUMERIC\" display-position=\"20\"/>\n"
			+ "        <item level=\"03\" name=\"field-5\" picture=\"spvpp999\" position=\"17\" storage-length=\"3\" display-length=\"6\" scale=\"5\" numeric=\"COBOL_NUMERIC\" display-position=\"26\"/>\n"
			+ "        <item level=\"03\" name=\"field-6\" picture=\"s999\" position=\"20\" storage-length=\"3\" display-length=\"3\" numeric=\"COBOL_NUMERIC\" display-position=\"32\"/>\n"
			+ "        <item level=\"03\" name=\"field-7\" picture=\"xxx\" position=\"23\" storage-length=\"3\" display-length=\"3\" display-position=\"35\"/>\n"
			+ "    </item>\n"
			+ "</copybook>";
	String displayPosXml2n
			= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<copybook filename=\"Record\" dialect=\"Mainframe\" cb2xml-format=\"2017\">\n"
			+ "    <item level=\"01\" name=\"tst-1\" position=\"1\" storage-length=\"25\" display-length=\"37\" display-position=\"1\">\n"
			+ "        <item level=\"03\" name=\"field-1\" picture=\"s9(5)\" usage=\"computational-3\" position=\"1\" storage-length=\"3\" display-length=\"5\" numeric=\"COBOL_NUMERIC\" display-position=\"1\"/>\n"
			+ "        <item level=\"03\" name=\"Group-1\" position=\"4\" storage-length=\"10\" display-length=\"14\" display-position=\"6\">\n"
			+ "            <item level=\"05\" name=\"Group-2\" position=\"4\" storage-length=\"10\" display-length=\"14\" display-position=\"6\">\n"
			+ "                <item level=\"10\" name=\"area-1\" picture=\"x(10)\" position=\"4\" storage-length=\"10\" display-length=\"10\" display-position=\"6\" redefined=\"true\"/>\n"
			+ "                <item level=\"10\" name=\"redef-1\" position=\"4\" storage-length=\"8\" display-length=\"14\" display-position=\"6\" redefines=\"area-1\">\n"
			+ "                    <item level=\"15\" name=\"field-2\" picture=\"s9(7)\" usage=\"computational-3\" position=\"4\" storage-length=\"4\" display-length=\"7\" numeric=\"COBOL_NUMERIC\" display-position=\"6\"/>\n"
			+ "                    <item level=\"15\" name=\"field-3\" picture=\"s9(5)V99\" usage=\"computational-3\" position=\"8\" storage-length=\"4\" display-length=\"7\" scale=\"2\" numeric=\"COBOL_NUMERIC\" display-position=\"13\"/>\n"
			+ "                </item>\n"
			+ "                <item level=\"10\" name=\"redef-2\" picture=\"x(5)\" position=\"4\" storage-length=\"5\" display-length=\"5\" display-position=\"6\" redefines=\"area-1\"/>\n"
			+ "            </item>\n"
			+ "        </item>\n"
			+ "        <item level=\"03\" name=\"field-4\" picture=\"s999ppp\" position=\"14\" storage-length=\"3\" display-length=\"6\" scale=\"-3\" numeric=\"COBOL_NUMERIC\" display-position=\"20\"/>\n"
			+ "        <item level=\"03\" name=\"field-5\" picture=\"spvpp999\" position=\"17\" storage-length=\"3\" display-length=\"6\" scale=\"5\" numeric=\"COBOL_NUMERIC\" display-position=\"26\"/>\n"
			+ "        <item level=\"03\" name=\"field-6\" picture=\"s999\" position=\"20\" storage-length=\"3\" display-length=\"3\" numeric=\"COBOL_NUMERIC\" display-position=\"32\"/>\n"
			+ "        <item level=\"03\" name=\"field-7\" picture=\"xxx\" position=\"23\" storage-length=\"3\" display-length=\"3\" display-position=\"35\"/>\n"
			+ "    </item>\n"
			+ "</copybook>";
	String displayPosXml3n
			= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<copybook filename=\"Record\" dialect=\"Mainframe\" cb2xml-format=\"2017\">\n"
			+ "    <item level=\"01\" name=\"tst-1\" position=\"1\" storage-length=\"25\" display-length=\"37\" display-position=\"1\">\n"
			+ "        <item level=\"03\" name=\"field-1\" picture=\"s9(5)\" usage=\"computational-3\" position=\"1\" storage-length=\"3\" display-length=\"5\" numeric=\"COBOL_NUMERIC\" display-position=\"1\"/>\n"
			+ "        <item level=\"03\" name=\"Group-1\" position=\"4\" storage-length=\"13\" display-length=\"20\" display-position=\"6\">\n"
			+ "            <item level=\"05\" name=\"Group-2\" position=\"4\" storage-length=\"10\" display-length=\"14\" display-position=\"6\">\n"
			+ "                <item level=\"10\" name=\"area-1\" picture=\"x(10)\" position=\"4\" storage-length=\"10\" display-length=\"10\" display-position=\"6\" redefined=\"true\"/>\n"
			+ "                <item level=\"10\" name=\"redef-1\" position=\"4\" storage-length=\"8\" display-length=\"14\" display-position=\"6\" redefines=\"area-1\">\n"
			+ "                    <item level=\"15\" name=\"field-2\" picture=\"s9(7)\" usage=\"computational-3\" position=\"4\" storage-length=\"4\" display-length=\"7\" numeric=\"COBOL_NUMERIC\" display-position=\"6\"/>\n"
			+ "                    <item level=\"15\" name=\"field-3\" picture=\"s9(5)V99\" usage=\"computational-3\" position=\"8\" storage-length=\"4\" display-length=\"7\" scale=\"2\" numeric=\"COBOL_NUMERIC\" display-position=\"13\"/>\n"
			+ "                </item>\n"
			+ "                <item level=\"10\" name=\"redef-2\" picture=\"x(5)\" position=\"4\" storage-length=\"5\" display-length=\"5\" display-position=\"6\" redefines=\"area-1\"/>\n"
			+ "            </item>\n"
			+ "            <item level=\"05\" name=\"field-4\" picture=\"s999ppp\" position=\"14\" storage-length=\"3\" display-length=\"6\" scale=\"-3\" numeric=\"COBOL_NUMERIC\" display-position=\"20\"/>\n"
			+ "        </item>\n"
			+ "        <item level=\"03\" name=\"field-5\" picture=\"spvpp999\" position=\"17\" storage-length=\"3\" display-length=\"6\" scale=\"5\" numeric=\"COBOL_NUMERIC\" display-position=\"26\"/>\n"
			+ "        <item level=\"03\" name=\"field-6\" picture=\"s999\" position=\"20\" storage-length=\"3\" display-length=\"3\" numeric=\"COBOL_NUMERIC\" display-position=\"32\"/>\n"
			+ "        <item level=\"03\" name=\"field-7\" picture=\"xxx\" position=\"23\" storage-length=\"3\" display-length=\"3\" display-position=\"35\"/>\n"
			+ "    </item>\n"
			+ "</copybook>";
	String displayPosXml4n
			= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<copybook filename=\"Record\" dialect=\"Mainframe\" cb2xml-format=\"2017\">\n"
			+ "    <item level=\"01\" name=\"tst-1\" position=\"1\" storage-length=\"2186\" display-length=\"2199\" display-position=\"1\">\n"
			+ "        <item level=\"03\" name=\"field-1\" picture=\"s9(5)\" usage=\"computational-3\" position=\"1\" storage-length=\"3\" display-length=\"5\" numeric=\"COBOL_NUMERIC\" display-position=\"1\"/>\n"
			+ "        <item level=\"03\" name=\"Group-1\" position=\"4\" storage-length=\"10\" display-length=\"14\" display-position=\"6\">\n"
			+ "            <item level=\"05\" name=\"Group-2\" position=\"4\" storage-length=\"10\" display-length=\"14\" display-position=\"6\">\n"
			+ "                <item level=\"10\" name=\"area-1\" picture=\"x(10)\" position=\"4\" storage-length=\"10\" display-length=\"10\" display-position=\"6\" redefined=\"true\"/>\n"
			+ "                <item level=\"10\" name=\"redef-1\" position=\"4\" storage-length=\"8\" display-length=\"14\" display-position=\"6\" redefines=\"area-1\">\n"
			+ "                    <item level=\"15\" name=\"field-2\" picture=\"s9(7)\" usage=\"computational-3\" position=\"4\" storage-length=\"4\" display-length=\"7\" numeric=\"COBOL_NUMERIC\" display-position=\"6\"/>\n"
			+ "                    <item level=\"15\" name=\"field-3\" picture=\"s9(5)V99\" usage=\"computational-3\" position=\"8\" storage-length=\"4\" display-length=\"7\" scale=\"2\" numeric=\"COBOL_NUMERIC\" display-position=\"13\"/>\n"
			+ "                </item>\n"
			+ "                <item level=\"10\" name=\"redef-2\" picture=\"x(5)\" position=\"4\" storage-length=\"5\" display-length=\"5\" display-position=\"6\" redefines=\"area-1\"/>\n"
			+ "            </item>\n"
			+ "        </item>\n"
			+ "        <item level=\"03\" name=\"Group-10\" position=\"14\" storage-length=\"1080\" display-length=\"1080\" display-position=\"20\">\n"
			+ "            <item level=\"05\" name=\"array-1\" occurs=\"10\" position=\"14\" storage-length=\"106\" display-length=\"106\" display-position=\"20\">\n"
			+ "                <item level=\"10\" name=\"field-10\" picture=\"s9(4)\" position=\"14\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\" display-position=\"20\"/>\n"
			+ "                <item level=\"10\" name=\"array-2\" occurs=\"5\" position=\"18\" storage-length=\"16\" display-length=\"16\" display-position=\"24\">\n"
			+ "                    <item level=\"15\" name=\"field-11\" picture=\"s99\" usage=\"computational\" position=\"18\" storage-length=\"2\" display-length=\"2\" numeric=\"COBOL_NUMERIC\" display-position=\"24\"/>\n"
			+ "                    <item level=\"15\" name=\"field-12\" occurs=\"5\" picture=\"xx\" position=\"20\" storage-length=\"2\" display-length=\"2\" display-position=\"26\"/>\n"
			+ "                    <item level=\"15\" name=\"field-14\" picture=\"s9(4)\" position=\"30\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\" display-position=\"36\"/>\n"
			+ "                </item>\n"
			+ "                <item level=\"10\" name=\"group-11\" position=\"98\" storage-length=\"12\" display-length=\"12\" display-position=\"104\">\n"
			+ "                    <item level=\"15\" name=\"field-15\" picture=\"s99\" usage=\"computational\" position=\"98\" storage-length=\"2\" display-length=\"2\" numeric=\"COBOL_NUMERIC\" display-position=\"104\"/>\n"
			+ "                    <item level=\"15\" name=\"field-16\" occurs=\"5\" picture=\"xx\" position=\"100\" storage-length=\"2\" display-length=\"2\" display-position=\"106\"/>\n"
			+ "                </item>\n"
			+ "                <item level=\"10\" name=\"array-2\" occurs=\"5\" position=\"110\" storage-length=\"2\" display-length=\"2\" display-position=\"116\">\n"
			+ "                    <item level=\"15\" name=\"field-17\" picture=\"s99\" usage=\"computational\" position=\"110\" storage-length=\"2\" display-length=\"2\" numeric=\"COBOL_NUMERIC\" display-position=\"116\"/>\n"
			+ "                </item>\n"
			+ "            </item>\n"
			+ "            <item level=\"05\" name=\"field-20\" picture=\"xx\" position=\"1074\" storage-length=\"2\" display-length=\"2\" display-position=\"1080\"/>\n"
			+ "            <item level=\"05\" name=\"group-12\" position=\"1076\" storage-length=\"18\" display-length=\"18\" display-position=\"1082\">\n"
			+ "                <item level=\"10\" name=\"field-21\" occurs=\"5\" picture=\"xxx\" position=\"1076\" storage-length=\"3\" display-length=\"3\" display-position=\"1082\"/>\n"
			+ "                <item level=\"10\" name=\"field-22\" picture=\"999\" position=\"1091\" storage-length=\"3\" display-length=\"3\" numeric=\"COBOL_NUMERIC\" display-position=\"1097\"/>\n"
			+ "            </item>\n"
			+ "        </item>\n"
			+ "        <item level=\"03\" name=\"Group-1\" position=\"1094\" storage-length=\"10\" display-length=\"14\" display-position=\"1100\">\n"
			+ "            <item level=\"05\" name=\"Group-2\" position=\"1094\" storage-length=\"10\" display-length=\"14\" display-position=\"1100\">\n"
			+ "                <item level=\"10\" name=\"area-1\" picture=\"x(10)\" position=\"1094\" storage-length=\"10\" display-length=\"10\" display-position=\"1100\" redefined=\"true\"/>\n"
			+ "                <item level=\"10\" name=\"redef-1\" position=\"1094\" storage-length=\"8\" display-length=\"14\" display-position=\"1100\" redefines=\"area-1\">\n"
			+ "                    <item level=\"15\" name=\"field-2\" picture=\"s9(7)\" usage=\"computational-3\" position=\"1094\" storage-length=\"4\" display-length=\"7\" numeric=\"COBOL_NUMERIC\" display-position=\"1100\"/>\n"
			+ "                    <item level=\"15\" name=\"field-3\" picture=\"s9(5)V99\" usage=\"computational-3\" position=\"1098\" storage-length=\"4\" display-length=\"7\" scale=\"2\" numeric=\"COBOL_NUMERIC\" display-position=\"1107\"/>\n"
			+ "                </item>\n"
			+ "                <item level=\"10\" name=\"redef-2\" picture=\"x(5)\" position=\"1094\" storage-length=\"5\" display-length=\"5\" display-position=\"1100\" redefines=\"area-1\"/>\n"
			+ "            </item>\n"
			+ "        </item>\n"
			+ "        <item level=\"03\" name=\"Group-10\" position=\"1104\" storage-length=\"1080\" display-length=\"1080\" display-position=\"1114\">\n"
			+ "            <item level=\"05\" name=\"array-1\" occurs=\"10\" position=\"1104\" storage-length=\"106\" display-length=\"106\" display-position=\"1114\">\n"
			+ "                <item level=\"10\" name=\"field-10\" picture=\"s9(4)\" position=\"1104\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\" display-position=\"1114\"/>\n"
			+ "                <item level=\"10\" name=\"array-2\" occurs=\"5\" position=\"1108\" storage-length=\"16\" display-length=\"16\" display-position=\"1118\">\n"
			+ "                    <item level=\"15\" name=\"field-11\" picture=\"s99\" usage=\"computational\" position=\"1108\" storage-length=\"2\" display-length=\"2\" numeric=\"COBOL_NUMERIC\" display-position=\"1118\"/>\n"
			+ "                    <item level=\"15\" name=\"field-12\" occurs=\"5\" picture=\"xx\" position=\"1110\" storage-length=\"2\" display-length=\"2\" display-position=\"1120\"/>\n"
			+ "                    <item level=\"15\" name=\"field-14\" picture=\"s9(4)\" position=\"1120\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\" display-position=\"1130\"/>\n"
			+ "                </item>\n"
			+ "                <item level=\"10\" name=\"group-11\" position=\"1188\" storage-length=\"12\" display-length=\"12\" display-position=\"1198\">\n"
			+ "                    <item level=\"15\" name=\"field-15\" picture=\"s99\" usage=\"computational\" position=\"1188\" storage-length=\"2\" display-length=\"2\" numeric=\"COBOL_NUMERIC\" display-position=\"1198\"/>\n"
			+ "                    <item level=\"15\" name=\"field-16\" occurs=\"5\" picture=\"xx\" position=\"1190\" storage-length=\"2\" display-length=\"2\" display-position=\"1200\"/>\n"
			+ "                </item>\n"
			+ "                <item level=\"10\" name=\"array-2\" occurs=\"5\" position=\"1200\" storage-length=\"2\" display-length=\"2\" display-position=\"1210\">\n"
			+ "                    <item level=\"15\" name=\"field-17\" picture=\"s99\" usage=\"computational\" position=\"1200\" storage-length=\"2\" display-length=\"2\" numeric=\"COBOL_NUMERIC\" display-position=\"1210\"/>\n"
			+ "                </item>\n"
			+ "            </item>\n"
			+ "            <item level=\"05\" name=\"field-20\" picture=\"xx\" position=\"2164\" storage-length=\"2\" display-length=\"2\" display-position=\"2174\"/>\n"
			+ "            <item level=\"05\" name=\"group-12\" position=\"2166\" storage-length=\"18\" display-length=\"18\" display-position=\"2176\">\n"
			+ "                <item level=\"10\" name=\"field-21\" occurs=\"5\" picture=\"xxx\" position=\"2166\" storage-length=\"3\" display-length=\"3\" display-position=\"2176\"/>\n"
			+ "                <item level=\"10\" name=\"field-22\" picture=\"999\" position=\"2181\" storage-length=\"3\" display-length=\"3\" numeric=\"COBOL_NUMERIC\" display-position=\"2191\"/>\n"
			+ "            </item>\n"
			+ "        </item>\n"
			+ "        <item level=\"03\" name=\"field-40\" picture=\"s999ppp\" position=\"2184\" storage-length=\"3\" display-length=\"6\" scale=\"-3\" numeric=\"COBOL_NUMERIC\" display-position=\"2194\"/>\n"
			+ "    </item>\n"
			+ "</copybook>";
	@Test
	public void testXml1c() throws XMLStreamException, LexerException, IOException, ParserException {
		ICb2XmlBuilder builder = Cb2Xml3.newBuilder(new StringReader(displayPosCobol1), "Record")
				.setXmlFormat(Cb2xmlConstants.Cb2xmlXmlFormat.CLASSIC)
				.setIndent(true);
		//System.out.println(builder.asXmlString());
		assertEquals(displayPosXml1c, builder.asXmlString());
		
		
		int[] storageLengths = {3, 10, 8, 5, 3, 3, 3, 3, };
		int[] displayLengths = {5, 10, 14, 5, 6, 6, 3, 3, };
		
		ICopybook copybook = builder.asCobolItemTree();
		assertEquals(1, copybook.getChildItems().size());

		checkItems(copybook.getChildItems().get(0), storageLengths, displayLengths);
	}


	@Test
	public void testXml1n() throws XMLStreamException, LexerException, IOException, ParserException {
		ICb2XmlBuilder builder = Cb2Xml3.newBuilder(new StringReader(displayPosCobol1), "Record")
				.setXmlFormat(Cb2xmlConstants.Cb2xmlXmlFormat.FORMAT_2017)
				.setIndent(true);
		//System.out.println(builder.asXmlString());
		assertEquals(displayPosXml1n, builder.asXmlString());
		//System.out.println(displayPosCobol1);
	}

	private static List<ItmDtls> itemDtls2n = Arrays.asList(
			new ItmDtls("tst-1", null, "NONE", 25, 37
					, new ItmDtls("field-1", "s9(5)", "COMP_3", 3, 5)
					, new ItmDtls("Group-1", null, "NONE", 10, 14
							, new ItmDtls("Group-2", null, "NONE", 10, 14
									, new ItmDtls("area-1", "x(10)", "NONE", 10, 10)
									, new ItmDtls("redef-1", null, "NONE", 8, 14
											, new ItmDtls("field-2", "s9(7)", "COMP_3", 4, 7)
											, new ItmDtls("field-3", "s9(5)V99", "COMP_3", 4, 7)
											)
									, new ItmDtls("redef-2", "x(5)", "NONE", 5, 5)
									)
							)
					, new ItmDtls("field-4", "s999ppp", "NONE", 3, 6)
					, new ItmDtls("field-5", "spvpp999", "NONE", 3, 6)
					, new ItmDtls("field-6", "s999", "NONE", 3, 3)
					, new ItmDtls("field-7", "xxx", "NONE", 3, 3)
					)
			);
	@Test
	public void testXml2n() throws XMLStreamException, LexerException, IOException, ParserException {
		ICb2XmlBuilder builder = Cb2Xml3.newBuilder(new StringReader(displayPosCobol2), "Record")
				.setXmlFormat(Cb2xmlConstants.Cb2xmlXmlFormat.FORMAT_2017)
				.setIndent(true);
		//System.out.println(builder.asXmlString());
		assertEquals(displayPosXml2n, builder.asXmlString());
		
		
		int[] storageLengths = {3, 10, 3, 3, 3, 3, };
		int[] displayLengths = {5, 14, 6, 6, 3, 3, };
		
		ICopybook copybook = builder.asCobolItemTree();
		assertEquals(1, copybook.getChildItems().size());

		checkItems(copybook.getChildItems().get(0), storageLengths, displayLengths);
		//writeItms(copybook.getChildItems());
		checkItms(new Lengths(1, 1), copybook.getChildItems(), itemDtls2n);
	}

	
	 private static List<ItmDtls> itemDtls3n = Arrays.asList(
	 			 new ItmDtls("tst-1", null, "NONE", 25, 37
	 					 , new ItmDtls("field-1", "s9(5)", "COMP_3", 3, 5)
	 					 , new ItmDtls("Group-1", null, "NONE", 13, 20
	 							 , new ItmDtls("Group-2", null, "NONE", 10, 14
	 									 , new ItmDtls("area-1", "x(10)", "NONE", 10, 10)
	 									 , new ItmDtls("redef-1", null, "NONE", 8, 14
	 											 , new ItmDtls("field-2", "s9(7)", "COMP_3", 4, 7)
	 											 , new ItmDtls("field-3", "s9(5)V99", "COMP_3", 4, 7)
	 											 )
	 									 , new ItmDtls("redef-2", "x(5)", "NONE", 5, 5)
	 									 )
	 							 , new ItmDtls("field-4", "s999ppp", "NONE", 3, 6)
	 							 )
	 					 , new ItmDtls("field-5", "spvpp999", "NONE", 3, 6)
	 					 , new ItmDtls("field-6", "s999", "NONE", 3, 3)
	 					 , new ItmDtls("field-7", "xxx", "NONE", 3, 3)
	 					 )
	 );
	 
	@Test
	public void testXml3n() throws XMLStreamException, LexerException, IOException, ParserException {
		ICb2XmlBuilder builder = Cb2Xml3.newBuilder(new StringReader(displayPosCobol3), "Record")
				.setXmlFormat(Cb2xmlConstants.Cb2xmlXmlFormat.FORMAT_2017)
				.setIndent(true);
		//System.out.println(builder.asXmlString());
		assertEquals(displayPosXml3n, builder.asXmlString());
		
		
		int[] storageLengths = {3, 13, 3, 3, 3, };
		int[] displayLengths = {5, 20, 6, 3, 3, };
		
		ICopybook copybook = builder.asCobolItemTree();
		assertEquals(1, copybook.getChildItems().size());

		checkItems(copybook.getChildItems().get(0), storageLengths, displayLengths);
//		writeItms(copybook.getChildItems());
		checkItms(new Lengths(1, 1), copybook.getChildItems(), itemDtls3n);
	}
	 
	
	private static List<ItmDtls> itemDtls4n = Arrays.asList(
			new ItmDtls("tst-1", null, "NONE", 2186, 2199
					, new ItmDtls("field-1", "s9(5)", "COMP_3", 3, 5)
					, new ItmDtls("Group-1", null, "NONE", 10, 14
							, new ItmDtls("Group-2", null, "NONE", 10, 14
									, new ItmDtls("area-1", "x(10)", "NONE", 10, 10)
									, new ItmDtls("redef-1", null, "NONE", 8, 14
											, new ItmDtls("field-2", "s9(7)", "COMP_3", 4, 7)
											, new ItmDtls("field-3", "s9(5)V99", "COMP_3", 4, 7)
											)
									, new ItmDtls("redef-2", "x(5)", "NONE", 5, 5)
									)
							)
					, new ItmDtls("Group-10", null, "NONE", 1080, 1080
							, new ItmDtls("array-1", null, "NONE", 106, 106
									, new ItmDtls("field-10", "s9(4)", "NONE", 4, 4)
									, new ItmDtls("array-2", null, "NONE", 16, 16
											, new ItmDtls("field-11", "s99", "COMP", 2, 2)
											, new ItmDtls("field-12", "xx", "NONE", 2, 2)
											, new ItmDtls("field-14", "s9(4)", "NONE", 4, 4)
											)
									, new ItmDtls("group-11", null, "NONE", 12, 12
											, new ItmDtls("field-15", "s99", "COMP", 2, 2)
											, new ItmDtls("field-16", "xx", "NONE", 2, 2)
											)
									, new ItmDtls("array-2", null, "NONE", 2, 2
											, new ItmDtls("field-17", "s99", "COMP", 2, 2)
											)
									)
							, new ItmDtls("field-20", "xx", "NONE", 2, 2)
							, new ItmDtls("group-12", null, "NONE", 18, 18
									, new ItmDtls("field-21", "xxx", "NONE", 3, 3)
									, new ItmDtls("field-22", "999", "NONE", 3, 3)
									)
							)
					, new ItmDtls("Group-1", null, "NONE", 10, 14
							, new ItmDtls("Group-2", null, "NONE", 10, 14
									, new ItmDtls("area-1", "x(10)", "NONE", 10, 10)
									, new ItmDtls("redef-1", null, "NONE", 8, 14
											, new ItmDtls("field-2", "s9(7)", "COMP_3", 4, 7)
											, new ItmDtls("field-3", "s9(5)V99", "COMP_3", 4, 7)
											)
									, new ItmDtls("redef-2", "x(5)", "NONE", 5, 5)
									)
							)
					, new ItmDtls("Group-10", null, "NONE", 1080, 1080
							, new ItmDtls("array-1", null, "NONE", 106, 106
									, new ItmDtls("field-10", "s9(4)", "NONE", 4, 4)
									, new ItmDtls("array-2", null, "NONE", 16, 16
											, new ItmDtls("field-11", "s99", "COMP", 2, 2)
											, new ItmDtls("field-12", "xx", "NONE", 2, 2)
											, new ItmDtls("field-14", "s9(4)", "NONE", 4, 4)
											)
									, new ItmDtls("group-11", null, "NONE", 12, 12
											, new ItmDtls("field-15", "s99", "COMP", 2, 2)
											, new ItmDtls("field-16", "xx", "NONE", 2, 2)
											)
									, new ItmDtls("array-2", null, "NONE", 2, 2
											, new ItmDtls("field-17", "s99", "COMP", 2, 2)
											)
									)
							, new ItmDtls("field-20", "xx", "NONE", 2, 2)
							, new ItmDtls("group-12", null, "NONE", 18, 18
									, new ItmDtls("field-21", "xxx", "NONE", 3, 3)
									, new ItmDtls("field-22", "999", "NONE", 3, 3)
									)
							)
					, new ItmDtls("field-40", "s999ppp", "NONE", 3, 6)
					)
	);
	@Test
	public void testXml4n() throws XMLStreamException, LexerException, IOException, ParserException {
		ICb2XmlBuilder builder = Cb2Xml3.newBuilder(new StringReader(displayPosCobol4), "Record")
				.setXmlFormat(Cb2xmlConstants.Cb2xmlXmlFormat.FORMAT_2017)
				.setIndent(true);
		//System.out.println(builder.asXmlString());
		assertEquals(displayPosXml4n, builder.asXmlString());
		
		
		ICopybook copybook = builder.asCobolItemTree();
		assertEquals(1, copybook.getChildItems().size());


		//writeItms(copybook.getChildItems());
		checkItms(new Lengths(1, 1), copybook.getChildItems(), itemDtls4n);
	}

	/**
	 * @param topLevel
	 * @param storageLengths
	 * @param displayLengths
	 */
	private void checkItems(IItem topLevel, int[] storageLengths, int[] displayLengths) {
		List<? extends IItem> childItems = topLevel.getChildItems();
		int pos = 1, displayPos=1, lastLen = 0, lastDisplayLen = 0;
		
		for (int i = 0; i < childItems.size(); i++) {
			IItem child = childItems.get(i); 
			//System.out.print(child.getStorageLength()+ ", ");
			
			if (i > 0 && ! child.isFieldRedefines()) {
				pos += lastLen;
				displayPos += lastDisplayLen;
			}
			assertEquals(pos, child.getPosition());
			assertEquals(displayPos, child.getDisplayPosition());
			assertEquals(storageLengths[i], child.getStorageLength());
			assertEquals(displayLengths[i], child.getDisplayLength());
			
			if (i > 0 && child.isFieldRedefines()) {
				lastLen = Math.max(lastLen, child.getStorageLength());
				lastDisplayLen = Math.max(lastDisplayLen, child.getDisplayLength());
			} else {
				lastLen =  child.getStorageLength();
				lastDisplayLen = child.getDisplayLength();
			}
		}
		assertEquals(storageLengths.length, childItems.size());
		assertEquals(lastLen + pos - 1, topLevel.getStorageLength());
		assertEquals(lastDisplayLen + displayPos - 1, topLevel.getDisplayLength());
	}

	
	private Lengths checkItms(Lengths l, List<? extends IItem> items, List<ItmDtls> expected) {
		
		Lengths last = new Lengths(0, 0);
		for (int i = 0; i < items.size(); i++) {
			IItem item = items.get(i);
			ItmDtls itmDtls = expected.get(i);
			if (! item.isFieldRedefines()) {
				l.add(last.storage, last.display);
				last = new Lengths(0, 0);
			}
			
			assertEquals(itmDtls.picture, item.getPicture());
			assertEquals(itmDtls.usage, item.getUsage().toString());
			assertEquals(itmDtls.storageLength, item.getStorageLength());
			assertEquals(itmDtls.displayLength, item.getDisplayLength());
			assertEquals(item.getFieldName(), l.storage, item.getPosition());
			assertEquals(item.getFieldName(), l.display, item.getDisplayPosition());
			

			if (itmDtls.children != null && itmDtls.children.size() > 0) {
				Lengths ll = checkItms(l.cloneThis(), item.getChildItems(), itmDtls.children);
				Lengths tmp = l.cloneThis();
				
				tmp.add(itmDtls.storageLength, itmDtls.displayLength);
				
				assertEquals(item.getFieldName(), tmp.storage, ll.storage);
				assertEquals(item.getFieldName(), tmp.display, ll.display);
			} 
			int occurs = 1;
			if (item.getOccurs() >= 0) {
				occurs = item.getOccurs();
			}

			last.max(itmDtls.storageLength * occurs, itmDtls.displayLength * occurs);

		}
		
		l.add(last.storage, last.display);
		return l;
	}

	
	private void writeItms(List<? extends IItem> items) {
		
		for (int i = 0; i < items.size(); i++) {
			IItem item = items.get(i);
			String s = item.getPicture() == null ? "null" : "\"" + item.getPicture() + "\"";
			System.out.print("\n\t, new ItmDtls(\"" + item.getFieldName() + "\", " + s + ", \"" + item.getUsage() 
						+ "\", " + item.getStorageLength() + ", " + item.getDisplayLength());
			if (item.getChildItems().size() == 0) {
				System.out.print(")");
			} else {
				writeItms(item.getChildItems());
				System.out.print("\n\t )");
			}
		}
	}
	

	private static List<ItmDtls> EMPTY_CHILD_LIST = Collections.emptyList();
	private static class ItmDtls {
		final String picture, usage;
		final int storageLength, displayLength;
		final List<ItmDtls> children;

		public ItmDtls(String fieldname, String picture, String usage, int storageLength, int displayLength, ItmDtls... childItms) {
			super();
			this.picture = picture;
			this.usage = usage;
			this.storageLength = storageLength;
			this.displayLength = displayLength;
			children = childItms.length == 0 ? EMPTY_CHILD_LIST : Arrays.asList(childItms);
		}
	}

	private static class Lengths {
		int storage, display;

		public Lengths(int storage, int display) {
			super();
			this.storage = storage;
			this.display = display;
		}
		
		void max(int s, int d) {
			storage = Math.max(storage, s);
			display = Math.max(display, d);
		}
		
		void add(int s, int d) {
			storage += s;
			display += d;
		}
		
		Lengths cloneThis() {
			return new Lengths(storage, display);
		}
	}
}
