package tests;

import static org.junit.Assert.*;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import javax.xml.parsers.ParserConfigurationException;

import net.sf.cb2xml.convert.MainframeToXml;
import net.sf.cb2xml.convert.XmlToMainframe;
import net.sf.cb2xml.util.FileUtils;
import net.sf.cb2xml.util.XmlUtils;

import org.junit.Test;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import common.Code;

public class TstCblDataToXml01 {

	private static final String LOC_DOWNLOAD_TXT = "Ams_LocDownload.txt";
	private String[][] files ={
			{"cb2xml_Output102a.xml", LOC_DOWNLOAD_TXT, "Ams_LocDownload_102.xml"},
			{"cb2xml_Output110.xml",  LOC_DOWNLOAD_TXT, "Ams_LocDownload_110.xml"},
			{"cb2xml_Output102.xml",  LOC_DOWNLOAD_TXT, "Ams_LocDownload_102.xml"},     //-- After updates for comments,
			{"cb2xml_Output111.xml",  LOC_DOWNLOAD_TXT, "Ams_LocDownload_111.xml"},
			{"cb2xml_Output112.xml",  "StoreSales5.txt",    "StoreSales5.xml"},          // -- Does not work 0.94
	};
	
	//private String firstLine = "TAR5839DCDC - Taras Ave                                                             30-68 Taras Ave                         Altona North                       3025      VICA";
	@Test
	public void testData2Xml() throws IOException, SAXException, ParserConfigurationException {
		String copybookName, dataName, xmlDataName;
		for (String[] d : files) {
			copybookName = Code.getFullName("xmlCopybook/" + d[0]);
			dataName = Code.getFullName(d[1]);
			
			Document doc = data2xml(dataName, copybookName);
			
			
			//System.out.println(XmlUtils.domToString(doc));
			System.out.println("Copybook: " + d[0] + " " + d[2]);
			System.out.println();
			
			xmlDataName = Code.getFullName(d[2]);
			common.Code.compare("File: " + copybookName,  xmlDataName, doc);

		}
	}

	@Test
	public void testXml2Data() throws IOException, SAXException, ParserConfigurationException {
		String xml2data;
		String expected = loadLocationFile();
		
		for (int i = 0; i < 3; i++) { // xml2data only works when there are no arrays !!!
			String[] d = files[i];
			System.out.println("->> " + d[0]);
			xml2data = xml2data(Code.getFullName(d[2]), Code.getFullName("xmlCopybook/" + d[0]));
			assertEquals(expected, xml2data);
		}
	}
	
	private static Document data2xml(String dataFileName, String copybookFileName) {
		
	       String sourceFileContents= FileUtils.readFile(dataFileName).toString();
	       Document copyBookXml = XmlUtils.fileToDom(copybookFileName);

	       return new MainframeToXml().convert(sourceFileContents, copyBookXml);
	}
	
	
	
	private static String xml2data(String dataFileName, String copybookFileName) {
		
        Document sourceFileXml = XmlUtils.fileToDom(dataFileName);
        Document copyBookXml = XmlUtils.fileToDom(copybookFileName);

        return new XmlToMainframe().convert(sourceFileXml, copyBookXml);
	}
	
	private static String loadLocationFile() throws IOException {
		StringBuilder b = new StringBuilder();
		String sep = "";
		BufferedReader r = new BufferedReader(new FileReader(Code.getFullName(LOC_DOWNLOAD_TXT)));
		String s;
		while ((s = r.readLine()) != null) {
			b.append(sep).append(s);
			sep = "\n";
		}
		r.close();
		
		if (b.charAt(b.length() - 1) == '\n') {
			b.setLength(b.length() - 1);
		}
		return b.toString();
	}
}
