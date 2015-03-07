package net.sf.cb2xml;

import net.sf.cb2xml.convert.MainframeToXml;
import net.sf.cb2xml.util.FileUtils;
import net.sf.cb2xml.util.XmlUtils;

import org.w3c.dom.Document;

/**
 * Converts a .DAT (data) file + a XML copybook description into a XML data file.
 */
public class Dat2Xml
{
    /**
     * Converts a .DAT (data) file + a XML copybook description into a XML data file.
     * Output is sent to stdout.
     * 
     * @param args command-line arguments:<ol>
     * <li>DAT file containing data</li>
     * <li>XML file containing copybook</li>
     */
    public static void main(String[] args)
    {
        if (args.length != 2) {
            System.err.println("Usage:\tdat2xml <dataFileName> <xmlCopybookFileName>");
            System.err.println();
            System.err.println("Output will be printed to stdout; you can redirect it to a file with \" > <outputFileName>");
            return;
        }
        
        final String dataFileName = args[0];
        final String copybookFileName = args[1];
        
        String sourceFileContents= FileUtils.readFile(dataFileName).toString();
        Document copyBookXml = XmlUtils.fileToDom(copybookFileName);

        // params 1) XML source document 2) Copybook as XML
        Document resultDocument = new MainframeToXml().convert(sourceFileContents, copyBookXml);
        final StringBuffer resultBuffer = XmlUtils.domToString(resultDocument);
        String resultString = resultBuffer.toString();
        System.out.println(resultString);
      }
}

