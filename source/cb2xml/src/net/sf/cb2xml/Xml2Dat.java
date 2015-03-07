package net.sf.cb2xml;

import net.sf.cb2xml.convert.XmlToMainframe;
import net.sf.cb2xml.util.XmlUtils;

import org.w3c.dom.Document;

/**
 * Converts a XML data file + a XML copybook description into a .DAT (data) file.
 */
public class Xml2Dat
{
    /**
     * Converts a XML data file + a XML copybook description into a .DAT (data) file.
     * The output is sent to stdout.
     * 
     * @param args command-line arguments:<ol>
     * <li>XML file containing data</li>
     * <li>XML file containing copybook</li>
     */
    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Usage:\txml2dat <xmlDataFileName> <copybookFileName>");
            System.err.println();
            System.err.println("Output will be printed to stdout; you can redirect it to a file with \" > <outputFileName>");
            return;
        }
        
        Document sourceFileXml = XmlUtils.fileToDom(args[0]);
        Document copyBookXml = XmlUtils.fileToDom(args[1]);

        String resultString = new XmlToMainframe().convert(sourceFileXml, copyBookXml);
        System.out.println(resultString);
    }
}

