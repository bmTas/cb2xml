rem  Purpose: Convert Xml files to Cobol data files (using program Xml2Dat)  
rem  Note: Xml2Dat only works with Text (line-sequential) files. 
rem       It can not handle:
rem         * Cobol binary files
rem         * Arrays
rem you should look at Xml2Data in the JRecord Project


java -cp ../../lib/cb2xml.jar net.sf.cb2xml.Xml2Dat Ams_LocDownload_102.xml  cbl2xml_Test102.cbl.xml > Ams_LocDownload_102.txt
java -cp ../../lib/cb2xml.jar net.sf.cb2xml.Xml2Dat Ams_LocDownload_110.xml  cbl2xml_Test110.cbl.xml > Ams_LocDownload_110.txt
pause