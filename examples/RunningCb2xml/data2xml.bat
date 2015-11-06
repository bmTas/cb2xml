rem  Convert cobol data (text) file to xml.
rem  Dat2Xml only handles text files. It can not handle Cobol binary files
rem you should look at Data2Xml in the JRecord Project
java -cp ../../lib/cb2xml.jar net.sf.cb2xml.Dat2Xml Ams_LocDownload.txt  cbl2xml_Test102.cbl.xml > Ams_LocDownload_102.xml
java -cp ../../lib/cb2xml.jar net.sf.cb2xml.Dat2Xml Ams_LocDownload.txt  cbl2xml_Test110.cbl.xml > Ams_LocDownload_110.xml
pause 