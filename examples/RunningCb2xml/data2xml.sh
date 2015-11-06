#! /bin/sh
##
##  Convert cobol data (text) file to xml.
##
##  Dat2Xml only handles text files. It can not handle binary files
##
    java -cp ../../lib/cb2xml.jar net.sf.cb2xml.Dat2Xml Ams_LocDownload.txt  cbl2xml_Test102.cbl.xml > Ams_LocDownload_102.xml
    java -cp ../../lib/cb2xml.jar net.sf.cb2xml.Dat2Xml Ams_LocDownload.txt  cbl2xml_Test110.cbl.xml > Ams_LocDownload_110.xml
 