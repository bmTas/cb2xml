#! /bin/sh

## ------------------------------------------------------------------------
##   Purpose: Convert Xml files to Cobol data files (using program Xml2Dat)
##   
##   Note: Xml2Dat only works with Text (line-sequential) files. 
##        It can not handle:
##          * Cobol binary files
##          * Arrays
## ------------------------------------------------------------------------ */
   
    java -cp ../../lib/cb2xml.jar net.sf.cb2xml.Xml2Dat Ams_LocDownload_102.xml  cbl2xml_Test102.cbl.xml > Ams_LocDownload_102.txt
    java -cp ../../lib/cb2xml.jar net.sf.cb2xml.Xml2Dat Ams_LocDownload_110.xml  cbl2xml_Test110.cbl.xml > Ams_LocDownload_110.txt
 