################################################################
## This is a simple example of accessing cb2xml from ruby
## It simply prints the content of the Xml
## Because we are reading the Cobol copybook (rather than the Xml)
## we need the cb2xml.jar and cb2xml_Jaxb.jar
##
## Author: Bruce Martin
## License: Any, e.g. LGPL (any), Apache, Creative Commons
################################################################
require '../../lib/cb2xml.jar'
require '../../lib/cb2xml_Jaxb.jar'

java_import 'net.sf.cb2xml.parse.CobolParser'
  
   def printXml indent, items

       items.each do |item|
           puts "#{indent} #{item.getLevel()} #{item.getName()}\t#{item.getPosition()}\t#{item.getStorageLength()}\t#{item.getPicture()}\t#{item.getUsage()}"   
           printXml "#{indent}   ", item.getItem()   
       end
   end

   copybook = CobolParser.new().parseCobol("cbl2xml_Test110.cbl")
   
   printXml "    ", copybook.getItem()
   
   x = gets
