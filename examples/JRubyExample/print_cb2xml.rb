################################################################
## This is a simple example of accessing cb2xml from ruby
## It simply prints the content of the Xml
## Because we are reading the cb2xml-Xml (rather than Cobol),
## we only need the cb2xml_Jaxb.jar
##
## Author: Bruce Martin
## License: Any, e.g. LGPL (any), Apache, Creative Commons
################################################################

require '../../lib/cb2xml_Jaxb.jar'

java_import 'net.sf.cb2xml.parse.XmlParser'
 
   def printXml indent, items

       items.each do |item|
           puts "#{indent} #{item.getLevel()} #{item.getName()}\t#{item.getPosition()}\t#{item.getStorageLength()}\t#{item.getPicture()}\t#{item.getUsage()}"   
           printXml "#{indent}   ", item.getItem()   
       end
   end

   copybook = XmlParser.new().parseXml("cb2xml_Output110.xml")
   
   printXml "    ", copybook.getItem()
   
   x = gets
