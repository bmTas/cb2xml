/**
 * Need to add ../../lib/cb2xml.jar & ../../lib/cb2xml_Jaxb.jar to the classpath
 * The run.bat/sh will do this
 */
import net.sf.cb2xml.jaxb.Copybook
import net.sf.cb2xml.jaxb.Item
import net.sf.cb2xml.parse.CobolParser
    
    // Parsing a Cobol copybook
    def cpy = CobolParser.newParser().parseCobol("cbl2xml_Test110.cbl")
    
    // Now lets print it
    print("    ", cpy.getItem())

// Print Cobol item (and its child items
def print(String indent, List<Item> items) {

    for (Item item : items) {
       String s = indent + item.getName() + "                                                                           "
       s = s.substring(0, 50)
       println s + item.getPosition() + "\t" + item.getStorageLength() + "\t" + item.getDisplayLength() + cn(item.getPicture()) + cn(item.getUsage()) + cn(item.isNumeric()) + cn(item.isSigned())
      
       print(indent + "    ", item.getItem())
   }
}

// add tab to object and convert to "" if null
def cn(obj) {
    if (obj == null) {
        return "\t"
    }
    return "\t" +obj
}