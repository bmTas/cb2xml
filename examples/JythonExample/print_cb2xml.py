##############################################################
#
#  Purpose: Illustrate processing a cb2xml xml file in jython.
#           It prints the Xml content to the terminal
#           This only needs the cb2xml_Jaxb.jar as it
#           accesses the Xml
#   Author: Bruce Martin
#  License: any e.g. LGPL (any), Apache, Creative Commons
#
##############################################################
import sys

sys.path.append("../../lib/cb2xml_Jaxb.jar")

from net.sf.cb2xml.parse import XmlParser


## return the value or '' if null. 
def fix(v):
    r="\t"
    if v != None:
        r="\t" + v
    return r

def fixBoolean(a, v):
    r=''
    if v != None:
        r=a + "=true" 
    return r

 
##########################################################################
# Purpose: Print one item
##########################################################################
def printItem(indent, item):
    n = indent + item.getLevel() + " " + item.getName() + "                                                                                      "
    n = n[:50]
    print n, '\t', item.getPosition(), '\t', item.getStorageLength(), fix(item.getPicture()), fix(item.getUsage()), fixBoolean("numeric",item.isNumeric()), fixBoolean("signed", item.isSigned())
    children = item.getItem()
    for child in children:
	printItem(indent + "   ", child)

#########################################################################

copybook = XmlParser().parseXml("cb2xml_Output110.xml")

print ">> ", copybook.getFilename()

children = copybook.getItem()
for child in children:
    printItem("    ", child)
    

