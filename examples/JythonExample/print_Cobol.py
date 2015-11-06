##############################################################
#
#  Purpose: Illustrate processing a Cobol Copybook in jython.
#           It prints the  Cobol Copybook content to the terminal
#           Because is processing Cobol, this program needs both the
#             * cb2xml.jar (Cobol Processing
#             * cb2xml_Jaxb (Xml JAXB stuff)
#   Author: Bruce Martin
#  License: any e.g. LGPL (any), Apache, Creative Commons
#
##############################################################

import sys

sys.path.append("../../lib/cb2xml.jar")
sys.path.append("../../lib/cb2xml_Jaxb.jar")

from net.sf.cb2xml.parse import CobolParser

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
# Purpose: Print one item and print any children
##########################################################################
def printItem(indent, item):
    n = indent + item.getLevel() + " " + item.getName() + "                                                                                      "
    n = n[:50]
    print n, '\t', item.getPosition(), '\t', item.getStorageLength(), fix(item.getPicture()), fix(item.getUsage()), fixBoolean("numeric",item.isNumeric()), fixBoolean("signed", item.isSigned())
    children = item.getItem()
    for child in children:
	printItem(indent + "   ", child)

#########################################################################

copybook = CobolParser().parseCobol("cbl2xml_Test110.cbl")

print ">> ", copybook.getFilename()

children = copybook.getItem()
for child in children:
    printItem("    ", child)
    

