#!/usr/bin/python
##############################################################
#
#  Purpose: Illustrate processing a cb2xml xml file in python.
#           It prints the Xml content to the terminal
#   Author: Bruce Martin
#  License: Whatever: LGPL (any), Apache, Creative commons, etc
#           Use at your own risk
#
##############################################################
import xml.etree.ElementTree as ET

## Get an attribute if it exist otherwise return ''
def getAttr(d, key):
    r=''
    if key in d.attrib:
        r = d.attrib[key]
    return r

# Get attribute if it exists
def getAttrId(d, key):
    r=''
    if key in d.attrib:
        r = key + '=' + d.attrib[key]
    return r
 
##########################################################################
# Purpose: Print one item
##########################################################################
def printItem(indent, item):
    n = indent + item.attrib['level'] + " " + item.attrib['name'] + "                                                                                    "
    n = n[:50]

    print n, '\t', item.attrib['position'], '\t', item.attrib['storage-length'], '\t', getAttr(item, 'display-length'), getAttr(item, 'picture'), getAttrId(item, 'usage'), getAttrId(item, 'numeric'), getAttrId(item, 'signed')
    for child in item.findall('item'):
	printItem(indent + "    ", child)

#########################################################################

tree = ET.parse('cbl2xml_Test110.cbl.xml')
root = tree.getroot()

print ">> ", root.tag, root.attrib['filename']

for child in root.findall('item'):
    printItem("  ", child)
    

