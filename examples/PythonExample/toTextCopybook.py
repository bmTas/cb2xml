#!/usr/bin/python
##############################################################
#
#  Purpose: Create "Text" version of a copybook and
#           generate code to move data from the normal
#           normal copybook to the text copybook.
#
#           Limitations:
#             1. Only intended for single record files with no redefines
#             2. Does not handle arrays
#             3. It can not handle duplicate field names
#             4. If fields names longer than 28, will generate field names
#                that are to long
#             5. It will not handle comp-1 / comp-2
#             6. It was written as an example program !!!
#
#   Author: Bruce Martin
#  License: Whatever: LGPL (any), Apache, Creative commons, etc
#           Use at your own risk
#
##############################################################
import xml.etree.ElementTree as ET
import argparse
import textwrap


## Get a xml attribute if it exist otherwise return ''
def getAttr(d, key):
    r=''
    if key in d.attrib:
        r = d.attrib[key]
    return r
    
## get the scaling factor (i.e. decimals) for the number    
def getScale(item):    
    scale = getAttr(item,'scale')   
    if  scale == '':
        scale=0
    else:
    	scale = int(scale)
    return scale
 
##########################################################################
# Purpose: Write 1 item in the cobol copybook and loop
#          through child items
##########################################################################
def writeCopybookItem(item):

    if 'picture' in item.attrib:
    	name=item.attrib['name']
        n = name + "-T                                                                                   "
        n = n[:35]
    	if name=='' or name.upper() == 'FILLER':
    	    x='x'
    	elif 'numeric' in item.attrib:
    	    scale = getScale(item)
    	    sign = '-'
    	    if getAttr(item, 'picture').startswith('9'):
    	    	sign = ''

    	    if  scale <= 0:
    	    	length = int(getAttr(item, 'display-length')) - scale
    	        copybook.write('           05 ' + n + 'PIC ' + sign + '9(' + str(length) + ').\n')
    	    else:
    	    	length = int(getAttr(item, 'display-length')) - scale
    	    	if  length < 1:
    	    	    length = 1
    	        copybook.write('           05 ' + n + 'PIC ' + sign + '9(' + str(length) + ').9(' + str(scale) + ').\n')
        else:
            copybook.write('           05 ' + n + 'PIC X(' + getAttr(item, 'display-length') + ').\n') 
    else:
        for child in item.findall('item'):
            writeCopybookItem(child)


##########################################################################
# Purpose: Write 1 item in the cobol copybook and loop
#          through child items
##########################################################################
def writeCodeItem(item):

    if 'picture' in item.attrib:
    	name=item.attrib['name'] 
        code.write('           Move ' + name + '\n') 
        code.write('             to ' + name + '-T\n') 
    else:
        for child in item.findall('item'):
            writeCodeItem(child)

 
#########################################################################
#    Main program
#########################################################################


parser = argparse.ArgumentParser(description=textwrap.dedent('''\
	This program will read a xml-copybook created by cb2xml and create 2 new "Cobol Copybooks:":
        * A "Text" Cobol copybook
        * A "Code" copybook that moves fields from the "input copybook" to the "Text copybook" '''))
parser.add_argument('-inxml', help='Input cb2xml Copybook file name',required=True)
parser.add_argument('-outcopy',help='Output Copybook file name', required=True)
parser.add_argument('-outcode',help='Output Code Copybook file name', required=True)
args = parser.parse_args()

tree = ET.parse(args.inxml)
root = tree.getroot()


print ">> ", root.tag, root.attrib['filename']

copybook = open(args.outcopy, 'w')
code = open(args.outcode, 'w')
for child in root.findall('item'):
    writeCopybookItem(child)   

    writeCodeItem(child)   
code.write('\n           .\n\n')     
code.close()
copybook.close()

