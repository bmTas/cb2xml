#! /bin/sh
#-----------------------------------------------------------------------------
# Convert "Xml Copybook's" to a text Cobol Copybook and Cobol Move code
#
# Author Bruce Martin
#
#-----------------------------------------------------------------------------

python toTextCopybook.py -inxml cbl2xml_Test110.cbl.xml -outcopy cbl2xml_Test110_copy.cbl -outcode cbl2xml_Test110_code.cbl
python toTextCopybook.py -inxml DTAR107.xml -outcopy DTAR107T.cbl -outcode DTAR107C.cbl
python toTextCopybook.py -inxml DTAR119.xml -outcopy DTAR119T.cbl -outcode DTAR119C.cbl
python toTextCopybook.py -h

