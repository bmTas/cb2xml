## cb2xml - Cobol to Xml

The cb2xml package reads a Cobol-Copybook and converts it to either

* A java Item tree holding all the Cobol Details (Picture, position, length etc).
* An Xml file containing the Cobol Details (Picture, position, length etc).

The cb2xml package is supplied with Mainframe Cobol details. It is possible to support
other Cobol dialects (via Java plugins), the [JRecord](https://sourceforge.net/projects/jrecord/) project does this.

### Using cb2xml
cb2xml converts Cobol:

```cobol
000006 01  REC-FACTURE.                                                 
000011     03  FS1                  PIC X.                              
000016     03  FS2.                                                     
000021         05  FS2A            PIC 9.                               
               05  RFS2B           PIC X(8).
000026         05  FS2B REDEFINES RFS2B  PIC 9(8).
000031     03  FS3.                                                     
000036         05  FS3A            PIC 9.                               
000041         05  FS3B            PIC X(10).                            
000046     03  FS4.                                                     
000051         05  FS4A            PIC 99.                              
000056         05  FS4B            PIC 99.                              
000061         05  FS4C            PIC 99.                              
000066     03  FS5                 PIC X(5).                              
000071     03  FS6                 PIC X(20).                           
000076     03  FS7                 PIC 9.                               
000081     03  FS8                 PIC S9(9)V99    COMP-3.              
000086     03  FS9                 PIC S9(9)V99    COMP-3.              
000091     03  FS10                PIC 9.                               
000096     03  FS11                PIC S9(9)V99    COMP-3.              
000101     03  FS12                PIC S9(9)V99    COMP-3.              
000106     03  FS13                PIC S9(9)V99    COMP-3.              
000111     03  FS14-15 OCCURS 10.                                       
000116         05  FS14            PIC 9.                               
000121         05  FS15            PIC S9(9)V99    COMP-3.              
000126         05  FS16            PIC S9(9)V99    COMP-3.              
000131     03  FS17 OCCURS 10       PIC S9(9)V99    COMP-3.              
000136     03 FS18                 PIC 9(6).                            
000141     03  FS19                PIC 9.                               
000241     03  FILLER              PIC X.    
```

To xml:

```xml
copybook filename="FD8.COP.CLEAN">
    <item display-length="428" level="01" name="REC-FACTURE" position="1" storage-length="428">
        <item display-length="1" level="03" name="FS1" picture="X" position="1" storage-length="1"/>
        <item display-length="9" level="03" name="FS2" position="2" storage-length="9">
            <item display-length="1" level="05" name="FS2A" numeric="true" picture="9" position="2" storage-length="1"/>
            <item display-length="8" level="05" name="RFS2B" picture="X(8)" position="3" redefined="true" storage-length="8"/>
            <item display-length="8" level="05" name="FS2B" numeric="true" picture="9(8)" position="3" redefines="RFS2B" storage-length="8"/>
        </item>
        <item display-length="11" level="03" name="FS3" position="11" storage-length="11">
            <item display-length="1" level="05" name="FS3A" numeric="true" picture="9" position="11" storage-length="1"/>
            <item display-length="10" level="05" name="FS3B" picture="X(10)" position="12" storage-length="10"/>
        </item>
        <item display-length="6" level="03" name="FS4" position="22" storage-length="6">
            <item display-length="2" level="05" name="FS4A" numeric="true" picture="99" position="22" storage-length="2"/>
            <item display-length="2" level="05" name="FS4B" numeric="true" picture="99" position="24" storage-length="2"/>
            <item display-length="2" level="05" name="FS4C" numeric="true" picture="99" position="26" storage-length="2"/>
        </item>
        <item display-length="5" level="03" name="FS5" picture="X(5)" position="28" storage-length="5"/>
        <item display-length="20" level="03" name="FS6" picture="X(20)" position="33" storage-length="20"/>
        <item display-length="1" level="03" name="FS7" numeric="true" picture="9" position="53" storage-length="1"/>
        <item display-length="11" level="03" name="FS8" numeric="true" picture="S9(9)V99" position="54" scale="2" signed="true" storage-length="6" usage="computational-3"/>
        <item display-length="11" level="03" name="FS9" numeric="true" picture="S9(9)V99" position="60" scale="2" signed="true" storage-length="6" usage="computational-3"/>
        <item display-length="1" level="03" name="FS10" numeric="true" picture="9" position="66" storage-length="1"/>
        <item display-length="11" level="03" name="FS11" numeric="true" picture="S9(9)V99" position="67" scale="2" signed="true" storage-length="6" usage="computational-3"/>
        <item display-length="11" level="03" name="FS12" numeric="true" picture="S9(9)V99" position="73" scale="2" signed="true" storage-length="6" usage="computational-3"/>
        <item display-length="11" level="03" name="FS13" numeric="true" picture="S9(9)V99" position="79" scale="2" signed="true" storage-length="6" usage="computational-3"/>
        <item display-length="13" level="03" name="FS14-15" occurs="10" position="85" storage-length="13">
            <item display-length="1" level="05" name="FS14" numeric="true" picture="9" position="85" storage-length="1"/>
            <item display-length="11" level="05" name="FS15" numeric="true" picture="S9(9)V99" position="86" scale="2" signed="true" storage-length="6" usage="computational-3"/>
            <item display-length="11" level="05" name="FS16" numeric="true" picture="S9(9)V99" position="92" scale="2" signed="true" storage-length="6" usage="computational-3"/>
        </item>
        <item display-length="11" level="03" name="FS17" numeric="true" occurs="10" picture="S9(9)V99" position="215" scale="2" signed="true" storage-length="6" usage="computational-3"/>
        <item display-length="6" level="03" name="FS18" numeric="true" picture="9(6)" position="275" storage-length="6"/>
        <item display-length="1" level="03" name="FS19" numeric="true" picture="9" position="281" storage-length="1"/>
   </item>
</copybook>
```

### Examples of Using cb2xml

* The [examples dircectory](examples) holds examples of using cb2xml output in a variety of languages

* This [stackoverflow answer](https://stackoverflow.com/questions/35846800/dynamically-reading-cobol-redefines-with-c-sharp/35977421#35977421)
how  one person used cb2xml to convert Cobol files to a database

* [JRecord](https://sourceforge.net/projects/jrecord/) uses cobol copybooks. The JRecord library supports reading/writing cobol files
from java

* [cobol2j](https://sourceforge.net/projects/cobol2j/) uses an Xml transform to convert cb2xml xml to it own Xml format

