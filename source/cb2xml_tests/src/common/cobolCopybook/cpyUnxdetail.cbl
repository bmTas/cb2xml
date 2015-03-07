*********************************************************************
********** DETAIL RECORD.                                          **
     ****   MAKE SURE ANY CHANGES HERE ARE REFLECTED IN THE TBL-  **
     ****      FOR DTL RECORDS TOWARDS THE BOTTOM                 **
*********************************************************************
       01  UNX-STMT-DETAIL-RECORD.
           02  UNX-DTL-INFO.
               03  UNX-DTL-REC-TYPE                 PIC X(03)
                                                  VALUE 'DTL'.
               03  UNX-DTL-SYS-PRIN                 PIC X(08).
               03  UNX-DTL-ACCT-NUMBER              PIC X(20).
           02  UNX-DETAIL-RECORD.
               03  UNX-DTL-RECORD-TYPE          PIC X(03).
               03  UNX-DTL-PARTNER-ID           PIC 9(08).
               03  UNX-DTL-MEMBER-ID            PIC 9(13).
               03  UNX-DTL-ARRANGE-NUM          PIC 9(02).
               03  UNX-DTL-STMT-ORDER           PIC 9(02).
               03  UNX-DTL-INVOICE-NUM          PIC X(11).
               03  UNX-DTL-LAYER                PIC 9(03).
               03  UNX-DTL-PARENT-GROUP         PIC 9(03).
               03  UNX-DTL-GROUP-ID             PIC 9(03).
               03  UNX-DTL-SUB-GROUP-ID         PIC 9(03).
               03  UNX-DTL-TRAN-DATE.
                   05  UNX-DTL-TRAN-YYYY        PIC 9(04).
                   05  UNX-DTL-TRAN-MM          PIC 9(02).
                   05  UNX-DTL-TRAN-DD          PIC 9(02).
               03  UNX-DTL-PREFIX-NUM           PIC 9(03).
               03  UNX-DTL-RUN-NUM              PIC 9(05).
               03  UNX-DTL-DETAIL-NUM           PIC 9(07).
               03  UNX-DTL-SIBLING-NUM          PIC 9(03).
               03  UNX-DTL-SIBLING-MSG-IND      PIC X(01).
               03  UNX-DTL-SEQ-NUM              PIC 9(03).
               03  UNX-DTL-BILLER-ACCT-NUM      PIC X(20). 
               03  UNX-DTL-LOB                  PIC X(02).
               03  UNX-DTL-DETAIL-CATEGORY      PIC X(02).
               03  UNX-DTL-DETAIL-TYPE          PIC X(05).
               03  UNX-DTL-DETAIL-TRAN-TYPE     PIC X(03).
               03  UNX-DTL-LEVEL                PIC 9(02).
               03  UNX-DTL-LINE-DEF             PIC X(05).
               03  UNX-DTL-LINE-DEF-2           PIC X(05).
               03  UNX-DTL-LINE-DEF-3           PIC X(05).
               03  UNX-DTL-COLUMN-DEF           PIC X(05).
               03  UNX-DTL-ICON-ID              PIC X(08).
               03  UNX-DTL-LOCATION             PIC X(01).
               03  UNX-DTL-GROUP-TYPE           PIC X(01).
               03  FILLER                       PIC X(03).
               03  UNX-DTL-MSG-ID               PIC X(08).
               03  UNX-DTL-MSG-TEXT             PIC X(500).
               03  UNX-DTL-POST-DATE.
                   05  UNX-DTL-POST-YYYY        PIC 9(04).
                   05  UNX-DTL-POST-MM          PIC 9(02).
                   05  UNX-DTL-POST-DD          PIC 9(02).
               03  UNX-DTL-FROM-DATE.
                   05  UNX-DTL-FROM-YYYY        PIC 9(04).
                   05  UNX-DTL-FROM-MM          PIC 9(02).
                   05  UNX-DTL-FROM-DD          PIC 9(02).
               03  UNX-DTL-FROM-TIME.
                   05  UNX-DTL-FROM-HOUR        PIC 9(02).
                   05  UNX-DTL-FROM-MINUTE      PIC 9(02).
                   05  UNX-DTL-FROM-SECOND      PIC 9(02).
               03  UNX-DTL-THRU-DATE.
                   05  UNX-DTL-THRU-YYYY        PIC 9(04).
                   05  UNX-DTL-THRU-MM          PIC 9(02).
                   05  UNX-DTL-THRU-DD          PIC 9(02).
               03  UNX-DTL-THRU-TIME.
                   05  UNX-DTL-THRU-HOUR        PIC 9(02).
                   05  UNX-DTL-THRU-MINUTE      PIC 9(02).
                   05  UNX-DTL-THRU-SECOND      PIC 9(02).
               03  UNX-DTL-CHARGE               PIC S9(11)V99.
               03  UNX-DTL-PHANTOM-ID           PIC X(01).
               03  UNX-DTL-REFERENCE-NUM.
                   05  UNX-DTL-REF-NUM-4        PIC X(4).
                   05  UNX-DTL-REF-NUM-5        PIC X(5).
               03  UNX-DTL-PROD-SERV-REF        PIC X(08).
               03  UNX-DTL-ITEM-NUMBER          PIC X(05).
               03  UNX-DTL-EQUIP-TELNO          PIC X(20).
               03  UNX-DTL-EQUIP-PID            PIC 9(05).
               03  UNX-DTL-EQUIP-SID            PIC 9(05).
               03  UNX-DTL-BUSINESS-ID          PIC X(05).
               03  UNX-DTL-VARIABLE1            PIC S9(11)V99.
               03  UNX-DTL-VARIABLE2            PIC S9(11)V99.
               03  UNX-DTL-VARIABLE3            PIC S9(11)V99.
               03  UNX-DTL-VARIABLE4            PIC S9(11)V99.
               03  UNX-DTL-ROLLUP-FLAG          PIC X(01).
               03  UNX-DTL-PRINT-HEADING        PIC X(01).
               03  UNX-DTL-RESERVED-UNIX1       PIC X(04).
               03  UNX-DTL-SIBLING-INFORMATION.
                   05  UNX-DTL-SIBLING-MMDD     PIC 9(04).
                   05  UNX-DTL-SIBLING-SEQ      PIC 9(04).                     
                   05  FILLER   REDEFINES UNX-DTL-SIBLING-SEQ
                                                PIC X(4).
                   05  UNX-DTL-SIBLING-ID       PIC X(02).
               03  UNX-DTL-ACCT-HIERARCHY-INFO.
                   05  UNX-DTL-AH-REPORT-GROUP  PIC X(08).
                   05  UNX-DTL-AH-LEVELS-DOWN   PIC X(04).
                   05  UNX-DTL-AH-GROUP-ID      PIC X(08).
                   05  UNX-DTL-AH-SUBSCRIBER-NR PIC X(16).
                   05  UNX-DTL-AH-LEAF-PRIN-NM  PIC X(26).
               03  UNX-DTL-GRAPHIC-LOC          PIC X(02).
               03  UNX-DTL-AMT-LITERAL          PIC X(08).
               03  UNX-DTL-PRIMARY-PHONE-FLAG   PIC X(01).
               03  UNX-DTL-CTRLBRK-PHONE        PIC X(20).
               03  UNX-DTL-UNIT-QTY             PIC 9(04).
               03  UNX-DTL-BULK-SERVICE-IND     PIC X(01).
               03  UNX-DTL-GRAPHIC-FORMAT       PIC X(03).
               03  UNX-DTL-CMSG-FLAG            PIC X(01).
               03  unx-dtl-transfer-account     pic x(16).
               03  FILLER                       PIC X(16).
               
               
               
               03  UNX-DTL-LOB-DATA             PIC X(750).
               03  UNX-DTL-TELEPHONY-DATA REDEFINES UNX-DTL-LOB-DATA.
                   05  UNX-DTLT-BILLING-NUM     PIC X(20).
                   05  UNX-DTLT-DURATION        PIC 9(06).
                   05  FILLER REDEFINES UNX-DTLT-DURATION.
                       10  UNX-DTLT-MIN         PIC 9(04).
                       10  UNX-DTLT-SEC         PIC 9(02).
                   05  UNX-DTLT-CALL-TYPE       PIC X(02).
                   05  UNX-DTLT-RATE            PIC X(02).
                   05  UNX-DTLT-PLCMNT-CD       PIC X(04).
                   05  UNX-DTLT-OCP             PIC X(05).
                   05  UNX-DTLT-CARRIER-CD      PIC X(04).
                   05  UNX-DTLT-CARRIER-VALUE   PIC X(04).
                   05  UNX-DTLT-CALL-AMT1       PIC -9(11)V99.
                   05  UNX-DTLT-CALL-AMT2       PIC -9(11)V99.
                   05  UNX-DTLT-CALL-AMT3       PIC -9(11)V99.
                   05  UNX-DTLT-FROM-NUM        PIC X(20).
                   05  UNX-DTLT-FROM-CITY       PIC X(24).
                   05  UNX-DTLT-FROM-STATE      PIC X(02).
                   05  UNX-DTLT-FROM-COUNTRY.           
                       10  UNX-DTLT-FROM-CNTRY  PIC X(03).          
                       10  FILLER               PIC X(21).         
                   05  UNX-DTLT-TO-NUM          PIC X(20).
                   05  UNX-DTLT-TO-CITY         PIC X(24).                     
                   05  UNX-DTLT-TO-STATE        PIC X(02).
                   05  UNX-DTLT-TO-COUNTRY.           
                       10  UNX-DTLT-TO-CNTRY    PIC X(03).          
                       10  FILLER               PIC X(21).         
                   05  UNX-DTLT-QUANTITY        PIC -9(13).
                   05  UNX-DTLT-TEL-PAC-CODE    PIC X(30).
                   05  FILLER                   PIC X(477).
               03  UNX-DTL-PARM-DATA REDEFINES UNX-DTL-LOB-DATA.
                   05  UNX-DTL-PARM-TYPE          PIC X(01).
                   05  UNX-DTL-PARM-STMT-ORDER    PIC 9(05).
                   05  unx-dtl-parm-sibling-order pic 9(03).
                   05  UNX-DTL-PARM-TREE-LEVEL    PIC 9(03).
                   05  UNX-DTL-PARM-REF-NO        PIC X(20).
                   05  UNX-DTL-PARM-ROOT-ID       PIC X(20).
                   05  UNX-DTL-PARM-PARENT-ID     PIC X(20).
                   05  UNX-DTL-PARM-SID           PIC 9(05).
                   05  UNX-DTL-PARM-TYPE          PIC X(20).
                   05  UNX-DTL-PARM-OFFER-ID      PIC 9(08).
                   05  UNX-DTL-PARM-PRODUCT-ID    PIC 9(08).
                   05  UNX-DTL-PARM-PRICE-PLAN-ID PIC 9(08).
                   05  UNX-DTL-PARM-VALUE         PIC X(255).
                   05  UNX-DTL-ID-AREA REDEFINES UNX-DTL-PARM-VALUE.
                       10  UNX-DTL-SAVINGS-AMT            PIC -9(11)V99.
                       10  UNX-DTL-TRANS-REMAIN-MTHS      PIC 9(03).
                       10  UNX-DTL-TRANS-END-DATE         PIC 9(07).
                       10  UNX-DTL-BUNDLE-TYPE-IND        PIC X(01).
                       10  UNX-DTL-DISCOUNT-IND           PIC X(01).
                       10  UNX-DTL-BLANK-OUT-PRICE        PIC X(01).
                       10  UNX-DTL-NUM-TEXT-IDS           PIC 9(02).
                       10  UNX-DTL-ID-TBL
                               OCCURS 10 TIMES.
                           15  UNX-DTL-ID-TYPE            PIC 9(03).
                           15  UNX-DTL-LANGUAGE           PIC X(01).
                           15  UNX-DTL-VALUE              PIC 9(10).
                       10  UNX-DTL-DISCOUNT-CODE          PIC X(02).
                       10  UNX-DTL-LANGUAGE-IND           PIC X(01).
                       10  UNX-DTL-PRM-ACTUAL-LANG        PIC X(01).
                       10  FILLER                         PIC X(82).
                   05  unx-dtl-oarm-message-id redefines
                       unx-dtl-parm-value.
                       10  unx-dtl-parm-msg-text-key      pic 9(10).
                       10  unx-dtl-parm-msg-location      pic 9(03).
                       10  unx-dtl-parm-payload-area occurs 2 times.
                           15  unx-dtl-parm-variable      pic x(20).
                           15  unx-dtl-parm-resolved-var  pic x(50).
                       10  unx-dtl-parm-msg-line-def-4    pic x(05).
                       10  unx-dtl-parm-msg-line-def-5    pic x(05).
                       10  filler                         pic x(92).
                   05  UNX-DTL-PRM-PRO-MSG-SIBLING REDEFINES
                           UNX-DTL-PARM-VALUE.
                       10  UNX-DTL-PRM-PRO-MSG-KEY        PIC 9(10).
                       10  UNX-DTL-PRM-PRO-MSG-LOCATION   PIC 9(03).
                       10  UNX-DTL-PRM-PRO-MSG-PAYLOAD OCCURS 4 TIMES.
                           15  UNX-DTL-PRM-PRO-MSG-VAR    PIC X(20).
                           15  UNX-DTL-PRM-PRO-MSG-TXT    PIC X(30).
                       10  FILLER                         PIC X(32).
                   05  UNX-DTL-PARM-PRORATE-IND   PIC X(01).
                   05  UNX-DTL-OFFER-COMP-ID      PIC 9(10).
                   05  unx-dtl-bundle-comp-id redefines
                       unx-dtl-offer-comp-id      pic 9(10).
                   05  FILLER                     PIC X(354).
               03  UNX-DTL-PRORATE-PARA-DATA REDEFINES UNX-DTL-LOB-DATA.
                   05  UNX-PROPARA-KEY.
                       10  UNX-PROPARA-BILL-DATE          PIC 9(08).
                       10  UNX-PROPARA-TYPE-IND           PIC X(01).
                       10  UNX-PROPARA-SENTENCE-TYPE      PIC X(01).
                       10  UNX-PROPARA-ORIG-GROUP-ID      PIC 9(03).
                       10  UNX-PROPARA-ORIG-GROUP-ID2     PIC 9(03).
                   05  UNX-PRO-PARA-CONTROL-FLAGS.
                       10  UNX-PROPARA-NEW-PARAGRAPH      PIC X(01).
                       10  UNX-PROPARA-NEW-SENTENCE       PIC X(01).
                       10  UNX-PROPARA-END-PARENT         PIC X(01).
                       10  UNX-PROPARA-END-SENTENCE       PIC X(01).
                       10  UNX-PROPARA-END-PARAGRAPH      PIC X(01).
                   05  UNX-PROPARA-DETAILS.  
                       10  UNX-PROPARA-TEXT-TYPE          PIC 9(03).
                       10  UNX-PROPARA-LANG-IND           PIC X(01).
                       10  UNX-PROPARA-TEXT-ID            PIC 9(10).
                       10  UNX-PROPARA-PARENT-TEXT-TYPE   PIC 9(03).
                       10  UNX-PROPARA-PARENT-TEXT-ID     PIC 9(10).
                       10  UNX-PROPARA-MONTHLY-RATE       PIC 9(11)V99.
                       10  UNX-PROPARA-DISC-RATE          PIC 9(11)V99.
                       10  UNX-PROPARA-NET-RATE           PIC 9(11)V99.
                       10  UNX-PROPARA-SHORT-DESC         PIC X(24).
                       10  UNX-PROPARA-SHORT-DISC-DESC    PIC X(24).
                       10  UNX-PROPARA-PRO-OFFER-SUP-IND  PIC X(01).
                       10  UNX-PROPARA-PARENT-MTHLY-RATE  PIC 9(11)V99.
                       10  UNX-PROPARA-PARENT-DISC-RATE   PIC 9(11)V99.
                       10  UNX-PROPARA-PARENT-NET-RATE    PIC 9(11)V99.
                       10  UNX-PROPARA-NET-QTY-CHG        PIC 9(04).
                       10  UNX-PROPARA-PKG-SVC            PIC X(05).
                       10  UNX-PROPARA-DISC-CODE          PIC X(02).
                       10  UNX-PROPARA-OFFER-ID           PIC 9(10).
                       10  UNX-PROPARA-OFFER-COMP-ID      PIC 9(10).
                       10  UNX-PROPARA-PRICE-PLAN-ID      PIC 9(10).
                   05  FILLER                             PIC X(534).
               03  UNX-DTL-AH-DATA REDEFINES UNX-DTL-LOB-DATA.
                   05  UNX-DTL-AH-LEAF-ADDRESS-1      PIC X(26).
                   05  UNX-DTL-AH-LEAF-ADDRESS-2      PIC X(26).
                   05  UNX-DTL-AH-LEAF-CITY           PIC X(18).
                   05  UNX-DTL-AH-LEAF-STATE          PIC X(02).
                   05  UNX-DTL-AH-LEAF-ZIP-CODE       PIC X(09).
                   05  UNX-DTL-AH-LEAF-TELEPHONE-NBR  PIC X(20).
                   05  FILLER                         PIC X(649).
               03  UNX-DTL-CMSG-DATA REDEFINES UNX-DTL-LOB-DATA.
                   05  UNX-DTL-CMSG-TEXT-KEY        PIC X(10).
                   05  UNX-DTL-CMSG-LOCATION        PIC 9(03).
                   05  UNX-DTL-CMSG-PAYLOAD-AREA
                               OCCURS 10 TIMES.
                       10  UNX-DTL-CMSG-VAR         PIC X(20).
                       10  UNX-DTL-CMSG-VAR-TXT     PIC X(50).
                   05  FILLER                       PIC X(37).
               03  UNX-DTL-EOR                  PIC X(01).