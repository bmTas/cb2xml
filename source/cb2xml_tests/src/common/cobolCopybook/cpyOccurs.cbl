******************************************************** 
*********
*********  This copybook caused problems in earlier releases of cb2xml
*********  so it has been included in automated tests.
********* 
******************************************************** 
         01 BI-BALANCE-INQUIRY-RS.
            05 BI-ACCT-BAL-RS.
               10 BI-BAL-TYPE-RS OCCURS 10 TIMES
                  PIC X(80).
                  88 AVAIL-BALANCE VALUE 'Avail', 'AVAIL'.
            05 BI-ACCT-NAME PIC X(100).