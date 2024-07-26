000101 01   MASTER-RECORD          COMP-3.
000102         05  COMP-DATA.
000103             07  BEGIN-BAL  PIC S9(13)V99.
000104             07  NO-CREDITS PIC S9(7).
000105             07  CR-AMT     PIC S9(13)V99.
000106             07  NO-DEBITS  PIC S9(7).
000107             07  DR-AMT     PIC S9(13)V99.
000108             07  SERV-CHG   PIC S9(13)V99.
000109             07  ITEMS      PIC S9(7).
000110             07  LINES      PIC S9(3).
000111             07  PAGE       PIC S9(5).
000112             07  DUPL       PIC S9.
                   07  Fld-1      pic x(3).
                   07  Fld-2      pic --9. 
                   07  Fld-3      pic 9.99.
                   07  Fld-4      PIC S9(13)V99.
000113         05  SPECL-HANDL    PIC X.
               05  zxc            Pic s99.