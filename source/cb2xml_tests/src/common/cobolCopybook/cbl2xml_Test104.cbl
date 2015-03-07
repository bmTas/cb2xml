      *********************************************************
      *    FILE LAYOUT OF THE DISPATCH EXTRACT                *
      *    NOTE: USES SAME FIELD NAMES AS STD. DB2 COPYBOOKS  *
      *          BUT ALL PACKED FIELDS ARE NOW IN UNSIGNED    *
      *          DISPLAY FORMAT, FOR EASIER TRANSFER/IMPORT   *
      *                                                       *
      **** 15/03/00.   150 BYTES.                             *
      *    27.04.00.   200 BYTES.                             *
      *********************************************************
      *
      *CHANGE CONTROL.
      *-----------------------------------------------------------------
      *VERS|  DATE  |DESCRIPTION                            | AUTHOR
      *----+--------+---------------------------------------+-----------
      *V01 |15/03/00|FIRST ISSUE                            | J.BINDER
      *----+--------+---------------------------------------+-----------
      *V02 |27.04.00|INCLUDE ALL KEY COLUMNS AND ADD RECORD | J.BINDER
      *    |{OR011} |TYPES AP AND AR                        |
      *----+--------+---------------------------------------+-----------
      *V03 |18.05.00|MOVE COUNTS OF DS & DI RECORDS FROM    | J.BINDER
      *    |{OR075} |THE DH RECORD TO THE DO RECORD         |
      *----+--------+---------------------------------------+-----------
      *V04 |16.06.00|ADD ASN#, ASN_SEQ#, & BRAND_DC TO "DO" | J.BINDER
      *    |{OR021} |ADD ASN#, ASN_SEQ#, & RCPT#    TO "DS" |
      *    |        |ADD RCPT#                      TO "DI" |
      *----+--------+---------------------------------------+-----------
      *V05 |24.07.00|ADD NEW RECORD TYPE "DP", SIMILAR TO   | J.BINDER
      *    |{OR143} |"DI" BUT FOR COMPLIANT SHIPMENTS ONLY. |
      *    |        |NOTE THIS IS A SUMMARY OF "AP" RECORDS |
      *----+--------+---------------------------------------+-----------
      *
      *01  RECORD.
      *     03  RECORD-TYPE            PIC X(2).

      *         88  REC-TYPE-FH                  VALUE 'FH'.

      *         88  REC-TYPE-DH                  VALUE 'DH'.
      *         88  REC-TYPE-DO                  VALUE 'DO'.
      *         88  REC-TYPE-DS                  VALUE 'DS'.
      *         88  REC-TYPE-AP                  VALUE 'AP'.
      *         88  REC-TYPE-AR                  VALUE 'AR'.
      *         88  REC-TYPE-DP                  VALUE 'DP'.
      *         88  REC-TYPE-DI                  VALUE 'DI'.

      *         88  REC-TYPE-FT                  VALUE 'FT'.

      * ========================================================
           03  DETAIL-RECORD.
      * ========================================================

      * FH     =================================================
      * FH     FILE HEADER
      * FH     =================================================
           05  RECORD-TYPE            PIC X(2).

           05  RECORD-TYPE            PIC X(2).

               05  FH-DESCRIPTION     PIC X(8).
               05  FH-BRAND-ID        PIC X(3).
               05  FH-CREATE-DATE     PIC X(10).
      *                                       'DD/MM/CCYY'
               05  FH-CREATE-TIME     PIC X(08).
      *                                       'HH:MM:SS'

               05  FH-FILLER          PIC X(169).

      * DH     =================================================
      * DH     DISPATCH HEADER
      * DH     =================================================
           03  DH REDEFINES DETAIL-RECORD.
               05  RECORD-TYPE            PIC X(2).


               05  DH-EQV033.
                   10 MNFST-NO-DH          PIC 9(9).
                   10 DC-NO-DH             PIC 9(4).
                   10 BRAND-ID-DH          PIC X(3).
                   10 STR-NO-DH            PIC 9(4).
                   10 DESP-DT-DH           PIC X(10).
      *                                       'DD.MM.CCYY'
                   10 DESP-TM-DH           PIC X(8).
      *                                       'HH.MM.SS'
                   10 OCCURS-TYPE-A-DH     PIC X(1).
                   10 OCCURS-TYPE-B-DH     PIC X(1).
                   10 OCCURS-TYPE-C-DH     PIC X(1).
                   10 PROCESSED-IND-DH     PIC X(1).
                   10 C-USR-UPDT-DH        PIC X(8).
                   10 TS-UPDT-DH           PIC X(26).
      *                                   'CCYY-MM-DD-HH.MM.SS.SSSSSS'

               05  DH-NUM-DO          PIC 9(09).

               05  DH-FILLER          PIC X(113).

      * DO     =================================================
      * DO     DISPATCH ORDER
      * DO     =================================================
           03  DO REDEFINES DETAIL-RECORD.
               05  RECORD-TYPE            PIC X(2).


               05  DO-EQV036.
                   10 MNFST-NO-DO          PIC 9(9).
                   10 DC-NO-DO             PIC 9(4).
                   10 BRAND-ID-DO          PIC X(3).
                   10 STR-NO-DO            PIC 9(4).
                   10 ORDER-NO-DO          PIC X(12).
                   10 RECEIPT-NO-DO        PIC 9(9).
                   10 ORDER-TYPE-DO        PIC X(1).
                   10 TOT-INNERS-DESP-DO   PIC 9(7).
                   10 C-USR-UPDT-DO        PIC X(8).
                   10 TS-UPDT-DO           PIC X(26).
      *                                   'CCYY-MM-DD-HH.MM.SS.SSSSSS'

               05  DO-NUM-DS          PIC 9(09).
               05  DO-NUM-DP          PIC 9(09).
               05  DO-NUM-DI          PIC 9(09).

               05  ASN-DO                  PIC X(30).
               05  ASN-SEQ-NO-DO           PIC 9(9).
               05  BRAND-DC-DO             PIC 9(4).

               05  DO-FILLER          PIC X(45).

      * DS     =================================================
      * DS     DISPATCH SCM (COMPLIANT)
      * DS     =================================================
           03  DS REDEFINES DETAIL-RECORD.

               05  RECORD-TYPE            PIC X(2).

               05  DS-EQV034.
                   10 MNFST-NO-DS          PIC 9(9).
                   10 DC-NO-DS             PIC 9(4).
                   10 BRAND-ID-DS          PIC X(3).
                   10 STR-NO-DS            PIC 9(4).
                   10 ORDER-NO-DS          PIC X(12).
                   10 SCM-INNER-DS         PIC X(20).
                   10 SCM-OUTER-DS         PIC X(20).
                   10 C-USR-UPDT-DS        PIC X(8).
                   10 TS-UPDT-DS           PIC X(26).
      *                                   'CCYY-MM-DD-HH.MM.SS.SSSSSS'

               05  DS-NUM-AP          PIC 9(09).
               05  DS-NUM-AR          PIC 9(09).

               05  ASN-DS                  PIC X(30).
               05  ASN-SEQ-NO-DS           PIC 9(9).
               05  RECEIPT-NO-DS           PIC 9(9).

      *        05  DS-FILLER          PIC X(074).
               05  DS-FILLER          PIC X(026).

      * AP     =================================================
      * AP     ASN PRODUCT
      * AP     =================================================
           03  AP REDEFINES DETAIL-RECORD.

               05  RECORD-TYPE            PIC X(2).

               05  AP-EQV007.
                   10 ASN-AP               PIC X(30).
                   10 ASN-SEQ-NO-AP        PIC 9(9).
                   10 ORDER-NO-AP          PIC X(12).
                   10 STORE-AP             PIC 9(4).
                   10 SCM-AP               PIC X(20).
                   10 SCM-SEQ-NO-AP        PIC 9(9).
                   10 PROD-NO-AP           PIC 9(14).
                   10 PROD-SEQ-NO-AP       PIC 9(5).
                   10 PROD-QUALIFIER-AP    PIC X(2).
                   10 QTY-SHIPPED-AP       PIC 9(10)V9(2).
                   10 PROD-MEASURE-CD-AP   PIC X(2).
                   10 USE-BY-DATE-AP       PIC X(10).
      *                                       'DD.MM.CCYY'
                   10 KEYCODE-AP           PIC 9(8).
                   10 ERROR-CODE-AP        PIC X(2).
                   10 DANGR-GOODS-NO-AP    PIC 9(4).
                   10 DANGR-GOODS-CLS-AP   PIC 9(3)V9(1).
                   10 C-USR-UPDT-AP        PIC X(8).
                   10 TS-UPDT-AP           PIC X(26).
      *                                   'CCYY-MM-DD-HH.MM.SS.SSSSSS'

               05  AP-FILLER          PIC X(017).

      * AR     =================================================
      * AR     SCM CONTENTS CHECKING
      * AR     =================================================
           03  AR REDEFINES DETAIL-RECORD.

               05  RECORD-TYPE            PIC X(2).

               05  AR-EQV044.
                   10 BRAND-ID-AR          PIC X(3).
                   10 ORDER-NO-AR          PIC X(12).
                   10 LOGISTICS-LOC-NOAR   PIC 9(4).
                   10 RECEIPT-NO-AR        PIC 9(9).
                   10 SCM-AR               PIC X(20).
                   10 PROD-NO-AR           PIC 9(14).
                   10 PROD-QUALIFIER-AR    PIC X(2).
                   10 QUALITY-CODE-AR      PIC X(2).
                   10 CHECKED-QTY-AR       PIC 9(10)V9(2).
                   10 SUP-ID-AR            PIC 9(8).
                   10 ASN-AR               PIC X(30).
                   10 BRAND-DC-AR          PIC 9(4).
                   10 VALID-PROD-IND-AR    PIC X(1).
                   10 PROCESSED-IND-AR     PIC X(1).
                   10 CHECKED-BY-AR        PIC X(10).
                   10 CHECKED-DATE-AR      PIC X(10).
                   10 CHECKED-TIME-AR      PIC X(8).
                   10 C-USR-UPDT-AR        PIC X(8).
                   10 TS-UPDT-AR           PIC X(26).
      *                                   'CCYY-MM-DD-HH.MM.SS.SSSSSS'

               05  AR-FILLER          PIC X(014).

      * DP     =================================================
      * DP     DISPATCH PRODUCTS (COMPLIANT)
      * DP     =================================================
           03  DP REDEFINES DETAIL-RECORD.

               05  RECORD-TYPE            PIC X(2).

               05  DP-RECORD.
                   10 MNFST-NO-DP          PIC 9(9).
                   10 DC-NO-DP             PIC 9(4).
                   10 BRAND-ID-DP          PIC X(3).
                   10 STR-NO-DP            PIC 9(4).
                   10 ORDER-NO-DP          PIC X(12).
                   10 KEYCODE-DP           PIC 9(8).
                   10 PROD-NO-DP           PIC 9(14).
                   10 PROD-QUALIFIER-DP    PIC X(2).
                   10 DESP-QTY-DP          PIC 9(9).
                   10 ITMS-PER-CTN-QTYDP   PIC 9(6).
                   10 C-USR-UPDT-DP        PIC X(8).
                   10 TS-UPDT-DP           PIC X(26).
      *                                   'CCYY-MM-DD-HH.MM.SS.SSSSSS'

               05  RECEIPT-NO-DP           PIC 9(9).

               05  DP-FILLER          PIC X(84).

      * DI     =================================================
      * DI     DISPATCH ITEMS (NON-COMPLIANT)
      * DI     =================================================
           03  DI REDEFINES DETAIL-RECORD.

               05  RECORD-TYPE            PIC X(2).

               05  DI-EQV035.
                   10 MNFST-NO-DI          PIC 9(9).
                   10 DC-NO-DI             PIC 9(4).
                   10 BRAND-ID-DI          PIC X(3).
                   10 STR-NO-DI            PIC 9(4).
                   10 ORDER-NO-DI          PIC X(12).
                   10 KEYCODE-DI           PIC 9(8).
                   10 PROD-NO-DI           PIC 9(14).
                   10 PROD-QUALIFIER-DI    PIC X(2).
                   10 DESP-QTY-DI          PIC 9(9).
                   10 ITMS-PER-CTN-QTYDI   PIC 9(6).
                   10 C-USR-UPDT-DI        PIC X(8).
                   10 TS-UPDT-DI           PIC X(26).
      *                                   'CCYY-MM-DD-HH.MM.SS.SSSSSS'

               05  RECEIPT-NO-DI           PIC 9(9).


      * FT     =================================================
      * FT     FILE TRAILER
      * FT     =================================================
           03  FT REDEFINES DETAIL-RECORD.

               05  RECORD-TYPE            PIC X(2).

               05  FT-NUM-RECDS       PIC 9(09).
               05  FT-NUM-FH          PIC 9(09).
               05  FT-NUM-DH          PIC 9(09).
               05  FT-NUM-DO          PIC 9(09).
               05  FT-NUM-DS          PIC 9(09).
               05  FT-NUM-AP          PIC 9(09).
               05  FT-NUM-AR          PIC 9(09).
               05  FT-NUM-DP          PIC 9(09).
               05  FT-NUM-DI          PIC 9(09).
               05  FT-NUM-FT          PIC 9(09).

      * ========================================================