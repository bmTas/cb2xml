002400  01 COBL-XREF-REC.                                               00003210
002450     05  XREF-HEADER.                                             00003306
002500*            COMBINED BILLING SORT CODE                           00003406
002600         10  CB-SORT-CODE           PIC X.                        00003506
002700*            STATISTICAL INDICATOR                                00003606
002800         10  STATISTICAL-IND        PIC X.                        00003706
002900*            HEADER PORTION LENGTH                                00003806
003000         10  HDR-PORTION-LENGTH     PIC XX.                       00003906
003100*            POLICY BODY LENGTH                                   00004006
003200         10  POL-BODY-LENGTH        PIC XX.                       00004106
003300         10  XBOLG-HEX REDEFINES                                  00004206
003350                  POL-BODY-LENGTH PIC 9(02) COMP.                 00004306
003400*            OVERFLOW LENGTH                                      00004406
003500         10  OVERFLOW-LENGTH        PIC XX.                       00004506
003600*            ARRANGEMENT TYPE                                     00004606
003700         10  ARRANGEMENT-TYPE       PIC X.                        00004706
003800*            ARRANGEMENT NUMBER                                   00004806
003900         10  ARRANGEMENT-NUMB       PIC 9(05) COMP-3.             00004906
004000*            PAY POINT NUMBER                                     00005006
004100         10  PAY-POINT-NUMB         PIC 9(03) COMP-3.             00005106
004200*            RECORD COUNT                                         00005206
004300*        10  XRCCT                  PIC 9(01) COMP-3.             00005306
004305         10  RECORD-COUNT           PIC X.                        00005412
004310             88 ONEREC              VALUE X'F1'.                  00005512
004330             88 FIRSTREC            VALUE X'01'.                  00005612
004400*            CSO                                                  00005706
004500         10  CSO                    PIC X.                        00005806
004600*            STATUS CODE                                          00005906
004700         10  STATUS-CD              PIC X.          
