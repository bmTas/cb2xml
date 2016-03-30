       01  XXXRECORD.

           05  XXXBal2                    PIC S9(11)V99   COMP-3.
               88 oooo1                         VALUE -999999999.99 THRU
                                                     -000000000.01.
           05  XXXBal3                    PIC S9(13)V99      COMP-3.
           05  XXXBal4                    PIC S9(11)v99      COMP-5.
               88 oooo2                         VALUE -999999999.99.
           05  XXXBal5                    PIC S9(11)v99      COMP.
               88 oooo3                         VALUE -999999999.99 THRU
                                                      -000000000.01.
           05  XXXBal5                    PIC S9(11)v99 .     
               88 oooo3                         VALUE -999999999.99 THRU
                                                      -000000000.01.
           05  xxx comp-3.
               10  XXXBal6                PIC S9(11)v99.      
                   88 oooo6                     VALUE -999999999.99.
               10  XXXBal7                PIC S9(9)v99.      
               10  XXXBal8                PIC S9(11)v99.      
                   88 oooo8                     VALUE -999999999.99.
               10  XXXBal9                PIC S9(9)v99.      
           
           05  XXXBalz                    PIC S999           COMP-3.