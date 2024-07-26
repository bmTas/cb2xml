          05 First-field             pic x(20).
          05 Order-customer-gr.
             10 order-customer occurs 5 times.    
                15 fld1              pic x(10).
                15 fld2              pic x(30).
                15 redefines fld2.
                 17 occurs 2.
                   20 fld3b          pic x(40).
                   20 redefines fld3b.
                      25 fld3a1       pic x(3).
                      25 fld3a2       pic x(30).
                      25 filler redefines fld3a2.
                        30 fld3a2a    pic x(3).
                        30 fld3a2b    pic x(4).
                      25 fld3a3       pic x(3).
                15 redefines fld2.
                   20 fld4a          pic x(5).
                   
                15 fld5              pic x(15).

          05 order-bank-gr redefines order-customer-gr.
             15 fld1                 pic x(10).
             15 fld2                 pic x(30).
             15 redefines fld2.
                20 fld3a             pic x(8).
                20 fld3b             pic x(3).
             15 redefines fld2.
                20 fld4a             pic x(5).
          
          05 order-bank-gr1 redefines order-customer-gr.
            10 order-customer occurs 3 times.    
                15 fld1              pic x(10).
                15 fld2              pic x(30).
                15 redefines fld2.
                 17 occurs 2.
                   20 fld3b          pic x(40).
                   20 redefines fld3b.
                      25 fld3a1       pic x(3).
                      25 fld3a2       pic x(30).
                      25 filler redefines fld3a2.
                        30 fld3a2a    pic x(3).
                        30 fld3a2b    pic x(4).
                      25 fld3a3       pic x(3).
                15 redefines fld2.
                   20 fld4a          pic x(5).
                   
                15 fld5              pic x(15).

          05 order-bank-gr3 redefines order-customer-gr.
             10 order-cust-line2     pic x(12).
          
          05 order-party             pic x(10).