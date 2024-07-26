          05 First-field             pic x(20).
          05 Order-customer-gr.
             10 order-customer occurs 5 times.    
                15 fld1              pic x(10).
                15 fld2              pic x(30).
                15 redefines fld2.
                   20 fld3a          pic x(8).
                   20 fld3b          pic x(3).
                15 redefines fld2.
                   20 fld4a          pic x(5).

          05 order-bank-gr redefines order-customer-gr.
             15 fld1                 pic x(10).
             15 fld2                 pic x(30).
             15 redefines fld2.
                20 fld3a             pic x(8).
                20 fld3b             pic x(3).
             15 redefines fld2.
                20 fld4a             pic x(5).
          
          05 order-bank-gr1 redefines order-customer-gr.
             10 order-customer occurs 6 times.    
                15 fld1              pic x(10).
                15 fld2              pic x(30).
                15 redefines fld2.
                   20 fld3a          pic x(8).
                   20 fld3b          pic x(3).
                15 redefines fld2.
                   20 fld4a          pic x(5).
                   20 fld4b          pic x(10).

          05 order-bank-gr3 redefines order-customer-gr.
             10 order-cust-line2     pic x(12).
          
          05 order-party             pic x(10).