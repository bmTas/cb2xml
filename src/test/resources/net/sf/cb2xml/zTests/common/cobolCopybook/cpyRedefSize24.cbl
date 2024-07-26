       03 start-1                    pic x(10).
       03 record-def.
          05 Order-customer-gr.
             10 order-customer occurs 5 times    
                                     pic x(35).

          05 order-bank-gr redefines order-customer-gr.
             10 order-cust-line1     pic x(6).
          
          05 order-bank-gr1 redefines order-customer-gr.
             10 order-cust-line2     pic x(12).

          05 order-bank-gr2 redefines order-bank-gr1.
             10 order-cust-line3     pic x(7).

          05 order-bank-gr3 redefines order-bank-gr2.
             10 order-cust-line3     pic x(11).

          05 order-bank-gr4 redefines order-bank-gr3.
             10 order-cust-line3     pic x(9).

          05 order-bank-gr5 redefines order-bank-gr3.
             10 order-cust-line3     pic x(6).
 
          05 order-bank-gr6 redefines Order-customer-gr.
             10 order-cust-line3     pic x(8).
          
          05 order-party             pic x(10).