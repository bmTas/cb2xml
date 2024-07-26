       05 Order-customer-gr.
          10 order-customer occurs 5 times    
                                     pic x(35).

       05 order-bank-gr redefines order-customer-gr.
          10 order-cust-line1        pic x(6).
          
       05 order-bank-gr1 redefines order-customer-gr.
          10 order-cust-line2        pic x(12).

       05 order-bank-gr2 redefines order-bank-gr1.
          10 order-cust-line3        pic x(7).
          
       05 order-party                pic x(10).