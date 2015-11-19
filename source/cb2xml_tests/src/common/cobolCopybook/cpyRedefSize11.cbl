       05 Order-customer-gr.
          10 order-customer occurs 5 times.    
             15 fld1                 pic x(10).
             15 fld2                 pic x(30).
             15 redefines fld2.
                20 fld3a             pic x(8).
                20 fld3b             pic x(3).
             15 redefines fld2.
                20 fld4a             pic x(5).

       05 order-bank-gr redefines order-customer-gr.
          10 order-cust-line1        pic x(6).
          
       05 order-bank-gr1 redefines order-customer-gr.
          10 order-cust-line2        pic x(12).
          
       05 order-party                pic x(10).