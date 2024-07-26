           10  ORDERING-INFORMATION.                             
             15  ORDER-CUSTOMER-GR.                            
                 20  ORDER-CUSTOMER                            
                   OCCURS 5 TIMES           PIC X(35).         
             15  FILLER REDEFINES ORDER-CUSTOMER-GR.           
                 20  ORD-CUST-LINE1         PIC X(06).         
                     88  ORD-CUST-TRAVEL-RULE                  
                         VALUE 'TEST'. 
             15  ORDER-BANK-GR.                                
                 20  ORDER-BANK                                
                   OCCURS 5 TIMES           PIC X(35).        
             15  ORDER-BANK-CUST-ID         PIC X(10).