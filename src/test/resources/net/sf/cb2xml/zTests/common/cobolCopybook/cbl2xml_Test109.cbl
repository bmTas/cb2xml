      *    Check arrays etc
      *
        05  Test-Record.
            10  p-field-1            pic s999P(4).
            10  p-field-2            pic svppp999.
            
            10 Array-1  occurs 6     pic 99.
            10 Array-2  occurs 5.
               15 field-1            pic x(3).
               15 field-2            pic x(5).
            10 Array-3  occurs 7.
               15 field-3            pic x(4).
               15 field-4            pic x(5).
               15 Array-4  occurs 5.
                  20 field-5         pic x(4).
                  20 field-6         pic x(3).
                  20 field-7         pic x(5).
