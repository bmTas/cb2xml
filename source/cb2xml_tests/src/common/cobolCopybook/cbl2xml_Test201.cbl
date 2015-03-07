      *    Test various unusual situations
      *
            10 TOP-LEVEL-ITEM.
               15 date-field           pic 9(8).
               15      redefines date-field.
                  20 YYYY              pic 9(4).
                  20 MM                pic 99.
                  20 DD                pic 99.
               15 table-data           pic x(10)
                  value 'a1s2d3f4g5'.
                  
               15     redefines table-data.
                  20 rcode occurs 5    pic xx.
               15 0-field              pic xxx.
               15 10-field             pic xx
               15 1a-field             pic 99
               15 10a-field            pic 999.

