      ***************************************
      *  basic N/G picture clause checking
      ***************************************

       01 Rec.
          03 Group1.   
             04 Group2.           
                05  fld1             pic x.
                05  fld2             pic n.
                05                   pic x.
                05  fld3             pic n(3).
              04 Group3.           
                05  fld11            pic xxxx.
              04 Group4.           
                05                   pic x.
                05  fld4             pic g.
                05                   pic x.
                05  fld5             pic g(3).     
                05  fld6             pic x.
              04 Group5.           
                05  fld12            pic xxxx.
          03 fld7                 pic xxx.