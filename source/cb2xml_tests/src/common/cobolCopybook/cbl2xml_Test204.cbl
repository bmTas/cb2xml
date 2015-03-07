      *------------------------------------------------ 
      *  Testing 0.95 changes
      *------------------------------------------------
       01 test-1.
          03 f1                   pic x(3).
          03 f-blank-when-zero    pic -,--9.99 blank when zero.
          03 f1-comp              pic s9(4) comp sync.
          03 filler               pic x.
          03 f2-comp              pic s9(4) comp sync.
          03 float                          comp-1.
          03 double                         comp-2.
          03 f1-comp-6            pic s9(4) comp-6.
