       * CT750MSG is a new program checked out by V000072
        01  FILLER   PIC X(25)  VALUE 'PROFILE ERROR HELP TABLES'.
        01  CT750MSG.
       ***********************************************************
       *    T A B L E S    A R E    D A T E    S P E C I F I C   *
       ***********************************************************

            03 DATE-COMP-1     PIC X(6)  VALUE '911   '.

            03 DATE-1-LITERALS.
              05 DATE-1-GRP-I1-LITERALS.
              
                07 GRP-I1-A        PIC X(78) VALUE
                'CURRENT YEAR PRESET IS ''FR''
       -         '                      '.
                07 GRP-I1-B        PIC X(78) VALUE
                'CURRENT YEAR HAS ONE OR MORE MATH ERRORS
       -         '                      '.

