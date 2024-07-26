      *    VARIOUS PICTURE CHARACTER STRINGS
      *
            10 TOP-LEVEL-ITEM.

                20 PIC-TEST-1 PIC X.
                20 PIC-TEST-2 PIC XX.
                20 PIC-TEST-3 PIC X(3).
                20 PIC-TEST-4 PIC S9.
                20 PIC-TEST-5 PIC S9.9.
                20 PIC-TEST-6 PIC S9V99.
                20 PIC-TEST-7 PIC $ZZZZ9999.
      *
      *    -----------------------------------------------------------
      *    DOM PARSER SHOULD INSERT SPACES IN THESE DASHED LINES
      *    TO AVOID CLASHING WITH THE XML COMMENT TAG / DELIMITER
      *    -----------------------------------------------------------
      *    DIFFERENT VALUE CLAUSES AND CONSTANTS

                20 VALUE-TEST-GROUP.
                    30 VALUE-TEST-1 PIC XXX VALUE SPACES.
                    30 VALUE-TEST-2 PIC 999 VALUE ZEROS.
                    30 VALUE-TEST-3 PIC XXX VALUE NULLS.
                    30 VALUE-TEST-4 PIC 999 VALUE 300.
                    30 VALUE-TEST-5 PIC XXXXX VALUE \"HELLO\".

      *    THE RARELY USED \"DEPENDING ON\" OCCURS SYNTAX

            20 DEPENDING-ON-TEST-GROUP.

                30 DEPENDING-TEST-1 PIC 9.
                30 DEPENDING-TEST-2 PIC X OCCURS 1 TO 9 TIMES
                    DEPENDING ON DEPENDING-TEST-1.
      *    88 LEVELS CAN APPEAR FOR GROUP DECLARATIONS

                20 GROUP-WITH-88-LEVEL.
                    88 GROUP-WITH-88-LEVEL-CONDITION-1 VALUE \"HELLO\".
                    88 GROUP-WITH-88-LEVEL-CONDITION-2 VALUE \"WORLD\".
                        30 GROUP-WITH-88-LEVEL-ITEM-1 PIC XX.
                        30 GROUP-WITH-88-LEVEL-ITEM-2 PIC XXX.

      *    COBOL KEYWORDS CAN BE LOWERCASE -
      *    AND CB2XML IS CASE INSENSITIVE.
      *    88 VALUES ARE CAPTURED CORRECTLY -
      *    AND NOT BULK CONVERTED TO UPPERCASE.

                20 GROUP-WITH-LOWERCASE.
                    30 LOWERCASE-KEYWORDS pic 9 value null.
                    30 lowercase-identifier PIC X.
                    30 MiXeDcAsE-TEST PiC XXX VaLuE SpAcEs.
                    30 LOWERCASE-88-VALUE PIC X(5).

                        88 LOWER-AND-UPPER-CASE-88-VALUES
                            VALUES 'HELLO', 'hello'.

      *    THE \"/\" CHARACTER IN THE FIRST COLUMN SIGNIFIES A COMMENT

                20 INDICATOR-COLUMN-SLASH-TEST-GROUP.
                    20 UNCOMMENTED-1 PIC X.
      /             20 INDICATOR-SLASH-COMMENTED PIC X.
                    20 UNCOMMENTED-2 PIC X.

      *    THERE CAN BE MULTIPLE RECORD LAYOUTS
      *    DEFINED WITHIN A SINGLE COPYBOOK

            10 ANOTHER-TOP-LEVEL-ITEM.

                20 PIC-TEST-99 PIC X.
                20 PIC-TEST-98 PIC 9.
  
           10 signed-comp-tests.
              20 comp-l1      pic s9    comp.
              20 comp-l2      pic s99   comp.
              20 comp-l3      pic s999  comp.
              20 comp-l4      pic s9(4) comp.
              20 comp-l5      pic s9(5) comp.
              20 comp-l6      pic s9(6) comp.

           10 unsigned-comp-tests.
              20 u-comp-l1      pic 9    comp.
              20 u-comp-l2      pic 99   comp.
              20 u-comp-l3      pic 999  comp.
              20 u-comp-l4      pic 9(4) comp.
              20 u-comp-l5      pic 9(5) comp.
              20 u-comp-l6      pic 9(6) comp.

            10 signed-comp-3-tests.
              20 comp-3-l1      pic s9    comp-3.
              20 comp-3-l2      pic s99   comp-3.
              20 comp-3-l3      pic s999  comp-3.
              20 comp-3-l4      pic s9(4) comp-3.
              20 comp-3-l5      pic s9(5) comp-3.
              20 comp-3-l6      pic s9(6) comp-3.

           10 unsigned-comp-3-tests.
              20 u-comp-3-l1      pic 9    comp-3.
              20 u-comp-3-l2      pic 99   comp-3.
              20 u-comp-3-l3      pic 999  comp-3.
              20 u-comp-3-l4      pic 9(4) comp-3.
              20 u-comp-3-l5      pic 9(5) comp-3.
              20 u-comp-3-l6      pic 9(6) comp-3.

           10 signed-comp-5-tests.
              20 comp-5-l1      pic s9    comp-5.
              20 comp-5-l2      pic s99   comp-5.
              20 comp-5-l3      pic s999  comp-5.
              20 comp-5-l4      pic s9(4) comp-5.
              20 comp-5-l5      pic s9(5) comp-5.
              20 comp-5-l6      pic s9(6) comp-5.

           10 unsigned-comp-5-tests.
              20 u-comp-5-l1      pic 9    comp-5.
              20 u-comp-5-l2      pic 99   comp-5.
              20 u-comp-5-l3      pic 999  comp-5.
              20 u-comp-5-l4      pic 9(4) comp-5.
              20 u-comp-5-l5      pic 9(5) comp-5.
              20 u-comp-5-l6      pic 9(6) comp-5.


           10 signed-comp-tests.
              20 comp-l1v0      pic s9      comp.
              20 comp-l1v1      pic s9v9    comp.
              20 comp-l1v2      pic s9v99   comp.
              20 comp-l1v3      pic s9v999  comp.
              20 comp-l1v4      pic s9v9(4) comp.
              20 comp-l1v5      pic s9v9(5) comp.
              20 comp-l1v6      pic s9v9(6) comp.

      *    THIS ADDITIONAL RECORD LAYOUT DEFINITION
      *    STARTS AT LEVEL 05 INSTEAD OF 10 AS EXPECTED

            05 YET-ANOTHER-TOP-LEVEL-ITEM.

                06 PIC-TEST-97 PIC X.
                06 PIC-TEST-96 PIC 9.

