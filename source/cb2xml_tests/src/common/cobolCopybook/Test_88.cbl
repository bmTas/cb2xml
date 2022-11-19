
       01 record-88.
           03 WS-GENDER-FLAG PIC X(01).
              88 MALE VALUE 'M'.
              88 FEMALE VALUE 'F'.
           03 WS-CONTINT-FLAG PIC X(02) VALUE SPACES.
              88 WS-ASIA          VALUES 'IN' 'PK' 'NP' 'BD' 'CH'.
              88 WS-AMERICA       VALUES 'US' 'CN' 'MX' 'BR'.
           03 WS-NUMBER-CHECK PIC 9(02).
              88 FIRST-10    VALUE 1 THRU 10.
              88 SCND-10     VALUE 11 THRU 20.
              88 THRD-10     VALUE 21 THRU 30.
           03  student-status    pic 9(2).   
               88  kindergarten  value 0.   
               88  elementary    values are 1 through 6.   
               88  jr-high       values 7, 8, 9.   
               88  high-school   values are 10 through 12.   
           03 age                  PIC 9(3).
              88 teenager  VALUE 0  THRU 17.
              88 adult     VALUE 18 THRU 60.
              88 retired   VALUE 61 THRU 150.