000100******************************************************************
000200*                                                                *
000300*   DTAR119 - KEYCODE / BONUS FILE                               *
000400*                                                                *
000500*   FORMAT  -  FB   54 CHARS.                                    *
000600*                                                                *
000700*  VER| DATE     | DESCRIPTION                  | AUTHOR         *
000800*  ---|----------|------------------------------|------------    *
000900*  01 | JUN 94   | FIRST ISSUE                  | R. GEALL       *
001000*                                                                *
001100******************************************************************
001200                                                                  
001300     03  DTAR119-STORE-NO               PIC 9(03)     COMP-3.     
001400     03  DTAR119-TRANS-DATE             PIC 9(07)     COMP-3.     
001500     03  DTAR119-TRANS-CODE             PIC 9(02)     COMP-3.     
001600     03  DTAR119-CARD-NO                PIC 9(16).                
001700     03  DTAR119-TERMINAL-ID            PIC 9(03).                
001800     03  DTAR119-TRANS-NO               PIC 9(04).                
001900     03  DTAR119-BONUS-POINTS           PIC S9(06)    COMP-3.     
002000     03  DTAR119-KEYCODE                PIC 9(08)     COMP-3.     
002100     03  DTAR119-KEYCODE-AMT            PIC S9(07)V99 COMP-3. 
002110     03  DTAR119-KEYCODE-QTY            PIC S9(07)V99 COMP-3. 
002200     03  DTAR119-PROM-NO                PIC 9(05)     COMP-3. 

