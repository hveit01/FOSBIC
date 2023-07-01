  5 REM This is a derived test
 10 LET I = 1
 20 LET G = 100
 30 LET S = 2
 40 IF I GT G GOTO 80
 50 LET S1 = S1 + I
 60 LET I = I + S
 70 GOTO 40
 80 PRINT S1
 90 END