* Note this example gives a runtime error despite of line 50
10 READ S
20 DATA 1, 2, 0.0
30 FOR I = 1 TO 10 STEP S
40   PRINT I
50   IF S EQ 0 GOTO 80
60 NEXT I
70 GOTO 10
80 PRINT 'SCHRITTWEITE GLEICH NULL',S
90 END

