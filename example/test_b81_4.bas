10 REM DIVISION
20 REM MATRIX = MATRIX / (ARITHMETISCHER AUSDRUCK)
22 REM                   ODER
25 REM MATRIX = (ARITHMETISCHER AUSDRUCK) / MATRIX
30 DIM A(5), B(5), C(5)
40 MAT READ A
50 MAT LET B = A/(SIN(0.7854))
55 MAT LET C = (10)/A
60 MAT PRINT A, B, C
70 DATA 1,2,3,4,5
80 END
