10  REM VEREINFACHTE MATRIZENMULTIPLIKATION
40  DIM A(3,4),B(3,7),C(7,4)
50  READ N,M,O
60  MAT LET A=ZER
70  MAT READ B,C
80  MAT LET A=B*C
150 MAT PRINT A,B,C
160 DATA 3,4,7
170 DATA 1,2,3,4,5,6,7,8,9,10
171 DATA 11,12,13,14,15,16,17,18,19,20
172 DATA 21
180 DATA 1,2,3,4,5,6,7,8,9,10
181 DATA 11,12,13,14,15,16,17,18,19,20
182 DATA 21,22,23,24,25,26,27,28
190 END