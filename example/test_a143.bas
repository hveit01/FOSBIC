10  DIM A(3,4),B(3,4),C(3,4)
15  PRINT '        I','        J','     A(I,J)','     B(I,J)','     C(I,J)'
20  FOR I=1 TO 3
30    FOR J=1 TO 3
40      READ A(I,J),B(I,J)
50      LET C(I,J)=A(I,J)+B(I,J)
60      PRINT I,J,A(I,J),B(I,J),C(I,J)
70    NEXT J
80  NEXT I
90  DATA 1,12,2,11,3,10,4,9,5,8,6,7
100 DATA 7,6,8,5,9,4,10,3,11,2,12,1
110 END