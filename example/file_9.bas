*6NO-HEADLINE
      REM In file_7.bas we wrote a fixed record with an array D with putting
      REM each array element in a loop.
      REM But FOSBIC also has MAT PUT/MAT GET commands.
      REM Attention: you can only use complete arrays, not sub ranges
      
      DIM D(5)
      
10    OPEN 'F1';1

20    L = 5                    ! number of items in a set, must be the same as for OPEN and DIM
      N = 20

30    REM write N records with I, 2*I, 3*I, I*I, I**3
      FOR I=1 TO N
        D(1) = I
        D(2) = 2*I
        D(3) = 3*I
        D(4) = I*I
        D(5) = I**3
        GOSUB 1000
      NEXT I

40    REM read records in reverse order
      PRINT '        I','      2*I','      3*I','      I*I','     I**3'
      FOR I=N TO 1 STEP -1
        R = I
        GOSUB 2000
        MAT PRINT D
      NEXT I

      REM simulate an EOF error
      REM R = R9+1
      REM GOSUB 2000

      GOTO 9999

1000  REM write L items in D array
      MAT PUT 'F1';1, D
      R9 = R9 + 1            ! last record written, initially R9=0
      RETURN

2000  REM read L items of record# into D array
      IF R<1 GOTO 2999
      IF R>R9 GOTO 2999
      R8 = (R-1)*L+1         ! true record position
      FOR I9=1 TO L
        RESET 'F1';1;R8
        MAT GET 'F1';1, D
      NEXT I9
      RETURN
2999  PRINT 'READ FROM INVALID RECORD';R
      STOP

9999  END