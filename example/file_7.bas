*6
      REM So far we accessed only single items with a GET/PUT
      REM In case we want to access larger fixed-length records, there are two
      REM methods:
      REM 1. we write a single file with e.g. fixed length of L items each record and
      REM    then adjust the record# by multipying with L
      REM 2. if L is smaller then the available limit of sentences, write
      REM    each item to a different sentence.
      REM this example shows the first method. We neglect double buffering as in
      REM file_5.bas and file_6.bas; we just keep track of the last record written (R9)
      REM as we don't have an EOF check.
10    OPEN 'F1';1

20    L = 5                    ! number of items in a set
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
        FOR K=1 TO L
          PRINT D(K),
        NEXT K
        PRINT
      NEXT I

      GOTO 9999

1000  REM write L items in D array as a single record
      FOR I9=1 TO L
        PUT 'F1';1, D(I9)
      NEXT I9
      R9 = R9 + 1            ! last record written, initially R9=0
      RETURN

2000  REM read L items of record# into D array
      IF R<1 GOTO 2999
      IF R>R9 GOTO 2999
      R8 = (R-1)*L+1         ! true record position
      RESET 'F1';1;R8
      FOR I9=1 TO L
        GET 'F1';1, D(I9)
      NEXT I9
      RETURN
2999  PRINT 'READ FROM INVALID RECORD';R
      STOP

9999  END