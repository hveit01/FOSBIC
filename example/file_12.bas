*5NO-HEADLINE
      REM In file_11.bas we accessed variable record sizes.
      REM While it is simple to write such a file, read access
      REM is slow for a large file as for higher record numbers
      REM one needs to sequentially step forward and skip over the
      REM records which length is not known in advance. Imagine
      REM this would be a slow mag tape which would need to
      REM If one knows the actual first item position of a record
      REM in a file, one can directly skip to the right position.
      REM We therefore store the index to the record in a second key file.
      REM This is the classical index sequential access method.

      DIM D(20)
      
10    OPEN 'F1';2            ! ;1 is key, ;2 is data

20    REM Fill D with data
      MAT READ D
      DATA 1,3,5,7,9,11,13,15,17,19
      DATA 21,23,25,27,29,31,33,35,37,39

30    N = 20

40    REM write N records with a record size of (N+1) -I
      FOR I=1 TO N
        L = (N+1) - I
        GOSUB 1000
      NEXT I

50    REM read records in reverse order
      FOR I=N TO 1 STEP -1
        R = I
        GOSUB 2000
        PRINT 'RECORD';R; 'WITH LENGTH';L
        MAT PRINT D(L)
      NEXT I

      REM simulate an EOF error
      REM R = R9+1
      REM GOSUB 2000

      GOTO 9999

1000  REM write L items in D array, P is current pos, initially 0
      REM Note, we write data sequentially to keep the example simple
      PUT 'F1';2, L           ! write length of record
      FOR I9=1 TO L
        PUT 'F1';2, D(I9)     ! as D is variable, MAT PUT cannot be used
      NEXT I9
      PUT 'F1';1, P+1         ! 1st rec# is 1, 2nd is P + (1+L)
      P = P + (1 + L)
      R9 = R9 + 1             ! last record written, initially R9=0
      RETURN

2000  REM read record R into D array
      IF R<1 GOTO 2999
      IF R>R9 GOTO 2999
      RESET 'F1';1;R          ! position to record R
      GET 'F1';1, R8          ! true position

      RESET 'F1';2;R8         ! position in file
      GET 'F1';2, L           ! get length
      FOR J9=1 TO L
        GET 'F1';2, D(J9)
      NEXT J9
      REM read record into D, length is L
      RETURN
2999  PRINT 'READ FROM INVALID RECORD';R
      STOP

9999  END