*5NO-HEADLINE
      REM In file_7.bas we wrote fixed record data.
      REM Now we create a variable record file, where each record is
      REM tagged with the record length.
      REM We do this with a single index sequential file first.

      DIM D(20)
      
10    OPEN 'F1';1

20    REM Fill D with data
      MAT READ D
      DATA 1,3,5,7,9,11,13,15,17,19
      DATA 21,23,25,27,29,31,33,35,37,39

30    N = 20

40    REM write N records with a record size of 21-N
      FOR I=1 TO N
        L = 21 - I
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

1000  REM write L items in D array
      PUT 'F1', L            ! write length of record
      FOR I9=1 TO L
        PUT 'F1', D(I9)      ! as D is variable, MAT PUT cannot be used
      NEXT I9
      R9 = R9 + 1            ! last record written, initially R9=0
      RETURN

2000  REM read record R into D array
      IF R<1 GOTO 2999
      IF R>R9 GOTO 2999
      REM The problem here is: we cannot position directly
      REM to the record, so we need to sequentially advance.
      RESET 'F1'
      FOR I9=1 TO R
        GET 'F1', L          ! get length
        FOR J9=1 TO L
          GET 'F1', D(J9)
        NEXT J9
      NEXT I9
      REM read record into D, length is L
      RETURN
2999  PRINT 'READ FROM INVALID RECORD';R
      STOP

9999  END