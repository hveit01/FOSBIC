      REM A naive sort with files
      REM Nowadays, one just sorts data in a large enough in-memory
      REM array; in former times, it had to be done in files or on tapes.
      REM Donald Knuth's TAOCP, Vol 3 describes various algorithms for
      REM sorting in this situation which are suitable and unsuitable
      REM when handling tape sorting. Remember a tape is sequential
      REM and requires significant time to rewind, so we want to
      REM avoid that if possible.

      REM We use an unsuitable sort with simulated tapes now.
      REM note we could save a bit by allowing to overwrite the
      REM IN tape and use this as a TMP, but it won't help much.

10    OPEN 'IN'           ! input data in 1
      OPEN 'TMP';2        ! temporary buffers
      OPEN 'OUT'          ! result data

20    R0 = 0              ! counter for tape rewinds
21    N = 450             ! number of elements to sort

30    REM create a data set to be sorted
      GOSUB 2000

40    L = 999999          ! currently smallest element
      R = 1               ! TMP buffer to read
      W = 2               ! TMP buffer to write
      PRINT 'COPY INPUT TAPE TO SORT TAPE';R
      FOR I=1 TO N        ! we make a copy into TMP;R
        GET 'IN', D       ! because we don't want to destroy IN tape
        PUT 'TMP';R, D
      NEXT I
      RESET 'TMP'
      R0=R0+1

50    PRINT
      PRINT 'START SORTING'
      FOR I=1 TO N
REM        PRINT 'READ FROM TAPE';R;'I=';I
        GET 'TMP';R, L    ! assume first is smallest
        FOR J=I+1 TO N    ! find smallest element among remaining
          GET 'TMP';R, C  ! current element
          IF C < L GOTO 60! L is not smallest
          PUT 'TMP';W, C  ! C is not smallest, store it
          GOTO 70
60        PUT 'TMP';W, L  ! save L
          L = C           ! make C new smallest        
70      NEXT J
        PUT 'OUT', L      ! save smallest in output
        REM now OUT has I items, W has N-1 items, R will be overwritten
        RESET 'TMP'       ! rewind buffers
        R0=R0+2           
        X=R               ! swap buffers
        R=W
        W=X
79    NEXT I
      PRINT 'END SORTING, RESULT IN OUTPUT'
      PRINT

80    RESET 'OUT'
      FOR I=1 TO N        ! print results
        GET 'OUT', D
        PRINT D;
      NEXT I
      PRINT
      PRINT
      PRINT 'WE NEEDED';R0;'REWINDS OF TAPES FOR';N;'SORTS'

      GOTO 9999

2000 REM create input data
     PRINT 'CREATE DATA SET'
     FOR I=1 TO N
       D = INT(RND(1)*N/2+1)    ! N/2 enforces presence of duplicates
       PUT 'IN', D
       PRINT D;
     NEXT I
     PRINT
     RESET 'IN'
     RETURN

9999  END