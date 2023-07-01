    REM basic index sequential READ access

    REM create/open an index sequential file
    REM becomes fort.1000
    REM ;1 means a single file to simulate indexed access
    OPEN '1';1

200 N = 100
    PRINT 'WRITE';N;' RECORDS TO FILE'
    PRINT 'FILL RECORD K WITH VALUE 2*K'
    FOR I=1 TO N
      V = 2*I     ! value
      PUT '1', V
    NEXT I

    REM now read 10 random values from file
    PRINT 'READ 10 RANDOM RECORDS BY KEY'
300 FOR I=1 TO 10
      K = INT(RND(1)*N+1)
      RESET '1';1;K                ! pointer to record K
      GET '1';1, A                 ! get data from record K
      PRINT 'RECORD ';K;' IS ';A
    NEXT I

    CLOSE '1'

1000 END