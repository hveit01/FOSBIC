    REM basic sequential file handling
    REM write and read two files

    REM create/open multiple files on disk
    REM first becomes fort.1000, second fort.1001
     OPEN '1'
     OPEN '2'
200 N = 5
    PUT '2', N
    PRINT 'WRITE ';N;' RECORDS TO FILE 2'
    R$ = ' REC=';
    FOR I=1 TO N
    PUT '2', R$,I*I
    PRINT 'WRITING ';R$;I*I
    NEXT I

    REM rewind file an copy content to first
250 RESET '2'
    GET '2', M
    PUT '1', M
    PRINT 'FILE 2 CONTAINS ';M;' RECORDS'
    PRINT 'WRITE COPY TO FILE 1'
    S$ = 'COPY='
    FOR I=1 TO M
      GET '2', A$, B
      PRINT 'READING ';A$; B
      PUT '1', S$,B
      PRINT 'WRITING ';S$; B
    NEXT I
    CLOSE '2'
  
    REM rewind first file and read
300 RESET '1'
    GET '1', K
    PRINT 'FILE 1 CONTAINS ';K;' RECORDS'
    FOR I=1 TO K
      GET '1', C$, D
      PRINT 'READING ';C$; D
    NEXT I
    CLOSE '1'

1000 END