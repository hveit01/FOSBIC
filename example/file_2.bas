    REM basic sequential file handling
    REM write and read a single file
    REM dont close file but use RESET

    REM create/open a file on disk
    REM they are named fort.1000...fort.1024
    REM opening the first file will use fort.1000
    REM the string 'F1' is just syntactic sugar
200 OPEN 'F1'

    REM write number of records
    N = 20
    PUT 'F1', N

    REM WRITE THE RECORDS
    FOR I=1 TO N
      PUT 'F1', ' REC=',I*I
    NEXT I

    REM RESET file back to begining
    RESET 'F1'

    REM obtain number of records
    GET 'F1', N

    REM read the records
    FOR I=1 TO N
      GET 'F1', A$, B
      PRINT A$; I, B
    NEXT I

    REM close the file again
    CLOSE 'F1'

1000 END