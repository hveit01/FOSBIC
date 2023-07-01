     REM The case with the COMMON-FILE, part 1.
     REM I was wondering what this COMMON-FILE statement is for
     REM COMMON-FILE must be created before any other OPEN statement.
     REM It can be a sequential, or indexed file, and multiple sentences
     REM and also multiple files can be built. The point is that the
     REM file slots are pre-allocated, so their names far fixed to
     REM fort.1000...fort.xxxx. With OPEN/CLOSE within the program
     REM the allocated unit number is not fixed.

     REM In this test, remove the fort.* files in advance!
     REM We write to the common file and to the second TMP2
     REM file. The TMP1 file is a placeholder for some other operation.
     REM Writing to some other file than the common file is unreliable.
     REM In the follwoing example file_13_2.bas we try to read
     REM the data back.

10   COMMON-FILE 'COM1'   ! open a shared file as fort.1000
     OPEN 'TMP1'          ! open a temporary file as fort.1001
     OPEN 'TMP2'          ! open another temp file as fort.1002

20   FOR I=1 TO 10
       PRINT 'WRITE ';I;I*I;'TO COMMON FILE'
       PUT 'COM1', I, I*I
     NEXT I

30   FOR I=1 TO 10
       PRINT 'WRITE ';I;I*I;'TO TEMP2 FILE'       
       PUT 'TMP2', I, I*I
     NEXT I


9999 END