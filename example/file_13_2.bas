     REM The case with the COMMON-FILE, part 2
     REM In file_13_1.bas, we wrote data to the common file as well as
     REM to some other file. You should find two files fort.1000 and fort.1002
     REM (and the normal DATA scratch file fort.999).
     REM When declaring the same common-file as in file_13.1, it will be mapped
     REM to fort.1000, but an arbitrary OPENed other file will get the next free
     REM unit number, here it is 1001. But we wrote to fort.1002 before, so
     REM an attempt to read from the former TMP2 file will yield a runtime
     REM error, i.e. naming a file the same won't map it to the same file on disk.

     REM To share data between programs, save them to COMMON-FILEs and
     REM use the same COMMON-FILE declaration in all programs.

10   COMMON-FILE 'COM1'    ! open a shared file as fort.1000
     OPEN 'TMP2'           ! open a tmp file as fort.1001

20   FOR I=1 TO 10
       GET 'COM1', A, B
       PRINT 'READING '; A; B; 'FROM COMMON FILE'
     NEXT I

30   PRINT
     PRINT 'ATTEMPT TO READ FROM A TMP FILE'
     PRINT 'THIS WILL RESULT IN A RUNTIME ERROR, OR WRONG DATA READ'

     REM The following code should fail, unless a remining fort.1001 still
     REM exists.
40   A = -1
     B = -1
     FOR I=1 TO 10
41     GET 'TMP1', A, B
       PRINT 'READING '; A; B; ' FROM TEMP FILE'
     NEXT I


9999 END