     REM index sequential WRITE access version 2
     REM You will have noticed from file_5.bas
     REM that writing is rather slow, because the whole file
     REM needs to be copied each time.
     REM this could be optimized by keeping data in a cache array
     REM but the file handling is actually there to avoid this.
     REM Instead of positioning to each record, position to 1st
     REM record and then copy sequentially. A dummy GET at the position R
     REM is required to keep the files in sync.

10   OPEN 'F';2
11   RESET 'F'

20   N = 100        ! total number of recs in file

30   REM write N records
     N9 = N
     GOSUB 1000     ! initialize N9 records

40   REM change records
     FOR I = 1 TO 20
       R = INT(RND(1)*N+1)   ! random record#
       D = R                 ! data to be written, here same as record#
       GOSUB 3000            ! writerecord(R,D)
       REM read it back, record in R, data into D
       GOSUB 2000            ! D = readrecord(R)
       PRINT 'WRITE R=';R,'D=';D
     NEXT I
     PRINT

50   REM read records in reverse order
     FOR I=N TO 1 STEP -1
       R = I
       GOSUB 2000            ! D = readrecord(R)
       PRINT 'READ R=';R,'D=';D
     NEXT I

60   GOTO 9999

1000 REM initialize file with N9 records
     REM A9 = active buffer is 0 initially
     FOR I1=1 TO N9
       PUT 'F';A9+1, 0
     NEXT I
     RETURN

2000 REM read R from active buffer
     IF R<1 GOTO 2999
     IF R>N9 GOTO 2999
     RESET 'F';A9+1;R
     GET 'F';A9+1, D
     RETURN
2999 PRINT 'READ FROM INVALID RECORD';R
     STOP

3000 REM write D to record R
     IF R<1 GOTO 3999
     IF R>N9 GOTO 3999
     Z9 = 1-A9                  ! other buffer
     RESET 'F'                  ! reset all sentences at once
                                ! and now read sequentially
     FOR I1=1 TO R-1            ! copy up to R
       GET 'F';A9+1, D9
       PUT 'F';Z9+1, D9
     NEXT I1
     GET 'F';A9+1, D9           ! dummy read to position
     PUT 'F';Z9+1, D            ! write D
     FOR I1=R+1 TO N9           ! copy from R to N9
       GET 'F';A9+1, D9
       PUT 'F';Z9+1, D9
     NEXT I1
     A9 = Z9                    ! swap buffers
     RETURN
3999 PRINT 'WRITE TO INVALID RECORD';R
     STOP

9999 END