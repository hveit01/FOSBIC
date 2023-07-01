     REM We could see a significant delay in file_14_1.base for the sorting
     REM and a large number of tape rewinds. With a real tape drive, this
     REM could require minutes each time, so this should be reduced to a minimum.

     REM Knuths proposal on external sorting (5.4) is 'balanced two-way merge'
     REM which reads portions into memory array S0, sort them, and write them
     REM alternating into tapes 1 and 2. After that merge the two tapes alternating
     REM into tapes 3 and 4.  Merging means to take items from both tapes 1 and 2 
     REM and write them in sort order to a new tape

     REM For simplicity we assume that the number of elements N and
     REM the size S of the in-memory array S0 are divisible without remainder
     REM otherwise it will less understandable, because we need to keep score
     REM of the actual lengths of files.
     REM BASIC functions to tell the file size or report EOF would be a nice extension.

     OPEN 'IN'                ! input tape
     OPEN 'TMP';4             ! temporary tapes
     OPEN 'OUT'               ! output tape

20   R0 = 0                   ! counter for tape rewinds
21   N = 450                  ! number of elements to sort
22   S0 = 50                  ! sortable block size
     DIM S(50)                ! array for sorting in memory
     DIM C(4)                 ! # of blocks in TMP tapes

25   B = N/S0                 ! number of blocks
     IF INT(N-INT(B)*S0) <> 0 GOTO 9998 ! N is not a multiple of S0

30   REM create a data set to be sorted
     GOSUB 2000

40   PRINT
     PRINT 'START SORTING'
     T = 1                    ! TMP tape pair T1,T2
     U = 3                    ! TMP tape pair T3,T4 

50   REM PRINT 'SPLIT INPUT TAPE INTO TAPES';T;T+1
     FOR I=1 TO B
       MAT GET 'IN', S        ! read a sortable chunk from IN     
       GOSUB 1000             ! sort S
       MAT PUT 'TMP';T, S     ! store into current TMP tape
       C(T) = C(T) + S0       ! so many items are in tapes T1 or T2 resp.
       T = (1+2)-T            ! select other tape from 1,2
     NEXT I

     REM now we have partly sorted stuff in blocks of B in tape 1 and 2
     REM looks like:
     REM T1 = B1 B3 B5 B7
     REM T2 = B2 B4 B6

     REM PRINT 'BLOCKS IN TAPE 1:';INT(C(1)/S0)+1
     REM PRINT 'BLOCKS IN TAPE 2:';INT(C(2)/S0)+1

     T=1                      ! enforce that T points to set T1,T2
100  RESET 'TMP';T            ! rewind files
     RESET 'TMP';T+1
     R0 = R0 + 2

200  REM S0 is current maximum merge size
     REM Merge T=(T1,T2) TO U=(T3,T4) in groups of S0
     
     REM NOTE the size of T1 is always larger or equal to the size of T2
     REM therefore we can read a maximum of T2 blocks

205  IF C(T+1)=0 THEN 300     ! second TMP file is empty?
     M = INT(C(T+1)/S0)+1     ! number of full blocks
     FOR I=1 TO M

       B1 = S0                ! B1 = minimum(S0, C(T))
       IF B1<C(T) THEN 210    ! remaining items in block
       B1 = C(T)
210    B2 = S0                ! B2 = minimum(S0, C(T+1))
       IF B2<C(T+1) THEN 220  ! remaining items in block
       B2 = C(T+1)

220    GOSUB 3000             ! merge block into U
       GOSUB 8000             ! swap target files U
     NEXT I                   ! for all blocks

     GOSUB 7000               ! enforce last block from 1 to 3 or 3 to 1

240  IF C(T)<=0 THEN 250      ! remaining stuff in T1?
     FOR I=C(T) TO 1 STEP -1  ! append it into T3
       GET 'TMP';T, A
       PUT 'TMP';U, A
     NEXT I

250  REM SWAP files 1,2 with 3,4
     REM T and U are known to point to either 1 or 3, from subroutine 7000
     X=T
     T=U
     U=X

     RESET 'TMP'              ! rewind all 4 files
     R0 = R0 + 4

     S0 = S0*2                ! double block size
     GOTO 205

300  PRINT 'SORTING DONE'

     RESET 'TMP';1
     R0 = R0 + 1

     PRINT 'COPY RESULT TO OUTPUT TAPE'
     FOR I=1 TO N             ! print results
       GET 'TMP';1, D
       PUT 'OUT', D
       PRINT D;
     NEXT I
     PRINT

     PRINT
     PRINT 'WE NEEDED';R0;'REWINDS OF TAPES FOR';N;'SORTS'

     GOTO 9999

!********************************************************************
! sort a block in memory
!********************************************************************
1000 REM sort the array S0 with B elements
     REM any sorting algorithm will do; for simplicity bubble sort is used
     FOR I9=1 TO S0
       FOR K9=1 TO S0-I9
         IF S(K9) <= S(K9+1) GOTO 1010
         X = S(K9)             ! swap elements
         S(K9) = S(K9+1)
         S(K9+1) = X
1010   NEXT K9
     NEXT I9
     RETURN

!********************************************************************
! create random input data
!********************************************************************
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

!********************************************************************
! merge T pair into active file U
!********************************************************************
3000 REM  Merge B1 items in T with B2 items in T+1 into B1+B2 into U
     REM PRINT 'MERGE';B1;'ITEMS IN';T;'AND';B2;'ITEMS IN';T+1;'INTO';U
     I1=1                    ! counts items in block from T
     J1=1                    ! counts items in block from T+1
     K1=1                    ! counts items written to U
     V1=1                    ! marker: need new A1 from T
     V2=1                    ! marker: need new A2 from T+1

3010 IF I1>B1 THEN 3100      ! at least one block is empty
     IF J1>B2 THEN 3100
     GOSUB 4000              ! get next A1 from T
     GOSUB 5000              ! get next A2 from T+1
3020 IF A1>=A2 THEN 3030
       A = A1
       GOSUB 6000            ! save A1
       V1 = 1                ! need new A1
       I1 = I1+1
     GOTO 3010
3030   A = A2
      GOSUB 6000             ! save A2
       V2 = 1
       J1 = J1+1
     GOTO 3010

3100 FOR I2=I1 TO B1
       GOSUB 4000            ! get next A1 from T
       A = A1
       GOSUB 6000            ! save A1
       V1 = 1                ! need new A1
     NEXT I2

3200 FOR J2=J1 TO B2
       GOSUB 5000            ! get next A2 from T+1
       A = A2
       GOSUB 6000            ! save A2
       V2 = 1                ! need new A2
     NEXT J2

3300 C(T) = C(T) - B1        ! correct the item counts
     C(T+1) = C(T+1) - B2
     C(U) = C(U) + B1 + B2
     RETURN

!********************************************************************
! get an item from file T when needed (V=1)
!********************************************************************
4000 IF V1=0 THEN 4999
     GET 'TMP';T, A1
     V1=0
4999 RETURN

!********************************************************************
! get an item from file T+1 when needed (V=1)
!********************************************************************
5000 IF V2=0 THEN 5999
     GET 'TMP';T+1, A2
     V2=0
5999 RETURN

!********************************************************************
! write item A to file U, and increment write counter K
!********************************************************************
6000 REM write A to file U
     PUT 'TMP';U, A
     K1 = K1 + 1
     RETURN

!********************************************************************
! enforce that last block is read from first file of source pair,
! and written to first file of target pair
!********************************************************************
7000 IF T>2 THEN 7100         ! T=(3,4) is currently target?
     U=3                      ! no, read from 1 to 3
     T=1
     RETURN
7100 U=1                      ! yes, read from 3 to 1
     T=3
     RETURN

!********************************************************************
! swap target files (X,X+1) <-> (X+1,X)
!********************************************************************
8000 REM swap target files U
     IF U>2 THEN 8100       ! U=(3,4)?
     U = (1+2) - U          ! no, make U=(2,1)
     RETURN
8100 U = (3+4) - U          ! make U=(4,3)
     RETURN


9998 PRINT 'DATASET SIZE';N;'IS NOT A MULTIPLE OF SORTSET SIZE';S0
9999 END