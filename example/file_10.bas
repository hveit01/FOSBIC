*8
      REM More on the MAT stuff:
      REM MAT PUT does not store the size of the array, so you can read more
      REM or less from the file than you have written. The former is an error,
      REM the latter will likely result in a mess. So take care what you deal with.
      REM in case of 2-dim arrays, the structure is not stored either.
      REM so you can write A(4,5) array and read B(5,4) back.
      
      DIM W(6), R(5)
      
10    OPEN 'F1'            ! sequential read

20    REM fill write array W with data
      MAT READ W
      DATA 1,2,3,4,5,6

30    REM write array
      MAT PUT 'F1', W
      MAT PRINT W

40    REM read it back
      RESET 'F1'
      MAT GET 'F1', R
      MAT PRINT R

      REM read excess data
      GET 'F1', E
      PRINT 'EXCESS DATA'; E

      REM trying to read more will result in EOF
      REM GET 'F1', X

      REM simulate array shift
      PRINT
      RESET 'F1'
      GET 'F1', E           ! first item
      MAT GET 'F1', R
      PRINT 'FIRST ITEM'; E
      MAT PRINT R

9999  END