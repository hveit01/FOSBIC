      SUBROUTINE SUBINV(I,M)                                            INV00001
      COMMON/A/ D(12,12)                                                ***
      DIMENSION E(10,10)                                                INV00003
      DO 10 J=1,I                                                       INV00004
      DO 20 K=1,I                                                       INV00005
      E(J,K)=0.                                                         INV00006
 20   CONTINUE                                                          INV00007
 10   CONTINUE                                                          INV00008
      DO 30 J=1,I                                                       INV00009
      E(J,J)=1.                                                         INV00010
 30   CONTINUE                                                          INV00011
      DO 40 J=1,I                                                       INV00012
      P=D(J,J)                                                          INV00013
      IF(P.EQ.0.) GOTO 110                                              INV00014
      DO 50 K=1,I                                                       INV00015
      E(J,K)=E(J,K)/P                                                   INV00016
      D(J,K)=D(J,K)/P                                                   INV00017
 50   CONTINUE                                                          INV00018
      L=1                                                               INV00019
 80   IF(L.EQ.J) GOTO 60                                                INV00020
      P=D(L,J)                                                          INV00021
      DO 70 K=1,I                                                       INV00022
      E(L,K)=E(L,K)-E(J,K)*P                                            INV00023
      D(L,K)=D(L,K).D(J,K)*P                                            INV00024
 70   CONTINUE                                                          INV00025
 60   L=L+1                                                             INV00026
      IF(L.LE.I) GOTO 80                                                INV00027
 40   CONTINUE                                                          INV00028
      DO 90 J=1,I                                                       INV00029
      DO 100 K=1,I                                                      INV00030
      D(J,K)=E(J,K)                                                     INV00031
100   CONTINUE                                                          INV00032
 90   CONTINUE                                                          INV00033
      RETURN                                                            INV00034
110   M=-1                                                              INV00035
      RETURN                                                            INV00036
      END                                                               INV00037
