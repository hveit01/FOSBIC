      SUBROUTINE ZNUMB(IANF,IEND,NUM,ISGN,IFIR)                         ZNU00001
C**** SUBROUTINE TO TRANSFORM NUM(INTEGER) INTO DIGITS                  ZNU00002
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      ZNU00003
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST,INEXT, ZNU00004
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  ZNU00005
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, ZNU00006
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     ZNU00007
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     ZNU00008
     1NIFMAX,INTZEI                                                     ZNU00009
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        ZNU00010
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      ZNU00011
     1IRET(20),XXX(4),NFILE(25,3)                                       ZNU00012
      COMMON// ISTLST(340),LISTST(340)                                  ZNU00013
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               ZNU00015
      DIMENSION IPROG(3700)                                             ZNU00016
      EQUIVALENCE (DATA(1),IPROG(1))                                    ZNU00017
      DO 10 I=1,80                                                      ZNU00018
 10   CARDT(I)=BLANK                                                    ZNU00019
      NUMZ=IEND-IANF+1                                                  ZNU00020
      MEM=NUMZ                                                          ZNU00021
      IF(ISGN.EQ.2) GOTO 200                                            ZNU00022
      I=1                                                               ZNU00023
      IF(NUM.EQ.0) GOTO 500                                             ZNU00024
      IF(ISGN.EQ.0) GOTO 35                                             ZNU00025
 336  NUMZ=NUMZ-1                                                       ZNU00026
      I=I+1                                                             ZNU00027
 35   IF(NUMZ.LE.INTNUM) GOTO 355                                       ZNU00028
      GOTO 336                                                          ZNU00029
 355  IF(NUM.GE.10**NUMZ) GOTO 200                                      ZNU00030
 40   NUMZ=NUMZ-1                                                       ZNU00031
      IX=NUM/10**NUMZ                                                   ZNU00032
      IF(IX.NE.0) GOTO 20                                               ZNU00033
      IF(IFIR.GE.1) GOTO 20                                             ZNU00034
      CARDT(I)=BLANK                                                    ZNU00035
      GOTO 30                                                           ZNU00036
 20   IFIR=IFIR+1                                                       ZNU00037
      IF(IFIR.GT.1) GOTO 25                                             ZNU00038
      IF(ISGN.EQ.1) CARDT(I-1)=CMINUS                                   ZNU00039
 25   CARDT(I)=DIGIT(IX+1)                                              ZNU00040
      NUM=NUM-IX*10**NUMZ                                               ZNU00041
      IF(NUMZ.EQ.0) GOTO 400                                            ZNU00042
 30   I=I+1                                                             ZNU00043
      GOTO 40                                                           ZNU00044
C**** SPACE IS TOO SMALL TO INSERT DIGITS                               ZNU00045
 200  ZEICH=ASTRSK                                                      ZNU00046
 205  IF(ISGN.EQ.1) NUMZ=NUMZ+1                                         ZNU00047
      DO 210 I=1,NUMZ                                                   ZNU00048
 210  CARDT(I)=ZEICH                                                    ZNU00049
      GOTO 400                                                          ZNU00050
C**** ONLY NEGATIVE SIGN                                                ZNU00051
 300  IF(IFIR.EQ.0) CARDT(NUMZ)=CMINUS                                  ZNU00052
      GOTO 400                                                          ZNU00053
C**** NUM IS ZERO                                                       ZNU00054
 500  CARDT(NUMZ)=DIGIT(1)                                              ZNU00055
      IF(ISGN.EQ.1) GOTO 300                                            ZNU00056
      IF(IFIR.EQ.0) GOTO 400                                            ZNU00057
      ZEICH=DIGIT(1)                                                    ZNU00058
      GOTO 205                                                          ZNU00059
 400  DO 410 I=1,MEM                                                    ZNU00060
      CARP(NPRUS)=CARDT(I)                                              ZNU00061
      NPRUS=NPRUS+1                                                     ZNU00062
      IF(NPRUS.GT.IIMAGE) GOTO 420                                      ZNU00063
 410  CONTINUE                                                          ZNU00064
      RETURN                                                            ZNU00065
C**** OUTPUT BUFFER OVERFLOW                                            ZNU00066
 420  NERROR=37                                                         ZNU00067
      CALL EXERR(NERROR,I1,I2,X1,X2)                                    ZNU00068
      IANF=-1                                                           ZNU00069
      RETURN                                                            ZNU00070
      END                                                               ZNU00071
      