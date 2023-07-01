C     compared with source                                              ***
      SUBROUTINE STRING(IST,ITO,IX)                                        00001
C---- DIE SUBROUTINE **STRING** MUSS NORMALERWEISE DEM PROGRAMM **MAIN**   00002
C---- FOLGEN UND NICHT WIE JETZT VORAUSGEHEN,WENN IN OVERLAY-TECHNIK       00003
C---- GEARBEITET WIRD.                                                     00004
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,         00005
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST,INEXT,    00006
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,     00007
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE,    00008
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI        00009
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,        00010
     1NIFMAX,INTZEI                                                        00011
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                           00012
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),         00013
     1IRET(20),XXX(4),NFILE(25,3)                                          00014
      COMMON// ISTLST(340),LISTST(340)                                     00015
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                                  00017
      DIMENSION IPROG(3700)                                                00018
      EQUIVALENCE (DATA(1),IPROG(1))                                       00019
      INTZEI=48                                                            00020
      IK=0                                                                 00021
      IF((ITO-IST).LE.4) GOTO 584                                          00022
      WRITE(IWC,583)                                                       00023
 583  FORMAT(20X,79HSTRING CONSTANT HAS MORE THAN FIVE CHARACTERS -- I O   00024
     1NLY TAKE THE LEFT MOST FIVE)                                         00025
      IK=8                                                                 00026
 584  IX=0                                                                 00027
      K=0                                                                  00028
      DO 582 I=1,5                                                         00029
      LOCN=IST-1+I                                                         00030
      IF(LOCN.GT.ITO) GOTO 587                                             00031
      CALL ZALPH(CARDP(LOCN),L)                                            00032
      IF(L.LE.INTZEI) GOTO 581                                             00033
      IK=9                                                                 00034
      WRITE(IWC,585) CARDP(LOCN)                                           00035
 585  FORMAT(20X,38HUNKNOWN CHARACTER IN STRING CONSTANT *,A1,25H* I REP   00036
     1LACE IT BY A BLANK)                                                  00037
 587  L=38                                                                 00038
      GOTO 588                                                          ***
581   IF(L.EQ.38) L=45                                                  ***
588   IX=IX+((L-1)*(INTZEI**K))                                         ***
      K=K+1                                                                00040
 582  CONTINUE                                                             00041
      IF(IK.NE.0) WRITE(IWC,586) IK                                        00042
 586  FORMAT(1H+,110X,18H***WARNING NUMBER=,I3/11X,28HSEE BASIC TEXTBOOK   00043
     1  A  322.2/)                                                         00044
      RETURN                                                               00045
      END                                                                  00046
