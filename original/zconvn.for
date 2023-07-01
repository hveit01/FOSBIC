      SUBROUTINE ZCONVN(IFR,ITO,FNUM)                                   ZCO00001
C*****SUBROUTINE TO CONVERT A POSITIVE NUMBER IN CARDP(IFR) THROUGH     ZCO00002
C*****CARDP(ITO TO A REAL NUMBER FNUM                                   ZCO00003
C                                                                       ZCO00004
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      ZCO00005
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST, INEXT,ZCO00006
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  ZCO00007
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, ZCO00008
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     ZCO00009
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     ZCO00010
     1NIFMAX,INTZEI                                                     ZCO00011
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        ZCO00012
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      ZCO00013
     1IRET(20),XXX(4),NFILE(25,3)                                       ZCO00014
      COMMON// ISTLST(340),LISTST(340)                                  ZCO00015
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               ZCO00017
      DIMENSION IPROG(3700)                                             ZCO00018
      EQUIVALENCE (DATA(1),IPROG(1))                                    ZCO00019
C                                                                       ZCO00020
      IF(ITO.LT.IFR) GOTO 190                                           ZCO00021
      FNUM=0.                                                           ZCO00022
      IDEC=0                                                            ZCO00023
      DO 120 I=IFR,ITO                                                  ZCO00024
      CALL ZDIGIT(CARDP(I),J)                                           ZCO00025
      IF(J.LE.10) GOTO 118                                              ZCO00026
      IF(CARDP(I).NE.DECMAL) GOTO 190                                   ZCO00027
      IDEC=I                                                            ZCO00028
      GOTO 120                                                          ZCO00029
 118  DIG=J-1                                                           ZCO00030
      FNUM=(FNUM*10.)+DIG                                               ZCO00031
 120  CONTINUE                                                          ZCO00032
      IF(IDEC.EQ.0) RETURN                                              ZCO00033
      IDIFF=ITO-IDEC                                                    ZCO00034
      FNUM=FNUM/(10.**IDIFF)                                            ZCO00035
      RETURN                                                            ZCO00036
C                                                                       ZCO00037
C*****ILLEGAL NUMBER                                                    ZCO00038
 190  FNUM=-1.                                                          ZCO00039
      RETURN                                                            ZCO00040
      END                                                               ZCO00041
