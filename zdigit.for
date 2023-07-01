      SUBROUTINE ZDIGIT(ZEICH,IA)                                       ZDI00001
C---- DIE SUBROUTINE **STRING** MUSS NORMALERWEISE DEM PROGRAMM **MAIN**ZDI00002
C---- FOLGEN UND NICHT WIE JETZT VORAUSGEHEN,WENN IN OVERLAY-TECHNIK    ZDI00003
C---- GEARBEITET WIRD.                                                  ZDI00004
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      ZDI00005
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST,INEXT, ZDI00006
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  ZDI00007
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, ZDI00008
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     ZDI00009
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     ZDI00010
     1NIFMAX,INTZEI                                                     ZDI00011
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        ZDI00012
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      ZDI00013
     1IRET(20),XXX(4),NFILE(25,3)                                       ZDI00014
      COMMON// ISTLST(340),LISTST(340)                                  ZDI00015
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               ZDI00017
      DIMENSION IPROG(3700)                                             ZDI00018
      EQUIVALENCE (DATA(1),IPROG(1))                                    ZDI00019
      DO 10 I=1,10                                                      ZDI00020
      IF(ZEICH.EQ.DIGIT(I)) GOTO 20                                     ZDI00021
 10   CONTINUE                                                          ZDI00022
      I=11                                                              ZDI00023
 20   IA=I                                                              ZDI00024
      RETURN                                                            ZDI00025
      END                                                               ZDI00026
