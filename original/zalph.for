      SUBROUTINE ZALPH(ZEICH,IA)                                        ZAL00001
C---- DIE SUBROUTINE **ZALPH*** MUSS NORMALERWEISE DEM PROGRAMM **MAIN**ZAL00002
C---- FOLGEN UND NICHT WIE JETZT VORAUSGEHEN,WENN IN OVERLAY-TECHNIK    ZAL00003
C---- GEARBEITET WIRD.                                                  ZAL00004
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      ZAL00005
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST,INEXT, ZAL00006
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  ZAL00007
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, ZAL00008
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     ZAL00009
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     ZAL00010
     1NIFMAX,INTZEI                                                     ZAL00011
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        ZAL00012
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      ZAL00013
     1IRET(20),XXX(4),NFILE(25,3)                                       ZAL00014
      COMMON// ISTLST(340),LISTST(340)                                  ZAL00015
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               ZAL00017
      DIMENSION IPROG(3700)                                             ZAL00018
      EQUIVALENCE (DATA(1),IPROG(1))                                    ZAL00019
      DO 10 I=1,48                                                      ZAL00020
      IF(ZEICH.EQ.ALPH(I)) GOTO 20                                      ZAL00021
 10   CONTINUE                                                          ZAL00022
      I=49                                                              ZAL00023
 20   IA=I                                                              ZAL00024
      RETURN                                                            ZAL00025
      END                                                               ZAL00026
