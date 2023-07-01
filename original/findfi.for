      SUBROUTINE FINDFI(IX,IA  )                                        FIN00001
C---- DIE SUBROUTINE **FINDFI** MUSS NORMALERWEISE DEM PROGRAMM **MAIN**FIN00002
C---- FOLGEN UND NICHT WIE JETZT VORAUSGEHEN,WENN IN OVERLAY-TECHNIK    FIN00003
C---- GEARBEITET WIRD.                                                  FIN00004
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      FIN00005
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST,INEXT, FIN00006
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  FIN00007
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, FIN00008
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     FIN00009
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     FIN00010
     1NIFMAX,INTZEI                                                     FIN00011
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        FIN00012
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      FIN00013
     1IRET(20),XXX(4),NFILE(25,3)                                       FIN00014
      COMMON// ISTLST(340),LISTST(340)                                  FIN00015
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               FIN00017
      DIMENSION IPROG(3700)                                             FIN00018
      EQUIVALENCE (DATA(1),IPROG(1))                                    FIN00019
      DO 10 K=1,NUMFIL                                                  FIN00020
      IF(NFILE(K,1).EQ.IX) GOTO 11                                      FIN00021
 10   CONTINUE                                                          FIN00022
      K=NUMFIL+1                                                        FIN00023
 11   IA=K                                                              FIN00024
      RETURN                                                            FIN00025
      END                                                               FIN00026
