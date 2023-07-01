      SUBROUTINE CLEAR(IANF,IEND)                                       CLR00001
C**** SUBROUTINE TO FILL UP CARP WITH BLANKS                            CLR00002
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      CLR00003
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST,INEXT, CLR00004
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  CLR00005
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, CLR00006
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     CLR00007
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     CLR00008
     1NIFMAX,INTZEI                                                     CLR00009
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        CLR00010
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      CLR00011
     1IRET(20),XXX(4),NFILE(25,3)                                       CLR00012
      COMMON// ISTLST(340),LISTST(340)                                  CLR00013
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               CLR00015
      DIMENSION IPROG(3700)                                             CLR00016
      EQUIVALENCE (DATA(1),IPROG(1))                                    CLR00017
      DO 10 I=IANF,IEND                                                 CLR00018
      CARP(I)=BLANK                                                     CLR00019
 10   CONTINUE                                                          CLR00020
      RETURN                                                            CLR00021
      END                                                               CLR00022
